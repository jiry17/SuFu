//
// Created by pro on 2022/1/13.
//

#include "istool/solver/maxflash/topdown_context_graph.h"
#include "glog/logging.h"
#include <queue>
#include <iostream>

TopDownContextGraph::Node::Node(NonTerminal *_symbol, const PTopDownContext& _context): symbol(_symbol), context(_context) {
}
std::string TopDownContextGraph::Node::toString() const {
    return symbol->name + "@" + context->toString();
}
TopDownContextGraph::Edge::Edge(int _u, const std::vector<int> &_v_list, double _weight, const PSemantics &_semantics):
    u(_u), v_list(_v_list), weight(_weight), semantics(_semantics) {
}

namespace {
    double KDoubleINF = 1e100;
    double KDoubleEps = 1e-6;
}

// Note: possibly make the lower-bound on nodes incorrect
// TODO: Reorganize this class such that the usage of probability and log-probability is explicit.
void TopDownContextGraph::normalizeProbability() {
    for (auto& node: node_list) {
        std::vector<double> prob_list;
        double total_prob = 0.0;
        for (auto& edge: node.edge_list) {
            auto prob = ext::vsa::getTrueProb(edge.weight, prob_type);
            prob_list.push_back(prob);
            total_prob += prob;
        }
        assert(total_prob > KDoubleEps);
        for (int i = 0; i < prob_list.size(); ++i) {
            node.edge_list[i].weight = ext::vsa::getRepresentedProb(prob_list[i] / total_prob, prob_type);
        }
    }
    initNodeLowerBound();
}

TopDownContextGraph::TopDownContextGraph(Grammar *g, TopDownModel *model, ProbModelType _type): prob_type(_type) {
    std::unordered_map<std::string, int> node_map;
    auto initialize_node = [&](NonTerminal* symbol, const PTopDownContext& ctx) -> int {
        Node node(symbol, ctx);
        auto feature = node.toString();
        if (node_map.find(feature) != node_map.end()) return node_map[feature];
        int res = int(node_list.size());
        node_map[feature] = res;
        node_list.push_back(node);
        return res;
    };

    initialize_node(g->start, model->start);
    for (int id = 0; id < node_list.size(); ++id) {
        auto* symbol = node_list[id].symbol;
        auto* ctx = node_list[id].context.get();
        std::vector<Semantics*> sem_list;
        for (const auto& rule: symbol->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) {
                LOG(FATAL) << "Current implementation of VSA requires ConcreteRule but get " << rule->toString();
            }
            sem_list.push_back(cr->semantics.get());
        }
        auto prob_list = model->getWeightList(ctx, sem_list, prob_type);
        for (int rule_id = 0; rule_id < symbol->rule_list.size(); ++rule_id) {
            double weight = prob_list[rule_id]; std::vector<int> v_list;
            auto* cr = dynamic_cast<ConcreteRule*>(symbol->rule_list[rule_id]);
            for (int i = 0; i < cr->param_list.size(); ++i) {
                auto next_context = model->move(ctx, cr->semantics.get(), i);
                v_list.push_back(initialize_node(cr->param_list[i], next_context));
            }
            node_list[id].edge_list.emplace_back(id, v_list, weight, cr->semantics);
        }
    }

    normalizeProbability();
    initNodeLowerBound();
}

void TopDownContextGraph::initNodeLowerBound() {
    for (int i = 0; i < node_list.size(); ++i) node_list[i].lower_bound = KDoubleINF;
    std::vector<std::vector<std::pair<int, int>>> reversed_edge_list(node_list.size());
    for (int u = 0; u < node_list.size(); ++u) {
        for (int e_id = 0; e_id < node_list[u].edge_list.size(); ++e_id) {
            for (const auto& id: node_list[u].edge_list[e_id].v_list) {
                reversed_edge_list[id].emplace_back(u, e_id);
            }
        }
    }
    std::priority_queue<std::pair<double, int>, std::vector<std::pair<double, int>>, std::greater<>> Q;
    auto update = [&](int id, double new_lower_bound) -> void {
        if (node_list[id].lower_bound <= new_lower_bound + KDoubleEps) return;
        node_list[id].lower_bound = new_lower_bound;
        Q.push({new_lower_bound, id});
    };
    for (int i = 0; i < node_list.size(); ++i) {
        double lower_bound = KDoubleINF;
        for (auto& edge: node_list[i].edge_list) {
            if (edge.v_list.empty()) lower_bound = std::min(lower_bound, ext::vsa::changeProbModel(edge.weight, prob_type, ProbModelType::NEG_LOG_PROB));
        }
        if (lower_bound < KDoubleINF) update(i, lower_bound);
    }
    auto update_edge = [&](int u, int e_id) -> void {
        auto& edge = node_list[u].edge_list[e_id];
        double sum = ext::vsa::changeProbModel(edge.weight, prob_type, ProbModelType::NEG_LOG_PROB);
        for (auto id: edge.v_list) {
            sum += node_list[id].lower_bound;
        }
        update(u, sum);
    };

    while (!Q.empty()) {
        int id = Q.top().second; double w = Q.top().first; Q.pop();
        if (std::abs(node_list[id].lower_bound - w) > KDoubleEps) continue;
        for (const auto& edge: reversed_edge_list[id]) {
            update_edge(edge.first, edge.second);
        }
    }
    for (auto& node: node_list) {
        node.lower_bound = ext::vsa::changeProbModel(node.lower_bound, ProbModelType::NEG_LOG_PROB, prob_type);
    }
}

void TopDownContextGraph::print() const {
    for (int i = 0; i < node_list.size(); ++i) {
        std::cout << "node #" << i << " " << node_list[i].toString() << " >= " << node_list[i].lower_bound << std::endl;
        for (const auto& edge: node_list[i].edge_list) {
            std::cout << "  " << edge.weight << ": " << edge.semantics->getName() << "(";
            for (int j = 0; j < edge.v_list.size(); ++j) {
                if (j) std::cout << ",";
                std::cout << edge.v_list[j];
            }
            std::cout << ")" << std::endl;
        }
    }
}

TopDownGraphMatchStructure::TopDownGraphMatchStructure(int _edge_id, const PProgram& _program, const std::vector<TopDownGraphMatchStructure *> &_sub_list):
    edge_id(_edge_id), sub_list(_sub_list), program(_program) {
}
TopDownGraphMatchStructure::~TopDownGraphMatchStructure() {
    for (auto* sub: sub_list) delete sub;
}

namespace {
    // TODO: add a memoization table
    TopDownGraphMatchStructure* _getProgramMatchStructure(int node_id, TopDownContextGraph* graph, const PProgram& p) {
        auto& node = graph->node_list[node_id];
        for (int i = 0; i < node.edge_list.size(); ++i) {
            if (node.edge_list[i].semantics->getName() != p->semantics->getName()) continue;
            std::vector<TopDownGraphMatchStructure*> sub_list;
            bool is_success = true;
            for (int j = 0; j < p->sub_list.size(); ++j) {
                auto* sub_res = _getProgramMatchStructure(node.edge_list[i].v_list[j], graph, p->sub_list[j]);
                if (!sub_res) {
                    is_success = false; break;
                }
                sub_list.push_back(sub_res);
            }
            if (!is_success) {
                for (auto* sub: sub_list) delete sub;
            }
            return new TopDownGraphMatchStructure(i, p, sub_list);
        }
        return nullptr;
    }
}

TopDownGraphMatchStructure * TopDownContextGraph::getProgramMatchStructure(const PProgram& p) {
    return _getProgramMatchStructure(0, this, p);
}