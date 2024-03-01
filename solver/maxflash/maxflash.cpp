//
// Created by pro on 2022/1/13.
//

#include "istool/solver/maxflash/maxflash.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "glog/logging.h"
#include <iostream>

namespace {
    int KDefaultIterStep = 3;
}

const std::string solver::maxflash::KIterProbStepName = "MaxFlash@IterProbStep";

MaxFlash::MaxFlash(Specification *_spec, Verifier* _v, TopDownModel* model, const VSAEnvSetter& _setter):
    VerifiedSolver(_spec, _v), ext(ext::vsa::getExtension(_spec->env.get())), setter(_setter), env_id(-1) {
    io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "MaxFlash requires IOExamples";
    }
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "MaxFlash can only synthesize a single program";
    }
    // spec->info_list[0]->grammar->print();
    graph = new TopDownContextGraph(spec->info_list[0]->grammar, model, ProbModelType::NEG_LOG_PROB);
    auto* step_data = spec->env->getConstRef(solver::maxflash::KIterProbStepName);
    if (step_data->isNull()) {
        spec->env->setConst(solver::maxflash::KIterProbStepName, BuildData(Int, KDefaultIterStep));
    }
    KDefaultIterStep = theory::clia::getIntValue(*step_data);
}

void MaxFlash::prepareEnv(int example_id) {
    if (example_id == env_id) return;
    env_id = example_id; setter(spec->info_list[0]->grammar, spec->env.get(), counter_example_list[example_id]);
}

void MaxFlash::clear() {
    for (const auto& map: combined_node_map) {
        for (const auto& info: map) {
            delete info.second;
        }
    }
    for (const auto& map: single_node_map) {
        for (const auto& info: map) {
            delete info.second;
        }
    }
    counter_example_list.clear();
    combined_node_map.clear();
    single_node_map.clear();
}

MaxFlash::~MaxFlash() {
    clear();
}

namespace {
    std::string _getFeature(int id, const WitnessTerm& oup_list) {
        std::string res = std::to_string(id);
        for (const auto& oup: oup_list) res += "@" + oup->toString();
        return res;
    }

    void _collectOup(MaxFlashNode* node, WitnessTerm& result) {
        auto* single_node = dynamic_cast<SingleMaxFlashNode*>(node);
        if (single_node) {
            result.push_back(single_node->oup); return;
        }
        auto* multi_node = dynamic_cast<MultiMaxFlashNode*>(node);
        assert(multi_node);
        _collectOup(multi_node->l, result);
        _collectOup(multi_node->r, result);
    }
}

void MaxFlash::buildEdge(MaxFlashNode *node, int example_id) {
    if (node->is_build_edge) return;
    node->is_build_edge = true;
    if (example_id <= 0) {
        auto* single_node = dynamic_cast<SingleMaxFlashNode*>(node);
        auto& graph_node = graph->node_list[node->node_id];
        prepareEnv(-example_id);
        for (const auto& edge: graph_node.edge_list) {
            auto witness_list = ext->getWitness(edge.semantics.get(), single_node->oup, counter_example_list[-example_id].first);
            for (auto& witness_term: witness_list) {
                std::vector<MaxFlashNode*> sub_node_list;
                for (int i = 0; i < witness_term.size(); ++i) {
                    sub_node_list.push_back(initNode(edge.v_list[i], {witness_term[i]}, example_id));
                }
                bool is_self_circle = false;
                for (auto* sub_node: sub_node_list) if (node == sub_node) {
                    is_self_circle = true;
                }
                if (!is_self_circle) {
                    node->edge_list.emplace_back(sub_node_list, edge.semantics, edge.weight);
                }
            }
        }
    } else {
        auto* multi_node = dynamic_cast<MultiMaxFlashNode*>(node);
        MaxFlashNode *l = multi_node->l, *r = multi_node->r;
        if (!l->is_build_edge) buildEdge(l, example_id - 1);
        if (!r->is_build_edge) buildEdge(r, -example_id);
        std::unordered_map<std::string, std::pair<std::vector<int>, std::vector<int>>> edge_comb_info;
        for (int i = 0; i < l->edge_list.size(); ++i) {
            edge_comb_info[l->edge_list[i].semantics->getName()].first.push_back(i);
        }
        for (int i = 0; i < r->edge_list.size(); ++i) {
            edge_comb_info[r->edge_list[i].semantics->getName()].second.push_back(i);
        }
        for (const auto& info: edge_comb_info) {
            for (int l_id: info.second.first) {
                for (int r_id: info.second.second) {
                    std::vector<MaxFlashNode*> sub_node_list;
                    auto& l_edge = l->edge_list[l_id]; auto& r_edge = r->edge_list[r_id];
                    for (int i = 0; i < l_edge.node_list.size(); ++i) {
                        WitnessTerm oup_list;
                        _collectOup(l_edge.node_list[i], oup_list);
                        _collectOup(r_edge.node_list[i], oup_list);
                        sub_node_list.push_back(initNode(l_edge.node_list[i]->node_id, oup_list, example_id));
                    }
                    node->edge_list.emplace_back(sub_node_list, l_edge.semantics, l_edge.weight);
                }
            }
        }
    }
}

namespace {
    const double KDoubleEps = 1e-8;
}

bool MaxFlash::getBestProgram(MaxFlashNode *node, int example_id, double upper_bound) {
    if (node->best_program) return true;
    if (node->lower_bound > upper_bound) return false;
    auto* multi_node = dynamic_cast<MultiMaxFlashNode*>(node);
    if (multi_node) {
        if (!getBestProgram(multi_node->l, example_id - 1, upper_bound) || !getBestProgram(multi_node->r, -example_id, upper_bound)) {
            node->updateLowerBound();
            return false;
        }
        auto candidate = multi_node->l->best_program;
        if (multi_node->r->oup->isInclude(spec->env->run(candidate.get(), counter_example_list[example_id].first))) {
            node->best_program = candidate;
            node->lower_bound = multi_node->l->lower_bound;
            return true;
        }
    }
    if (!node->is_build_edge) {
        buildEdge(node, example_id);
    }
    if (node->updateLowerBound() > upper_bound) return false;
    std::vector<int> possible_edges;
    for (int i = 0; i < node->edge_list.size(); ++i) {
        auto& edge = node->edge_list[i];
        if (edge.lower_bound <= upper_bound) { // edge.lower_bound has been updated in node->updateLowerBound()
            if (edge.isFinished()) upper_bound = std::min(upper_bound, edge.lower_bound);
            else possible_edges.push_back(i);
        }
    }
    while (true) {
        if (node->best_program) return true;
        int best_edge_id = -1;
        double best_remain = 0.0;
        for (auto edge_id: possible_edges) {
            auto& edge = node->edge_list[edge_id];
            if (edge.lower_bound >= upper_bound || edge.isFinished()) continue;
            double remain = edge.getAverageRemain(upper_bound);
            if (remain > best_remain) {
                best_remain = remain; best_edge_id = edge_id;
            }
        }
        if (best_edge_id == -1) break;
        std::vector<double> remain_list;
        auto& best_edge = node->edge_list[best_edge_id];
        for (auto* sub_node: best_edge.node_list) {
            remain_list.push_back(sub_node->lower_bound + best_remain);
        }
        for (int i = 0; i < remain_list.size(); ++i) {
            auto* sub_node = best_edge.node_list[i];
            if (!sub_node->best_program && getBestProgram(sub_node, example_id, remain_list[i])) {
                break;
            }
        }
        int now = 0; node->lower_bound = upper_bound;
        for (int edge_id: possible_edges) {
            auto& edge = node->edge_list[edge_id];
            if (edge.updateLowerBound() > upper_bound) continue;
            if (edge.isFinished()) {
                upper_bound = std::min(upper_bound, edge.lower_bound);
            } else {
                possible_edges[now++] = edge_id;
            }
            node->lower_bound = std::min(node->lower_bound, edge.lower_bound);
        }
        if (multi_node) {
            node->lower_bound = std::max(node->lower_bound, std::max(multi_node->l->lower_bound, multi_node->r->lower_bound));
        }
        possible_edges.resize(now);
    }
    node->updateLowerBound();
    int edge_id = -1;
    for (int i = 0; i < node->edge_list.size(); ++i) {
        if (node->edge_list[i].isFinished() && std::abs(node->edge_list[i].lower_bound - upper_bound) < KDoubleEps) {
            edge_id = i; break;
        }
    }
    if (edge_id == -1) return false;
    node->best_program = node->edge_list[edge_id].getBestProgram();
    return true;
}

MaxFlashNode * MaxFlash::initNode(int id, const WitnessTerm &oup_list, int example_id) {
    std::string feature = _getFeature(id, oup_list);
    auto& cache = example_id > 0 ? combined_node_map[example_id] : single_node_map[-example_id];
    auto& result = cache[feature];
    if (result) return result;
    auto& graph_node = graph->node_list[id];

    if (example_id <= 0) {
        result = new SingleMaxFlashNode(id, graph_node.lower_bound, oup_list[0]);
        return result;
    } else {
        auto* r = dynamic_cast<SingleMaxFlashNode*>(initNode(id, {oup_list[oup_list.size() - 1]}, -example_id));
        auto l_oup_list = oup_list; l_oup_list.pop_back();
        auto* l = initNode(id, l_oup_list, example_id - 1);
        return result = new MultiMaxFlashNode(id, l, r);
    }
}

void MaxFlash::insertExample(const Example &example) {
    auto io_example = io_space->getIOExample(example);
    LOG(INFO) << "Insert example " << example::ioExample2String(io_example);
    counter_example_list.push_back(io_example);
    combined_node_map.emplace_back();
    single_node_map.emplace_back();
}

FunctionContext MaxFlash::synthesis(TimeGuard *guard) {
    clear();
    auto initial_program = grammar::getMinimalProgram(spec->info_list[0]->grammar);
    Example counter_example; std::string func_name = spec->info_list[0]->name;
    auto res_info = semantics::buildSingleContext(func_name, initial_program);
    if (v->verify(res_info, &counter_example)) return res_info;
    insertExample(counter_example);

    double upper_bound = 0;
    while (1) {
        WitnessTerm oup_list; int example_num = counter_example_list.size();
        for (const auto& example: counter_example_list) {
            oup_list.push_back(std::make_shared<DirectWitnessValue>(example.second));
        }
        auto* root = initNode(0, oup_list, example_num - 1);
        while (!getBestProgram(root, example_num - 1, upper_bound)) {
            upper_bound += KDefaultIterStep;
            LOG(INFO) << "relax upper bound to " << upper_bound << std::endl;
        }
        auto current_program = root->best_program;
        res_info = semantics::buildSingleContext(func_name, current_program);
        if (v->verify(res_info, &counter_example)) return res_info;
        auto io_example = io_space->getIOExample(counter_example);
        std::cout << current_program->toString() << std::endl;
        insertExample(counter_example);
    }
}