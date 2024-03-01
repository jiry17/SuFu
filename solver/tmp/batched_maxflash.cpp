//
// Created by pro on 2023/1/3.
//

#include "istool/solver/tmp/batched_maxflash.h"
#include "glog/logging.h"

using namespace tmp;

namespace {
    double KINF = 1e100;
    double KEPS = 1e-5;
    int _sign(double k) {
        if (k < -KEPS) return -1;
        if (k > KEPS) return 1;
        return 0;
    }
}

ComposeState::ComposeState(const std::vector<int> &_id_list, double _prob, int _pos):
    id_list(_id_list), prob(_prob), pos(_pos) {
}
BatchedMaxFlashEdge::BatchedMaxFlashEdge(const std::vector<BatchedMaxFlashNode *> &_node_list, const PSemantics &_semantics, double _weight):
    node_list(_node_list), semantics(_semantics), weight(_weight) {
    std::vector<int> id_list;
    for (int i = 0; i < node_list.size(); ++i) id_list.push_back(0);
    auto* state = new ComposeState(id_list, 0, 0);
    updateComposeState(state);
    Q.push(state);
}
bool BatchedMaxFlashEdge::updateComposeState(ComposeState *state) {
    double tmp = weight;
    for (int i = 0; i < node_list.size(); ++i) {
        tmp += node_list[i]->getLowerBound(state->id_list[i]);
    }
    if (_sign(tmp - state->prob) == 0) return false;
    state->prob = tmp; return true;
}
void BatchedMaxFlashEdge::updateTopState() {
    while (updateComposeState(Q.top())) {
        auto* top = Q.top();
        Q.pop(); Q.push(top);
    }
}
double BatchedMaxFlashEdge::updateLowerBound() {
    if (Q.empty()) {
        lower_bound = KINF;
    } else {
        updateTopState();
        lower_bound = Q.top()->prob;
    }
    return lower_bound;
}
void BatchedMaxFlashEdge::extendTopState() {
    auto* top = Q.top(); Q.pop();
    for (int i = top->pos; i < top->id_list.size(); ++i) {
        std::vector<int> id_list = top->id_list;
        id_list[i]++;
        auto* state = new ComposeState(id_list, 0, i);
        updateComposeState(state);
        Q.push(state);
    }
    delete top;
    updateLowerBound();
}
ResultUnit BatchedMaxFlashEdge::constructProgram() {
    auto* top = Q.top();
    ProgramList sub_list;
    double w = weight;
    for (int i = 0; i < node_list.size(); ++i) {
        int sub_rank = top->id_list[i]; auto* sub_node = node_list[i];
        assert(sub_node->result.size() > sub_rank);
        sub_list.push_back(sub_node->result[sub_rank].first);
        w += sub_node->result[sub_rank].second;
    }
    assert(_sign(w - top->prob) == 0);
    auto res = std::make_shared<Program>(semantics, sub_list);
    extendTopState();
    return {res, w};
}
BatchedMaxFlashNode::BatchedMaxFlashNode(int _node_id, double _lower_bound, const std::string &_oup_string):
    node_id(_node_id), lower_bound(_lower_bound), oup_string(_oup_string), is_build_edge(false) {
}
std::string BatchedMaxFlashNode::toString() const {
    return std::to_string(node_id) + "@" + oup_string;
}
double BatchedMaxFlashNode::getLowerBound(int rank) {
    assert(rank <= result.size());
    if (rank == result.size()) return lower_bound; else return result[rank].second;
}
BatchedSingleMaxFlashNode::BatchedSingleMaxFlashNode(int _node_id, double _lower_bound, const WitnessData &_oup):
    BatchedMaxFlashNode(_node_id, _lower_bound, _oup->toString()), oup(_oup) {
}
void BatchedSingleMaxFlashNode::updateLowerBound() {
    if (!result.empty()) {
        lower_bound = std::max(lower_bound, result[int(result.size()) - 1].second);
    }
    if (!is_build_edge) return;
    auto tmp = KINF;
    for (auto& edge: edge_list) {
        tmp = std::min(tmp, edge.updateLowerBound());
    }
    // LOG(INFO) << "update lower bound " << toString() << edge_list.size() << " " << tmp;
    lower_bound = std::max(lower_bound, tmp);
}
void BatchedSingleMaxFlashNode::insert(const ResultUnit &unit) {
    /*LOG(INFO) << "insert " << result.size() << " " << toString() << " " << unit.first->toString() << " " << unit.second << " " << lower_bound;
    for (auto& [prog, w]: result) {
        std::cout << "  " << prog->toString() << " " << w <<std::endl;
    }*/
    assert(_sign(unit.second - lower_bound) >= 0);
    result.push_back(unit); lower_bound = unit.second;
}
BatchedMultiMaxFlashNode::BatchedMultiMaxFlashNode(int _node_id, BatchedMaxFlashNode *_l, BatchedSingleMaxFlashNode *_r, ExecuteInfo* _info):
    BatchedMaxFlashNode(_node_id, 0, _l->oup_string + "@" + _r->oup_string), info(_info),
    l(_l), r(_r), l_pos(0) {
    getValidResultsFromR();
}
void BatchedMultiMaxFlashNode::insert(const ResultUnit &unit) {
    if (!result.empty()) {
        int pos = int(result.size()) - 1;
        auto sign = _sign(unit.second - result[pos].second);
        if (sign < 0) return;
        for (; pos >= 0 && _sign(unit.second - result[pos].second) == 0; --pos) {
            if (result[pos].first->toString() == unit.first->toString()) return;
        }
    }
    result.push_back(unit); lower_bound = unit.second;
}
void BatchedMultiMaxFlashNode::getValidResultsFromR() {
    for (; l_pos < l->result.size(); ++l_pos) {
        auto current = l->result[l_pos].first;
        if (!r->oup->isInclude(current->run(info))) continue;
        insert(l->result[l_pos]);
    }
    lower_bound = std::max(l->lower_bound, r->lower_bound);
}
void BatchedMultiMaxFlashNode::updateLowerBound() {
    getValidResultsFromR();
    if (!result.empty()) lower_bound = std::max(lower_bound, result[int(result.size()) - 1].second);
    if (!is_build_edge) return;
    auto tmp = KINF;
    for (auto& edge: edge_list) {
        tmp = std::min(tmp, edge.updateLowerBound());
    }
    lower_bound = std::max(lower_bound, tmp);
}

BatchedMaxFlash::BatchedMaxFlash(Specification *spec, Verifier *v, TopDownModel* model, const VSAEnvSetter& _setter):
    VerifiedSolver(spec, v), env_id(-1), setter(_setter) {
    assert(spec->info_list.size() == 1);
    grammar = spec->info_list[0]->grammar;
    example_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    target_name = spec->info_list[0]->name;
    ext = ext::vsa::getExtension(spec->env.get());
    assert(example_space);
    graph = new TopDownContextGraph(spec->info_list[0]->grammar, model, ProbModelType::NEG_LOG_PROB);
}
PProgram BatchedMaxFlash::synthesisWithExamples(int rank) {
    WitnessTerm oup_list;
    for (auto& [_, oup]: example_list) oup_list.push_back(std::make_shared<DirectWitnessValue>(oup));
    auto* root = initNode(0, oup_list, int(example_list.size() - 1));
    while (1) {
        // LOG(INFO) << "update lower bound " << limit << " " << root->getLowerBound(rank);
        if (search(root, int(example_list.size()) - 1, limit, rank)) {
            return root->result[rank].first;
        }
        limit += 3;
    }
}

namespace {
    std::string _getFeature(int state_id, const WitnessTerm& oup_list) {
        auto res = std::to_string(state_id);
        for (auto& oup: oup_list) res += "@" + oup->toString();
        return res;
    }
}

BatchedMaxFlashNode * BatchedMaxFlash::initNode(int id, const WitnessTerm &oup_list, int example_id) {
    std::string feature = _getFeature(id, oup_list);
    auto& graph_node = graph->node_list[id];

    if (example_id <= 0) {
        auto& res = single_node_map[-example_id][feature];
        if (res) return res;
        return res = new BatchedSingleMaxFlashNode(id, graph_node.lower_bound, oup_list[0]);
    } else {
        auto& res = multi_node_map[example_id][feature];
        if (res) return res;
        auto* r = dynamic_cast<BatchedSingleMaxFlashNode*>(initNode(id, {oup_list[oup_list.size() - 1]}, -example_id));
        auto l_oup_list = oup_list; l_oup_list.pop_back();
        auto* l = initNode(id, l_oup_list, example_id - 1);
        return res = new BatchedMultiMaxFlashNode(id, l, r, info_list[example_id]);
    }
}
ProgramList BatchedMaxFlash::batchedSynthesis(int target_num, TimeGuard *guard) {
    auto init_program = grammar::getMinimalProgram(spec->info_list[0]->grammar);
    Example counter_example;
    assert(!v->verify(semantics::buildSingleContext(target_name, init_program), &counter_example));
    addExample(example_space->getIOExample(counter_example));

    ProgramList result; limit = 5;
    for (int i = 0; i < target_num; ++i) {
        while (1) {
            auto program = synthesisWithExamples(i);
            LOG(INFO) << "candidate " << i << " " << program->toString();
            if (v->verify(semantics::buildSingleContext(target_name, program), &counter_example)) {
                LOG(INFO) << "find " << i << " " << program->toString();
                result.push_back(program);
                break;
            }
            addExample(example_space->getIOExample(counter_example));
        }
    }
    return result;
}


void BatchedMaxFlash::addExample(const IOExample& example) {
    LOG(INFO) << "add example " << example::ioExample2String(example);
    example_list.push_back(example);
    auto* info = new ExecuteInfo(example.first, {});
    info_list.push_back(info);
    multi_node_map.emplace_back();
    single_node_map.emplace_back();
}
FunctionContext BatchedMaxFlash::synthesis(TimeGuard *guard) {
    auto res = batchedSynthesis(1, guard);
    assert(!res.empty());
    return semantics::buildSingleContext(target_name, res[0]);
}
void BatchedMaxFlash::prepareEnv(int example_id) {
    if (example_id == env_id) return;
    env_id = example_id;
    setter(grammar, spec->env.get(), example_list[example_id]);
}

namespace {
    void _collectOutput(BatchedMaxFlashNode* node, WitnessTerm& term) {
        auto* sn = dynamic_cast<BatchedSingleMaxFlashNode*>(node);
        if (sn) {
            term.push_back(sn->oup); return;
        }
        auto* mn = dynamic_cast<BatchedMultiMaxFlashNode*>(node);
        assert(mn);
        _collectOutput(mn->l, term); _collectOutput(mn->r, term);
    }
}

void BatchedMaxFlash::buildEdge(BatchedMaxFlashNode *node, int example_id) {
    if (node->is_build_edge) return;
    node->is_build_edge = true;
    if (example_id <= 0) {
        auto* single_node = dynamic_cast<BatchedSingleMaxFlashNode*>(node);
        auto& graph_node = graph->node_list[node->node_id];
        prepareEnv(-example_id);
        for (const auto& edge: graph_node.edge_list) {
            auto witness_list = ext->getWitness(edge.semantics.get(), single_node->oup, example_list[-example_id].first);
            for (auto& witness_term: witness_list) {
                std::vector<BatchedMaxFlashNode*> sub_node_list;
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
        auto* multi_node = dynamic_cast<BatchedMultiMaxFlashNode*>(node);
        BatchedMaxFlashNode *l = multi_node->l, *r = multi_node->r;
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
                    std::vector<BatchedMaxFlashNode*> sub_node_list;
                    auto& l_edge = l->edge_list[l_id]; auto& r_edge = r->edge_list[r_id];
                    for (int i = 0; i < l_edge.node_list.size(); ++i) {
                        WitnessTerm oup_list;
                        _collectOutput(l_edge.node_list[i], oup_list);
                        _collectOutput(r_edge.node_list[i], oup_list);
                        sub_node_list.push_back(initNode(l_edge.node_list[i]->node_id, oup_list, example_id));
                    }
                    node->edge_list.emplace_back(sub_node_list, l_edge.semantics, l_edge.weight);
                }
            }
        }
    }
}

bool BatchedMaxFlash::searchUpTo(BatchedMaxFlashNode *node, int example_id, double upper_bound, int rank) {
    for (int i = node->result.size(); i <= rank; ++i) {
        if (!search(node, example_id, upper_bound, i)) return false;
    }
    return true;
}

bool BatchedMaxFlash::search(BatchedMaxFlashNode *node, int example_id, double upper_bound, int rank) {
    if (node->result.size() > rank) return true;
    assert(node->result.size() == rank);
    if (_sign(node->lower_bound - upper_bound) >= 0) return false;
    auto* multi_node = dynamic_cast<BatchedMultiMaxFlashNode*>(node);
    if (multi_node) {
        if (!search(multi_node->l, example_id - 1, upper_bound, rank) || !searchUpTo(multi_node->r, -example_id, upper_bound, rank)) {
            node->updateLowerBound(); return false;
        }
        node->updateLowerBound();
        if (node->result.size() > rank) return true;
    }
    if (!node->is_build_edge) {
        buildEdge(node, example_id);
    }
    node->updateLowerBound();
    if (_sign(node->lower_bound - upper_bound) >= 0) return false;
    // LOG(INFO) << "current node " << node->toString() << " " << node->lower_bound;
    std::priority_queue<std::pair<double, int>> Q;
    for (int i = 0; i < node->edge_list.size(); ++i) {
        auto& edge = node->edge_list[i];
        if (_sign(edge.lower_bound - upper_bound) == -1) {
            // LOG(INFO) << "consider edge " << edge.semantics->getName() << " " << edge.lower_bound;
            Q.push({-edge.lower_bound, i});
        }
    }
    while (node->result.size() <= rank) {
        if (Q.empty()) return false;
        auto [best_w, best_id] = Q.top(); Q.pop();
        best_w = -best_w;
        auto& edge = node->edge_list[best_id];
        if (_sign(best_w - edge.updateLowerBound()) != 0) {
            if (_sign(edge.lower_bound - upper_bound) == -1) Q.push({-edge.lower_bound, best_id});
            continue;
        }
        std::vector<std::pair<int, double>> remain_list;
        auto* state = edge.Q.top();
        // LOG(INFO) << edge.semantics->getName() << " " << state->id_list.size() << " " << edge.node_list.size() << std::endl;
        for (int i = 0; i < state->id_list.size(); ++i) {
            auto* sub_node = edge.node_list[i];
            // LOG(INFO) << "node " << sub_node << " " << sub_node->result.size() << std::endl;
            if (sub_node->result.size() <= state->id_list[i]) {
                assert(sub_node->result.size() == state->id_list[i]);
                remain_list.emplace_back(i, sub_node->lower_bound);
            }
        }
        if (remain_list.empty()) {
            node->insert(edge.constructProgram());
            node->updateLowerBound();
            continue;
        }
        auto single_remain = (upper_bound - edge.lower_bound) / remain_list.size();
        for (int i = 0; i < remain_list.size(); ++i) remain_list[i].second += single_remain;
        for (auto [pos, sub_lower_bound]: remain_list) {
            auto* sub_node = edge.node_list[pos];
            auto sub_index = state->id_list[pos];
            if (search(sub_node, example_id, sub_lower_bound, sub_index)) break;
        }
        edge.updateLowerBound();
        if (_sign(edge.lower_bound - upper_bound) == -1) Q.push({-edge.lower_bound, best_id});
    }
    return node->result.size() > rank;
}