//
// Created by pro on 2022/5/2.
//

#include "istool/selector/random/grammar_flatter.h"
#include "istool/ext/z3/z3_extension.h"
#include "istool/ext/z3/z3_example_space.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include <unordered_set>
#include "glog/logging.h"
#include <iostream>

FlattenGrammar::ParamInfo::ParamInfo(const PType &_type, const PProgram &_program): type(_type), program(_program) {
}

FlattenGrammar::FlattenGrammar(TopDownContextGraph *_graph, Env* _env): graph(_graph), env(_env) {
}
FlattenGrammar::~FlattenGrammar() {
    delete graph;
    for (auto& info: match_cache) delete info.second;
}
void FlattenGrammar::print() const {
    graph->print();
    for (int i = 0; i < param_info_list.size(); ++i) {
        std::cout << "  #" << i << ": " << param_info_list[i].program->toString() << std::endl;
    }
}

FlattenGrammarBuilder::FlattenGrammarBuilder(Grammar *_grammar, TopDownModel *_model):
    grammar(_grammar), model(_model) {
}

TopDownGraphMatchStructure * FlattenGrammar::getMatchStructure(const PProgram &program) {
    auto feature = program->toString();
    if (match_cache.count(feature)) return match_cache[feature];
    auto res = getMatchStructure(0, program);
    if (!res) {
        LOG(FATAL) << "Cannot match program " << program->toString();
    }
    return res;
}
Example FlattenGrammar::getFlattenInput(const Example &input) const {
    Example new_input;
    for (auto& param: param_info_list) new_input.push_back(env->run(param.program.get(), input));
    return new_input;
}
typedef std::function<bool(Program*)> ProgramValidator;

namespace {
    typedef std::pair<PProgram, double> ProgramInfo;
    bool _isTerminateSemantics(Semantics* sem) {
        return dynamic_cast<ParamSemantics*>(sem) || dynamic_cast<ConstSemantics*>(sem);
    }
    int _getTerminateSize(Semantics* sem, const std::vector<int>& param_size_list) {
        auto* ps = dynamic_cast<ParamSemantics*>(sem);
        if (ps) return param_size_list[ps->id];
        assert(dynamic_cast<ConstSemantics*>(sem));
        return 1;
    }
    void _collectAllProgram(int pos, double prob, ProgramList& tmp, const std::vector<std::vector<ProgramInfo>>& storage, std::vector<ProgramInfo>& res, const PSemantics& sem) {
        if (pos == storage.size()) {
            res.emplace_back(std::make_shared<Program>(sem, tmp), prob); return;
        }
        for (auto& info: storage[pos]) {
            tmp[pos] = info.first;
            _collectAllProgram(pos + 1, prob * info.second, tmp, storage, res, sem);
        }
    }
    std::vector<ProgramInfo> _collectAllProgram(const std::vector<std::vector<ProgramInfo>>& storage, const TopDownContextGraph::Edge& edge) {
        ProgramList tmp(storage.size());
        std::vector<ProgramInfo> res;
        _collectAllProgram(0, edge.weight, tmp, storage, res, edge.semantics);
        return res;
    }

    void _deleteEmptyNode(TopDownContextGraph* graph) {
        while (1) {
            int node_id = -1;
            for (int i = 0; i < graph->node_list.size(); ++i) {
                if (graph->node_list[i].edge_list.empty()) {
                    node_id = i; break;
                }
            }
            if (node_id == -1) return;
            for (int i = node_id; i + 1 < graph->node_list.size(); ++i) {
                graph->node_list[i] = graph->node_list[i + 1];
            }
            graph->node_list.pop_back();
            for (auto& node: graph->node_list) {
                int now = 0;
                for (auto& edge: node.edge_list) {
                    bool is_removed = false;
                    for (auto& v: edge.v_list) {
                        if (v == node_id) {
                            is_removed = true; break;
                        } else if (v > node_id) --v;
                    }
                    if (!is_removed) node.edge_list[now++] = edge;
                }
                while (node.edge_list.size() > now) node.edge_list.pop_back();
            }
        }
    }

    const int KSizeLimit = 1e9;

    struct _MergePosInfo {
        int node_id, edge_id, fix_pos, fix_res_id;
        _MergePosInfo(int _node_id, int _edge_id, int _pos, int _fix_res_id):
            node_id(_node_id), edge_id(_edge_id), fix_pos(_pos), fix_res_id(_fix_res_id) {
        }
    };

    void _separateUsage(TopDownContextGraph* graph, const _MergePosInfo& info) {
        bool is_separate = true;
        for (int i = 0; i < graph->node_list.size() && is_separate; ++i) {
            auto& node = graph->node_list[i];
            for (int j = 0; j < node.edge_list.size() && is_separate; ++j) {
                if (i == info.node_id && j == info.edge_id) continue;
                for (auto v: node.edge_list[j].v_list) {
                    if (v == info.fix_pos) is_separate = false;
                }
            }
        }
        if (is_separate) return;
        auto new_node = graph->node_list[info.fix_pos];
        int new_id = graph->node_list.size();
        graph->node_list.push_back(new_node);
        for (int i = 0; i < graph->node_list.size(); ++i) {
            auto& node = graph->node_list[i];
            for (int j = 0; j < node.edge_list.size(); ++j) {
                if (i == info.node_id && j == info.edge_id) continue;
                for (auto& v: node.edge_list[j].v_list) {
                    if (v == info.fix_pos) v = new_id;
                }
            }
        }
    }

    _MergePosInfo _getBestMergePos(TopDownContextGraph* g, const std::vector<int>& param_size_list, Env* env) {
        std::vector<bool> is_final(g->node_list.size(), false);
        std::vector<double> average_size_list(g->node_list.size(), 0.0);
        std::vector<int> min_size(g->node_list.size(), 1e9);
        for (int node_id = 0; node_id < g->node_list.size(); ++node_id) {
            auto& node = g->node_list[node_id];
            bool flag = true; min_size[node_id] = 1e9;
            for (auto& edge: node.edge_list) {
                if (!_isTerminateSemantics(edge.semantics.get())) {
                    flag = false; break;
                } else {
                    auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
                    int current_size = ps ? param_size_list[ps->id] : 1;
                    min_size[node_id] = std::min(min_size[node_id], current_size);
                    average_size_list[node_id] += current_size * edge.weight;
                }
            }
            if (flag) is_final[node_id] = true;
        }
        _MergePosInfo best_pos(-1, -1, -1, -1);
        double best_size = KSizeLimit;
        std::vector<int> node_id_list(g->node_list.size());
        for (int i = 0; i < g->node_list.size(); ++i) node_id_list[i] = i;
        std::shuffle(node_id_list.begin(), node_id_list.end(), env->random_engine);
        for (auto node_id: node_id_list) {
            if (is_final[node_id]) continue;
            auto& node = g->node_list[node_id];
            std::vector<int> edge_id_list(node.edge_list.size());
            for (int i = 0; i < edge_id_list.size(); ++i) edge_id_list[i] = i;
            std::shuffle(edge_id_list.begin(), edge_id_list.end(), env->random_engine);
            for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                auto& edge = node.edge_list[edge_id];
                if (_isTerminateSemantics(edge.semantics.get())) continue;
                double average_size = 1.0; bool flag = true;
                for (auto sub_id: edge.v_list) {
                    if (!is_final[sub_id]) {
                        flag = false; break;
                    }
                    average_size += average_size_list[sub_id];
                }
                if (flag) {
                    for (auto sub_id: edge.v_list) {
                        double current = average_size - average_size_list[sub_id] + min_size[sub_id];
                        if (current < best_size) {
                            best_size = average_size;
                            best_pos = _MergePosInfo(node_id, edge_id, sub_id, -1);
                        }
                    }
                }
            }
        }
        if (best_pos.node_id == -1) return best_pos;
        std::vector<int> candidate_res_list;
        auto& node = g->node_list[best_pos.fix_pos];
        for (int i = 0; i < node.edge_list.size(); ++i) {
            auto* sem = node.edge_list[i].semantics.get();
            assert(_isTerminateSemantics(sem));
            auto* ps = dynamic_cast<ParamSemantics*>(sem);
            int current_size = ps ? param_size_list[ps->id] : 1;
            if (current_size == min_size[best_pos.fix_pos]) {
                candidate_res_list.push_back(i);
            }
        }
        assert(!candidate_res_list.empty());
        std::uniform_int_distribution<int> d(0, candidate_res_list.size() - 1);
        int choice = d(env->random_engine);
        best_pos.fix_res_id = candidate_res_list[choice];
        return best_pos;
    }

    const int KExtraFactor = 20;
}


TopDownGraphMatchStructure * TrivialFlattenGrammar::getMatchStructure(int node_id, const PProgram& p) const {
    auto feature = p->toString();
    auto it = param_map.find(feature);
    auto& node = graph->node_list[node_id];
    if (it != param_map.end()) {
        int param_id = it->second;
        for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
            auto *ps = dynamic_cast<ParamSemantics*>(node.edge_list[edge_id].semantics.get());
            if (ps && ps->id == param_id) return new TopDownGraphMatchStructure(edge_id, program::buildParam(param_id, param_info_list[param_id].type), {});
        }
    }
    auto sem_name = p->semantics->getName();
    for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
        auto& edge = node.edge_list[edge_id];
        if (edge.semantics->getName() == sem_name) {
            std::vector<TopDownGraphMatchStructure*> sub_list;
            bool flag = true;
            for (int i = 0; i < p->sub_list.size(); ++i) {
                auto* sub_match = getMatchStructure(edge.v_list[i], p->sub_list[i]);
                if (!sub_match) {
                    flag = false; break;
                }
                sub_list.push_back(sub_match);
            }
            if (flag) return new TopDownGraphMatchStructure(edge_id, p, sub_list);
            for (auto* sub_match: sub_list) delete sub_match;
        }
    }
    LOG(INFO) << "fail in match " << node_id << " " << p->toString() << std::endl;
    return nullptr;
}


namespace {
    std::vector<ProgramInfo> _getAllProgramsFromEdge(TopDownContextGraph* g, const _MergePosInfo& info,
            int limit, const std::vector<FlattenGrammar::ParamInfo>& param_info_list) {
        auto& edge = g->node_list[info.node_id].edge_list[info.edge_id];
        std::vector<ProgramInfo> res;
        for (int pos = 0; pos < edge.v_list.size(); ++pos) {
            if (edge.v_list[pos] != info.fix_pos) continue;
            int approx_num = 1;
            std::vector<std::vector<ProgramInfo>> storage;
            for (int i = 0; i < edge.v_list.size(); ++i) {
                int sub_id = edge.v_list[i];
                std::vector<ProgramInfo> info_list;
                for (int edge_id = 0; edge_id < g->node_list[sub_id].edge_list.size(); ++edge_id) {
                    auto& sub_edge = g->node_list[sub_id].edge_list[edge_id];
                    if (sub_id == info.fix_pos) {
                        if (i == pos && edge_id != info.fix_res_id) continue;
                        if (i < pos && edge_id == info.fix_res_id) continue;
                    }
                    assert(_isTerminateSemantics(sub_edge.semantics.get()));
                    if (dynamic_cast<ConstSemantics*>(sub_edge.semantics.get())) {
                        info_list.emplace_back(std::make_shared<Program>(sub_edge.semantics, (ProgramList){}), sub_edge.weight);
                    } else {
                        auto* ps = dynamic_cast<ParamSemantics*>(sub_edge.semantics.get());
                        assert(ps);
                        info_list.emplace_back(param_info_list[ps->id].program, sub_edge.weight);
                    }
                }
                storage.push_back(info_list);
                approx_num = std::min(1ll * approx_num * int(info_list.size()), 1ll * limit + 1);
            }
            if (approx_num + res.size() > limit) return {};
            for (auto& program_info: _collectAllProgram(storage, edge)) {
                res.push_back(program_info);
            }
        }
        return res;
    }
}

TrivialFlattenGrammar::TrivialFlattenGrammar(TopDownContextGraph *_g, Env *_env, int flatten_num, ProgramChecker* validator): FlattenGrammar(_g, _env) {
    assert(_g->prob_type == ProbModelType::NORMAL_PROB);
    for (auto& node: graph->node_list) {
        for (auto& edge: node.edge_list) {
            auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
            if (ps) {
                while (param_info_list.size() <= ps->id) param_info_list.emplace_back();
                if (ps->oup_type) {
                    auto pp = program::buildParam(ps->id, ps->oup_type);
                    param_info_list[ps->id] = {ps->oup_type, pp};
                }
            }
        }
    }

    std::vector<int> param_size_list;
    for (int i = 0; i < param_info_list.size(); ++i) {
        param_map[param_info_list[i].program->toString()] = i;
    }
    LOG(INFO) << "start" << std::endl;
    while (1) {
        for (int i = param_size_list.size(); i < param_info_list.size(); ++i) {
            param_size_list.push_back(param_info_list[i].program->size());
        }
        auto best_pos = _getBestMergePos(graph, param_size_list, env);
        if (best_pos.node_id == -1) break;

        auto info_list = _getAllProgramsFromEdge(graph, best_pos, KExtraFactor * flatten_num, param_info_list);
        if (info_list.empty()) break;
        int now = 0;
        for (auto& info: info_list) {
            if (!validator->isValid(info.first.get())) continue;
            info_list[now++] = info;
            auto feature = info.first->toString();
            if (param_map.count(feature) == 0) {
                if (param_map.size() > flatten_num) break;
                param_map[feature] = -1;
            }
        }
        info_list.resize(now);
        if (param_map.size() > flatten_num) break;
        _separateUsage(graph, best_pos);

        auto& node = graph->node_list[best_pos.node_id];
        auto& fix_node = graph->node_list[best_pos.fix_pos];
        double total = 0.0;
        for (auto& edge: fix_node.edge_list) total += edge.weight;
        assert(total >= 1-1e-8 && total <= 1+1e-8);
        total -= fix_node.edge_list[best_pos.fix_res_id].weight;
        for (int i = best_pos.fix_res_id + 1; i < fix_node.edge_list.size(); ++i) {
            fix_node.edge_list[i - 1] = fix_node.edge_list[i];
        }
        fix_node.edge_list.pop_back();
        if (!fix_node.edge_list.empty()) {
            for (int i = 0; i < fix_node.edge_list.size(); ++i) fix_node.edge_list[i].weight /= total;
        }
        for (auto v: node.edge_list[best_pos.edge_id].v_list) if (v == best_pos.fix_pos) node.edge_list[best_pos.edge_id].weight *= total;
        for (auto& p_info: info_list) {
            auto feature = p_info.first->toString();
            assert(param_map.count(feature));
            int id = param_map[feature];
            if (id == -1) {
                id = param_info_list.size();
                param_info_list.emplace_back(node.symbol->type, p_info.first);
                param_map[feature] = id;
            }
            auto sem = semantics::buildParamSemantics(id, param_info_list[id].type);
            node.edge_list.emplace_back(best_pos.node_id, (std::vector<int>){}, p_info.second, sem);
        }
        if (fix_node.edge_list.empty()) _deleteEmptyNode(graph);
    }

    // init prob and param_map
    param_map.clear();
    for (int i = 0; i < param_info_list.size(); ++i) {
        param_map[param_info_list[i].program->toString()] = i;
    }
    graph->normalizeProbability();
}

TrivialFlattenGrammarBuilder::TrivialFlattenGrammarBuilder(Grammar *g, TopDownModel *model, Env *_env, int _flatten_num, ProgramChecker* _validator):
    FlattenGrammarBuilder(g, model), env(_env), flatten_num(_flatten_num), validator(_validator) {
    LOG(INFO) << "env " << env << std::endl;
}
// todo: memory leak through fg
FlattenGrammar * TrivialFlattenGrammarBuilder::getFlattenGrammar(int depth) {
    auto* limited_grammar = grammar::generateHeightLimitedGrammar(grammar, depth);
    auto* graph = new TopDownContextGraph(limited_grammar, model, ProbModelType::NORMAL_PROB);
    graph->normalizeProbability();
    auto* res = new TrivialFlattenGrammar(graph, env, flatten_num, validator);
    // std::cout << std::endl << std::endl;
    //res->print();
    return res;
}
TrivialFlattenGrammarBuilder::~TrivialFlattenGrammarBuilder() noexcept {
    delete validator;
}

namespace {
    void _mergeSameEdge(TopDownContextGraph::Node& node) {
        std::unordered_map<int, int> param_pos;
        std::unordered_map<std::string, int> const_pos;
        int now = 0;
        for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
            auto& edge = node.edge_list[edge_id];
            auto* cs = dynamic_cast<ConstSemantics*>(edge.semantics.get());
            if (cs) {
                auto feature = cs->w.toString();
                if (const_pos.count(feature)) {
                    node.edge_list[const_pos[feature]].weight += edge.weight;
                } else {
                    const_pos[feature] = now;
                    node.edge_list[now++] = edge;
                }
                continue;
            }
            auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
            if (ps) {
                if (param_pos.count(ps->id)) {
                    node.edge_list[param_pos[ps->id]].weight += edge.weight;
                } else {
                    param_pos[ps->id] = now;
                    node.edge_list[now++] = edge;
                }
                continue;
            }
            node.edge_list[now++] = edge;
        }
        while (node.edge_list.size() > now) node.edge_list.pop_back();
    }
}

MergedFlattenGrammar::MergedFlattenGrammar(TopDownContextGraph *_g, Env *env, int flatten_num, ProgramChecker* validator, const PEquivalenceCheckTool& _tool):
    FlattenGrammar(_g, env), tool(_tool) {
    assert(_g->prob_type == ProbModelType::NORMAL_PROB);
    for (auto& node: graph->node_list) {
        for (auto& edge: node.edge_list) {
            auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
            if (ps) {
                while (param_info_list.size() <= ps->id) param_info_list.emplace_back();
                if (ps->oup_type) {
                    auto pp = program::buildParam(ps->id, ps->oup_type);
                    param_info_list[ps->id] = {ps->oup_type, pp};
                }
            }
        }
    }

    std::vector<int> param_size_list;
    for (int i = 0; i < param_info_list.size(); ++i) {
        param_map[param_info_list[i].program->toString()] = i;
        tool->insertProgram(param_info_list[i].program);
    }
    while (1) {
        for (int i = param_size_list.size(); i < param_info_list.size(); ++i) {
            param_size_list.push_back(param_info_list[i].program->size());
        }
        auto best_pos = _getBestMergePos(graph, param_size_list, env);
        if (best_pos.node_id == -1) break;

        auto info_list = _getAllProgramsFromEdge(graph, best_pos, KExtraFactor * flatten_num, param_info_list);
        // LOG(INFO) << "end merge " << info_list.size();
        if (info_list.empty()) break;
        int now = 0;
        for (auto& info: info_list) {
            if (!validator->isValid(info.first.get())) continue;
            info_list[now++] = info;
            if (tool->getConst(info.first.get()).isNull()) {
                auto feature = tool->insertProgram(info.first)->toString();
                if (param_map.count(feature) == 0) param_map[feature] = -1;
            }
        }
        info_list.resize(now);
        if (param_map.size() > flatten_num) break;
        _separateUsage(graph, best_pos);

        auto& node = graph->node_list[best_pos.node_id];
        auto& fix_node = graph->node_list[best_pos.fix_pos];
        double total = 0.0;
        for (auto& edge: fix_node.edge_list) total += edge.weight;
        total -= fix_node.edge_list[best_pos.fix_res_id].weight;
        for (int i = best_pos.fix_res_id + 1; i < fix_node.edge_list.size(); ++i) {
            fix_node.edge_list[i - 1] = fix_node.edge_list[i];
        }
        fix_node.edge_list.pop_back();
        if (!fix_node.edge_list.empty()) {
            for (int i = 0; i < fix_node.edge_list.size(); ++i) fix_node.edge_list[i].weight /= total;
        }
        for (auto v: node.edge_list[best_pos.edge_id].v_list) if (v == best_pos.fix_pos) node.edge_list[best_pos.edge_id].weight *= total;
        for (auto& p_info: info_list) {
            auto cons = tool->getConst(p_info.first.get());
            if (!cons.isNull()) {
                auto sem = semantics::buildConstSemantics(cons);
                node.edge_list.emplace_back(best_pos.node_id, (std::vector<int>){}, p_info.second, sem);
                continue;
            }
            auto feature = tool->insertProgram(p_info.first)->toString();
            // if (feature != p_info.first->toString()) LOG(INFO) << "merge " << p_info.first->toString() << " " << feature << std::endl;
            assert(param_map.count(feature));
            int id = param_map[feature];
            if (id == -1) {
                id = param_info_list.size();
                param_info_list.emplace_back(node.symbol->type, p_info.first);
                param_map[feature] = id;
            }
            auto sem = semantics::buildParamSemantics(id, param_info_list[id].type);
            node.edge_list.emplace_back(best_pos.node_id, (std::vector<int>){}, p_info.second, sem);
        }
        _mergeSameEdge(node);
        if (fix_node.edge_list.empty()) _deleteEmptyNode(graph);
    }

    // init prob and param_map
    param_map.clear();
    for (int i = 0; i < param_info_list.size(); ++i) {
        param_map[param_info_list[i].program->toString()] = i;
    }
    graph->normalizeProbability();
}

TopDownGraphMatchStructure * MergedFlattenGrammar::getMatchStructure(int node_id, const PProgram &program) const {
    auto cons = tool->getConst(program.get());
    auto& node = graph->node_list[node_id];
    if (!cons.isNull()) {
        for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
            auto *cs = dynamic_cast<ConstSemantics *>(node.edge_list[edge_id].semantics.get());
            if (cs && cs->w == cons) return new TopDownGraphMatchStructure(edge_id, program::buildConst(cons), {});
        }
    }
    auto param_prog = tool->queryProgram(program);
    if (param_prog) {
        auto feature = param_prog->toString(); // std::cout << "param " << program->toString() << " " << feature << std::endl;
        auto it = param_map.find(feature);
        if (it != param_map.end()) {
            int param_id = it->second;
            for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                auto *ps = dynamic_cast<ParamSemantics *>(node.edge_list[edge_id].semantics.get());
                if (ps && ps->id == param_id)
                    return new TopDownGraphMatchStructure(edge_id,
                                                          program::buildParam(param_id, param_info_list[param_id].type), {});
            }
        }
    }
    auto sem_name = program->semantics->getName();
    for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
        auto& edge = node.edge_list[edge_id];
        if (edge.semantics->getName() == sem_name) {
            std::vector<TopDownGraphMatchStructure*> sub_list;
            bool flag = true;
            for (int i = 0; i < program->sub_list.size(); ++i) {
                auto* sub_match = getMatchStructure(edge.v_list[i], program->sub_list[i]);
                if (!sub_match) {
                    flag = false; break;
                }
                sub_list.push_back(sub_match);
            }
            if (flag) return new TopDownGraphMatchStructure(edge_id, program, sub_list);
            for (auto* sub_match: sub_list) delete sub_match;
        }
    }
    LOG(INFO) << "fail in match " << program->toString() << std::endl;
    return nullptr;
}

MergedFlattenGrammarBuilder::MergedFlattenGrammarBuilder(Grammar *g, TopDownModel *model, Env *_env, int _flatten_num,
        ProgramChecker* _validator, const PEquivalenceCheckTool& _tool):
    FlattenGrammarBuilder(g, model), env(_env), flatten_num(_flatten_num), validator(_validator), tool(_tool) {
}
FlattenGrammar * MergedFlattenGrammarBuilder::getFlattenGrammar(int depth) {
    auto* limited_grammar = grammar::generateHeightLimitedGrammar(grammar, depth);
    auto* graph = new TopDownContextGraph(limited_grammar, model, ProbModelType::NORMAL_PROB);
    graph->normalizeProbability();
    return new MergedFlattenGrammar(graph, env, flatten_num, validator, tool);
}
MergedFlattenGrammarBuilder::~MergedFlattenGrammarBuilder() noexcept {
    delete validator;
}