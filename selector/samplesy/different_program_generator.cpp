//
// Created by pro on 2022/2/7.
//

#include "istool/selector/samplesy/different_program_generator.h"
#include "glog/logging.h"
#include <unordered_set>
#include <iostream>

using namespace selector::samplesy;

CollectRes::CollectRes(const PProgram &_p, const Data &_oup): p(_p), oup(_oup) {}

VSADifferentProgramGenerator::VSADifferentProgramGenerator(const PVSABuilder& _builder): builder(_builder) {
    ext = builder->ext;
    setRoot(builder->buildFullVSA());
    LOG(INFO) << "VSA Different Root " << root;
}
void VSADifferentProgramGenerator::addExample(const IOExample &example) {
    auto feature = example::ioExample2String(example);
    if (added_example_set.find(feature) != added_example_set.end()) return;
    added_example_set.insert(feature);
    auto next_root = builder->buildVSA(example.second, example.first, nullptr);
    setRoot(builder->mergeVSA(root, next_root, nullptr));
    LOG(INFO) << "Remain program: " << ext::vsa::getProgramNum(root);
}

namespace {
    void _collectAllNode(VSANode* node, std::vector<VSANode*>& node_list) {
        if (node_list[node->id]) return;
        node_list[node->id] = node;
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                _collectAllNode(sub_node, node_list);
            }
        }
    }

    void _mergeAllResult(int pos, const std::vector<std::vector<int>>& A, std::vector<int>& tmp, std::vector<std::vector<int>>& res) {
        if (pos == A.size()) {
            res.push_back(tmp); return;
        }
        for (int w: A[pos]) {
            tmp[pos] = w;
            _mergeAllResult(pos + 1, A, tmp, res);
        }
    }

    std::vector<std::vector<int>> _mergeAllResult(const std::vector<std::vector<int>>& A) {
        std::vector<std::vector<int>> res;
        std::vector<int> tmp(A.size());
        _mergeAllResult(0, A, tmp, res);
        return res;
    }

    std::vector<int> _buildRange(int l, int r) {
        std::vector<int> res;
        for (int i = l; i < r; ++i) res.push_back(i);
        return res;
    }
}

void VSADifferentProgramGenerator::setRoot(VSANode* new_root) {
    root = new_root;
    int n = ext::vsa::indexVSANodeByTopoSort(root);
    node_list.resize(n);
    for (int i = 0; i < n; ++i) node_list[i] = nullptr;
    _collectAllNode(root, node_list);
}

selector::samplesy::CollectRes VSADifferentProgramGenerator::getRes(const VSAEdge &edge, const std::vector<int>& id_list,
                                                                    ExecuteInfo* info) {
    auto* fs = dynamic_cast<FullExecutedSemantics*>(edge.semantics.get());
    ProgramList sub_programs;
    for (int i = 0; i < id_list.size(); ++i) {
        sub_programs.push_back(res_pool[edge.node_list[i]->id][id_list[i]].p);
    }
    PProgram res_p = std::make_shared<Program>(edge.semantics, sub_programs);
    Data res_oup;
    if (fs) {
        DataList inp;
        for (int i = 0; i < id_list.size(); ++i) {
            inp.push_back(res_pool[edge.node_list[i]->id][id_list[i]].oup);
        }
        res_oup = fs->run(std::move(inp), info);
    } else res_oup = edge.semantics->run(sub_programs, info);
    assert(builder->env->run(res_p.get(), info->param_value) == res_oup);
    return {res_p, res_oup};
}

bool VSADifferentProgramGenerator::isValidWitness(const VSAEdge &edge, const Data &oup, const std::vector<int> &id_list, const DataList &params) {
    auto oup_wit = std::make_shared<DirectWitnessValue>(oup);
    auto wit_list = ext->getWitness(edge.semantics.get(), oup_wit, params);
    DataList inp_list;
    for (int i = 0; i < id_list.size(); ++i) {
        inp_list.push_back(res_pool[edge.node_list[i]->id][id_list[i]].oup);
    }
    for (const auto& wit_term: wit_list) {
        bool is_valid = true;
        for (int i = 0; i < wit_term.size(); ++i) {
            if (!wit_term[i]->isInclude(inp_list[i])) {
                is_valid = false; break;
            }
        }
        if (is_valid) return true;
    }
    // std::cout << "banned " << edge.semantics->getName() << " " << oup.toString() << " " << data::dataList2String(inp_list) << std::endl;
    return false;
}

bool VSADifferentProgramGenerator::extendResPool(int limit, ExecuteInfo* info) {
    int n = node_list.size();
    std::vector<int> pre_size_list(n);
    bool is_finished = true;
    for (int i = 0; i < n; ++i) pre_size_list[i] = res_pool[i].size();
    for (int id = n - 1; id >= 0; --id) {
        auto* node = node_list[id];
        // LOG(INFO) << "Node " << node->toString();
        std::unordered_set<std::string> existing_result;
        for (const auto& d: res_pool[id]) existing_result.insert(d.oup.toString());
        for (const auto& edge: node->edge_list) {
            if (!is_finished && res_pool[id].size() == limit) break;
            // LOG(INFO) << "Edge " << edge.semantics->getName();
            int m = edge.node_list.size();
            std::vector<std::vector<int>> A(m);
            std::vector<int> id_list(m);
            for (int i = 0; i < m; ++i) id_list[i] = edge.node_list[i]->id;
            for (int i = 0; i < m; ++i) A[i] = _buildRange(0, res_pool[id_list[i]].size());
            A = _mergeAllResult(A);

            for (const auto& plan: A) {
                if (!is_finished && res_pool[id].size() == limit) break;
                bool is_new = plan.empty();
                for (int i = 0; i < plan.size(); ++i) {
                    if (plan[i] >= pre_size_list[id_list[i]]) {
                        is_new = true; break;
                    }
                }
                if (!is_new) {
                    continue;
                }

                auto res = getRes(edge, plan, info);
                auto feature = res.oup.toString();
                if (existing_result.find(feature) != existing_result.end()) continue;
                if (!isValidWitness(edge, res.oup, plan, info->param_value)) continue;
                existing_result.insert(feature);

                if (res_pool[id].size() == limit) {
                    is_finished = false; break;
                }
                res_pool[id].push_back(res);
            }
        }
    }
    return is_finished;
}

ProgramList VSADifferentProgramGenerator::getDifferentProgram(const IOExample &example, int num) {
    int limit = num;
    auto* info = new ExecuteInfo(example.first, {});
    ext->prepareEnv(builder->g, example);
    res_pool.clear();
    for (int i = 0; i < node_list.size(); ++i) res_pool.emplace_back();

    for (int i = 0; i < node_list.size(); ++i) node_list[i]->id = i;
    for (auto* node: node_list) {
        assert(node);
        for (auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                assert(sub_node->id > node->id);
            }
        }
    }

    while (1) {
        bool is_finished = extendResPool(limit, info);
        if (res_pool[0].size() >= num || is_finished) break;
        limit *= 2;
    }
    if (res_pool[0].empty()) {
        int cared_id = 0;
        auto* node = node_list[cared_id];
        std::cout << "Cared id: " << cared_id << " " << node_list[cared_id]->toString() << std::endl;
        for (const auto& edge: node->edge_list) {
            std::cout << "  " << edge.toString() << std::endl;
            for (auto* sub_node: edge.node_list) {
                std::cout << "    " << sub_node->toString() << " ";
                for (auto& res_info: res_pool[sub_node->id]) {
                    std::cout << res_info.oup.toString() << "@" << res_info.p->toString() << " ";
                }
                std::cout << std::endl;
            }
        }
        LOG(FATAL) << "No valid program";
    }
    assert(!res_pool[0].empty());
    ProgramList res_list;
    for (auto& res: res_pool[0]) {
        res_list.push_back(res.p);
    }
    if (res_list.size() > num) res_list.resize(num);
    return res_list;
}