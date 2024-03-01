//
// Created by pro on 2021/12/29.
//

#include <algorithm>
#include <queue>
#include "istool/solver/vsa/vsa_builder.h"
#include "glog/logging.h"

bool TrivialPruner::isPrune(VSANode *node) {return false;}
void TrivialPruner::clear() {}

SizeLimitPruner::SizeLimitPruner(int _size_limit, const VSANodeChecker& _checker): size_limit(_size_limit), remain(size_limit), checker(_checker) {
}
void SizeLimitPruner::clear() {remain = size_limit;}
bool SizeLimitPruner::isPrune(VSANode *node) {
    if (checker(node)) return true;
    if (remain) {
        --remain; return false;
    }
    return true;
}

VSABuilder::VSABuilder(Grammar *_g, VSAPruner *_pruner, Env* _env):
    g(_g), pruner(_pruner), ext(ext::vsa::getExtension(_env)), env(_env) {
}
VSABuilder::~VSABuilder() {
    delete pruner;
    for (auto& info: single_build_cache) ext::vsa::deleteVSA(info.second);
    for (auto& info: merge_build_cache) ext::vsa::deleteVSA(info.second);
}

VSANode * VSABuilder::buildVSA(const Data &oup, const DataList &inp_list, TimeGuard *guard) {
    auto feature = data::dataList2String(inp_list) + "->"+ oup.toString();
    if (single_build_cache.find(feature) != single_build_cache.end()) {
        return single_build_cache[feature];
    }
    ext->prepareEnv(g, {inp_list, oup});
    auto res = single_build_cache[feature] = _buildVSA(oup, inp_list, guard);
    LOG(INFO) << "Build single size " << ext::vsa::getEdgeSize(res);
    return res;
}
VSANode * VSABuilder::mergeVSA(VSANode *l, VSANode *r, TimeGuard *guard) {
    if (merge_build_cache.find({l, r}) != merge_build_cache.end()) {
        return merge_build_cache[{l, r}];
    }
    auto res = merge_build_cache[{l, r}] = _mergeVSA(l, r, guard);
    LOG(INFO) << "Merge size " << ext::vsa::getEdgeSize(res);
    return res;
}

VSANode* VSABuilder::buildFullVSA() {
    if (single_build_cache.find("") != single_build_cache.end()) {
        return single_build_cache[""];
    }
    g->indexSymbol();
    VSANodeList node_list;
    auto total = std::make_shared<TotalWitnessValue>();
    for (auto* symbol: g->symbol_list) {
        node_list.push_back(new SingleVSANode(symbol, total));
    }
    for (auto* symbol: g->symbol_list) {
        for (auto* rule: symbol->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "Current implementation of VSA requires ConcreteRule";
            VSANodeList sub_list;
            for (auto* sub_symbol: rule->param_list) {
                sub_list.push_back(node_list[sub_symbol->id]);
            }
            node_list[symbol->id]->edge_list.emplace_back(cr->semantics, sub_list);
        }
    }
    ext::vsa::cleanUpVSA(node_list[0]);
    return single_build_cache[""] = node_list[0];
}

DFSVSABuilder::DFSVSABuilder(Grammar *_g, VSAPruner *pruner, Env* env):
    VSABuilder(_g, pruner, env) {
}
BFSVSABuilder::BFSVSABuilder(Grammar *_g, VSAPruner *pruner, Env* env):
    VSABuilder(_g, pruner, env) {
}

VSANode* DFSVSABuilder::buildVSA(NonTerminal *nt, const WitnessData &oup, const DataList &inp_list, TimeGuard *guard, std::unordered_map<std::string, VSANode*> &cache) {
    std::string feature = nt->name + "@" + oup->toString();
    if (cache.find(feature) != cache.end()) return cache[feature];
    VSANode* node = new SingleVSANode(nt, oup);
    cache[feature] = node;
    if (pruner->isPrune(node)) return node;
    TimeCheck(guard);
    for (auto* rule: nt->rule_list) {
        auto* cr = dynamic_cast<ConcreteRule*>(rule);
        if (!cr) LOG(FATAL) << "Current implementation of VSA requires ConcreteRule";
        auto witness = ext->getWitness(cr->semantics.get(), oup, inp_list);
        for (auto& wl: witness) {
#ifdef DEBUG
            assert(wl.size() == rule->param_list.size());
#endif
            VSANodeList node_list;
            for (int i = 0; i < wl.size(); ++i) {
                auto sub_node = buildVSA(rule->param_list[i], wl[i], inp_list, guard, cache);
                node_list.push_back(sub_node);
            }
            node->edge_list.emplace_back(cr->semantics, node_list);
        }
    }
    return node;
}

VSANode* DFSVSABuilder::_buildVSA(const Data &oup, const DataList &inp_list, TimeGuard *guard) {
    pruner->clear();
    std::unordered_map<std::string, VSANode*> cache;
    auto wit_oup = std::make_shared<DirectWitnessValue>(oup);
    auto root = buildVSA(g->start, wit_oup, inp_list, guard, cache);
    ext::vsa::cleanUpVSA(root);
    return root;
}

namespace {
    struct EdgeMatchStatus {
        int ll, lr, rl, rr;
    };

    std::vector<EdgeMatchStatus> matchVSAEdgeList(const VSAEdgeList& l_list, const VSAEdgeList& r_list) {
        std::vector<EdgeMatchStatus> res;
        int l_pos = 0, r_pos = 0;
        while (l_pos < l_list.size() && r_pos < r_list.size()) {
            auto l_name = l_list[l_pos].semantics->getName();
            auto r_name = r_list[r_pos].semantics->getName();
            if (l_name != r_name) {
                if (l_name < r_name) ++l_pos; else ++r_pos;
                continue;
            }
            int l_ne = l_pos + 1;
            while (l_ne < l_list.size() && l_list[l_ne].semantics->getName() == l_name) ++l_ne;
            int r_ne = r_pos + 1;
            while (r_ne < r_list.size() && r_list[r_ne].semantics->getName() == r_name) ++r_ne;
            res.push_back({l_pos, l_ne, r_pos, r_ne});
            l_pos = l_ne; r_pos = r_ne;
        }
        return res;
    }
}

VSANode* DFSVSABuilder::mergeVSA(VSANode* l, VSANode* r, TimeGuard *guard, std::unordered_map<std::string, VSANode*> &cache) {
    std::string feature = std::to_string(l->id) + "@" + std::to_string(r->id);
    if (cache.find(feature) != cache.end()) return cache[feature];
    auto node = new MultiExampleVSANode(l, r);
    cache[feature] = node;
    if (pruner->isPrune(node)) return node;
    for (const auto& status: matchVSAEdgeList(l->edge_list, r->edge_list)) {
        for (int i = status.ll; i < status.lr; ++i)
            for (int j = status.rl; j < status.rr; ++j) {
                auto& le = l->edge_list[i], &re = r->edge_list[j];
#ifdef DEBUG
                assert(le.semantics->getName() == re.semantics->getName() && le.node_list.size() == re.node_list.size());
#endif
                VSANodeList node_list;
                for (int k = 0; k < le.node_list.size(); ++k) {
                    node_list.push_back(mergeVSA(le.node_list[k], re.node_list[k], guard, cache));
                }
                node->edge_list.emplace_back(le.semantics, node_list);
            }
    }
    return node;
}

namespace {
    void orderEdgeList(VSANode* node, std::vector<bool>& cache) {
        if (cache[node->id]) return;
        cache[node->id] = true; int n = node->edge_list.size();
        std::vector<std::pair<std::string, int>> id_list(n);
        for (int i = 0; i < n; ++i) id_list[i] = {node->edge_list[i].semantics->getName(), i};
        std::sort(id_list.begin(), id_list.end());
        auto copy = node->edge_list;
        for (int i = 0; i < id_list.size(); ++i) {
            node->edge_list[i] = copy[id_list[i].second];
        }
        for (const auto& e: node->edge_list) {
            for (auto* sub_node: e.node_list) {
                orderEdgeList(sub_node, cache);
            }
        }
    }

    void orderEdgeList(VSANode* node) {
        int n = ext::vsa::indexVSANode(node);
        std::vector<bool> cache(n);
        for (int i = 0; i < n; ++i) cache[i] = false;
        orderEdgeList(node, cache);
    }
}

VSANode* DFSVSABuilder::_mergeVSA(VSANode* l, VSANode* r, TimeGuard *guard) {
    pruner->clear();
    orderEdgeList(l); orderEdgeList(r);
    std::unordered_map<std::string, VSANode*> cache;
    auto root = mergeVSA(l, r, guard, cache);
    ext::vsa::cleanUpVSA(root);
    return root;
}

VSANode* BFSVSABuilder::_buildVSA(const Data &oup, const DataList &inp_list, TimeGuard *guard) {
    std::unordered_map<std::string, VSANode*> cache;
    std::queue<VSANode*> Q; pruner->clear();
    auto insert = [&](NonTerminal* nt, const WitnessData& d) {
        std::string feature = nt->name + "@" + d->toString();
        if (cache.find(feature) != cache.end()) return cache[feature];
        auto *node = new SingleVSANode(nt, d);
        if (!pruner->isPrune(node)) Q.push(node);
        return cache[feature] = node;
    };
    auto init_d = std::make_shared<DirectWitnessValue>(oup);
    auto root = insert(g->start, init_d);
    while (!Q.empty()) {
        TimeCheck(guard);
        auto node = Q.front(); Q.pop();
        auto* sn = dynamic_cast<SingleVSANode*>(node);
        for (auto* rule: node->symbol->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "Current implementation of VSA requires ConcreteRule";
            auto witness = ext->getWitness(cr->semantics.get(), sn->oup, inp_list);
            for (auto& wl: witness) {
#ifdef DEBUG
                assert(wl.size() == rule->param_list.size());
#endif
                VSANodeList node_list;
                for (int i = 0; i < wl.size(); ++i) {
                    node_list.push_back(insert(rule->param_list[i], wl[i]));
                }
                VSAEdge edge(cr->semantics, node_list);
                node->edge_list.push_back(edge);
            }
        }
    }
    ext::vsa::cleanUpVSA(root);
    return root;
}

VSANode* BFSVSABuilder::_mergeVSA(VSANode* l, VSANode* r, TimeGuard *guard) {
    pruner->clear();
    orderEdgeList(l); orderEdgeList(r);
    std::unordered_map<std::string, VSANode*> cache;
    std::queue<VSANode*> Q;
    auto insert = [&](VSANode* l, VSANode* r) {
        std::string feature = std::to_string(l->id) + "@" + std::to_string(r->id);
        if (cache.find(feature) != cache.end()) return cache[feature];
        auto *node = new MultiExampleVSANode(l, r);
        if (!pruner->isPrune(node)) Q.push(node);
        return cache[feature] = node;
    };

    auto root = insert(l, r);
    int num = 0;
    while (!Q.empty()) {
        TimeCheck(guard); ++num;
        auto node = Q.front(); Q.pop();
        auto* mn = dynamic_cast<MultiExampleVSANode*>(node);
        auto l_node = mn->l, r_node = mn->r;
        for (auto status: matchVSAEdgeList(l_node->edge_list, r_node->edge_list)) {
            for (int i = status.ll; i < status.lr; ++i)
                for (int j = status.rl; j < status.rr; ++j) {
                    auto& le = l_node->edge_list[i], &re = r_node->edge_list[j];
#ifdef DEBUG
                    assert(le.semantics->getName() == re.semantics->getName() && le.node_list.size() == re.node_list.size());
#endif
                    VSANodeList node_list;
                    for (int k = 0; k < le.node_list.size(); ++k) {
                        node_list.push_back(insert(le.node_list[k], re.node_list[k]));
                    }
                    VSAEdge edge(le.semantics, node_list);
                    node->edge_list.push_back(edge);
                }
        }
    }

    ext::vsa::cleanUpVSA(root);
    return root;
}