//
// Created by pro on 2022/6/11.
//

#include "istool/selector/random/learned_scorer.h"
#include "glog/logging.h"

DifferentPairCacheItem::DifferentPairCacheItem(TopDownContextGraph::Node *_node): node(_node), example_num(0) {
    index_list.resize(node->edge_list.size(), -1);
    for (int i = 0; i < node->edge_list.size(); ++i) {
        auto& edge = node->edge_list[i];
        if (selector::random::isTerminate(edge)) {
            terminate_list.push_back(&edge);
        }
    }
    match_storage.resize(terminate_list.size(), std::vector<int>(terminate_list.size(), 0));
    res_list.resize(terminate_list.size(), nullptr);
    full_res = nullptr;
    initResList();
}
void DifferentPairCacheItem::initResList() {
    delete[] full_res;
    full_res = new RandomSemanticsScore[1 << example_num];
    for (int i = 0; i < (1 << example_num); ++i) full_res[i] = 0.0;
    for (int x_id = 0; x_id < terminate_list.size(); ++x_id) {
        delete[] res_list[x_id];
        res_list[x_id] = new RandomSemanticsScore[1 << example_num];
        for (int i = 0; i < (1 << example_num); ++i) res_list[x_id][i] = 0.0;
        for (int y_id = 0; y_id < terminate_list.size(); ++y_id) {
            res_list[x_id][match_storage[x_id][y_id]] += terminate_list[y_id]->weight;
            full_res[match_storage[x_id][y_id]] += terminate_list[x_id]->weight * terminate_list[y_id]->weight;
        }
    }
}
DifferentPairCacheItem::~DifferentPairCacheItem() {
    delete[] full_res;
    for (auto* res: res_list) delete[] res;
}
std::unordered_map<std::string, std::vector<int> > DifferentPairCacheItem::getEqClass(const DataList &inp) {
    std::unordered_map<std::string, std::vector<int>> class_map;
    for (int i = 0; i < terminate_list.size(); ++i) {
        class_map[selector::random::getTerminateOutput(*terminate_list[i], inp).toString()].push_back(i);
    }
    return class_map;
}
void DifferentPairCacheItem::pushExample(const DataList &inp) {
    auto m = getEqClass(inp);
    for (auto& info: m) {
        for (int x_id: info.second)
            for (int y_id: info.second) match_storage[x_id][y_id] += (1 << example_num);
    }
    ++example_num;
    initResList();
}
void DifferentPairCacheItem::popExample() {
    for (auto& match_list: match_storage) {
        for (auto& match: match_list) match >>= 1;
    }
    --example_num;
    initResList();
}
namespace {
    void _getBitwiseSuffixSum(RandomSemanticsScore* x, int n) {
        for (int i = 0; i < n; ++i) {
            for (int S = 0; S < (1 << n); ++S)
                if (!(S & (1 << i))) x[S] += x[S + (1 << i)];
        }
    }
}

std::vector<RandomSemanticsScore*> DifferentPairCacheItem::getTerminateResult(const DataList &inp) {
    std::vector<RandomSemanticsScore*> res;
    int n = example_num + 1;
    for (int x_id = 0; x_id < res_list.size(); ++x_id) {
        auto* x = new RandomSemanticsScore[1 << n];
        for (int i = 0; i < (1 << example_num); ++i) x[i] = res_list[x_id][i];
        for (int i = (1 << example_num); i < (1 << n); ++i) x[i] = 0.0;
        res.push_back(x);
    }
    auto m = getEqClass(inp);
    for (auto& info: m) {
        for (int x_id: info.second)
            for (int y_id: info.second) {
                int now = match_storage[x_id][y_id];
                assert(now < (1 << example_num) && std::max(x_id, y_id) < terminate_list.size());
                res[x_id][now] -= terminate_list[y_id]->weight;
                res[x_id][now + (1 << example_num)] += terminate_list[y_id]->weight;
            }
    }
    for (auto* x: res) _getBitwiseSuffixSum(x, n);
    return res;
}
void DifferentPairCacheItem::getFullResult(const DataList &inp, RandomSemanticsScore* res) {
    int n = example_num + 1;
    for (int i = 0; i < (1 << example_num); ++i) res[i] = full_res[i];
    for (int i = (1 << example_num); i < (1 << n); ++i) res[i] = 0.0;
    auto m = getEqClass(inp);
    for (auto& info: m) {
        for (int x_id: info.second)
            for (int y_id: info.second) {
                int now = match_storage[x_id][y_id];
                auto w = terminate_list[x_id]->weight * terminate_list[y_id]->weight;
                res[now] -= w; res[now + (1 << example_num)] += w;
            }
    }
    _getBitwiseSuffixSum(res, n);
}
DifferentPairCache::DifferentPairCache(TopDownContextGraph *_graph): graph(_graph) {
    for (int i = 0; i < graph->node_list.size(); ++i) {
        item_list.push_back(new DifferentPairCacheItem(&(graph->node_list[i])));
    }
}
void DifferentPairCache::pushExample(const DataList &inp) {
    for (auto& item: item_list) item->pushExample(inp);
}
void DifferentPairCache::popExample() {
    for (auto& item: item_list) item->popExample();
}
void DifferentPairCache::getFullResult(int node_id, const DataList &inp, RandomSemanticsScore *res) {
    item_list[node_id]->getFullResult(inp, res);
}
std::vector<RandomSemanticsScore*> DifferentPairCache::getTerminateResult(int node_id, const DataList &inp) {
    assert(node_id < item_list.size());
    return item_list[node_id]->getTerminateResult(inp);
}
DifferentPairCache::~DifferentPairCache() {
    for (auto* item: item_list) delete item;
}

OptimizedDifferentPairLearnedScorer::OptimizedDifferentPairLearnedScorer(Env *env, FlattenGrammar *fg, RandomSemanticsLearner *_learner):
    LearnedScorer(env, fg, _learner), cache_pool(new DifferentPairCache(fg->graph)) {
}
void OptimizedDifferentPairLearnedScorer::pushExample(const Example &inp) {
    LearnedScorer::pushExample(inp); cache_pool->pushExample(fg->getFlattenInput(inp));
}
void OptimizedDifferentPairLearnedScorer::popExample() {
    LearnedScorer::popExample(); cache_pool->popExample();
}
OptimizedDifferentPairLearnedScorer::~OptimizedDifferentPairLearnedScorer() {
    delete cache_pool;
}

namespace {
    void _clear(RandomSemanticsScore* x, int n, RandomSemanticsScore w = 0.0) {
        for (int i = 0; i < n; ++i) x[i] = w;
    }
}

class OptimizedDifferentPairDPHolder {
public:
    std::vector<RandomSemanticsModel*> model_list;
    TopDownContextGraph* graph;
    DataList inp;
    RandomSemanticsModel* last_model;
    std::vector<RandomSemanticsScore*> pair_cache;
    std::unordered_map<std::string, RandomSemanticsScore*> triple_cache;
    int n, m_num;
    DifferentPairCache* cache_pool;
    std::vector<int> two2four;

    OptimizedDifferentPairDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list,
            const DataList& _inp, DifferentPairCache* _cache_pool): graph(_graph), model_list(_model_list),
            n(_model_list.size() * 2 - 1), m_num(_model_list.size()), cache_pool(_cache_pool), inp(_inp) {
        last_model = model_list[model_list.size() - 1];
        pair_cache.resize(graph->node_list.size(), nullptr);
        two2four.resize(1 << m_num, 0);
        for (int i = 1; i < (1 << m_num); ++i) {
            two2four[i] = (two2four[i >> 1] << 2) + (i & 1);
        }
    }
    ~OptimizedDifferentPairDPHolder() {
        for (auto* x: pair_cache) delete[] x;
        for (auto& info: triple_cache) delete[] info.second;
    }
    RandomSemanticsScore* getPairScore(int node_id) {
        if (pair_cache[node_id]) return pair_cache[node_id];
        auto* cache = pair_cache[node_id] = new RandomSemanticsScore[1 << m_num];
        _clear(cache, 1 << m_num); auto& node = graph->node_list[node_id];
        std::vector<int> nt_list, t_list;
        for (int i = 0; i < node.edge_list.size(); ++i) {
            if (!selector::random::isTerminate(node.edge_list[i])) nt_list.push_back(i);
            else t_list.push_back(i);
        }

        auto* tmp = new RandomSemanticsScore[1 << m_num];
        std::vector<RandomSemanticsScore> weight_list(m_num);
        { // Both x & y are terminate
            cache_pool->getFullResult(node_id, inp, tmp);
            for (int i = 0; i < (1 << m_num); ++i) cache[i] += tmp[i];
        }
        { // Only y is terminate
            for (auto x_id: nt_list) {
                for (auto y_id: t_list) {
                    auto weight = node.edge_list[x_id].weight * node.edge_list[y_id].weight * 2;
                    for (int i = 0; i < m_num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][x_id][y_id];
                    selector::random::buildSetProduct(tmp, weight_list);
                    for (int i = 0; i < (1 << m_num); ++i) cache[i] += tmp[i] * weight;
                }
            }
        }
        { // Both x and y are not terminate
            for (int x_pos = 0; x_pos < nt_list.size(); ++x_pos) {
                for (int y_pos = 0; y_pos <= x_pos; ++y_pos) {
                    int x_id = nt_list[x_pos], y_id = nt_list[y_pos];
                    auto weight = node.edge_list[x_id].weight * node.edge_list[y_id].weight * (1 + (x_pos != y_pos));
                    if (x_pos == y_pos) {
                        getPairResult(node_id, x_id, tmp);
                    } else {
                        for (int i = 0; i < m_num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][x_id][y_id];
                        selector::random::buildSetProduct(tmp, weight_list);
                    }
                    for (int i = 0; i < (1 << m_num); ++i) cache[i] += tmp[i] * weight;
                }
            }
        }
        return cache;
    }

    void getPairResult(int node_id, int edge_id, RandomSemanticsScore* res) {
        _clear(res, 1 << m_num, 1.0);
        auto& edge = graph->node_list[node_id].edge_list[edge_id];
        for (auto v: edge.v_list) {
            auto* sub_res = getPairScore(v);
            for (int i = 0; i < (1 << m_num); ++i) res[i] *= sub_res[i];
        }
        std::vector<RandomSemanticsScore> weight_list(m_num);
        for (int i = 0; i < m_num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][edge_id][edge_id];
        selector::random::updateViolateProb(res, weight_list);
    }

    void getTripleResult(int node_id, int edge_id, TopDownGraphMatchStructure* match, RandomSemanticsScore* res) {
        auto& edge = graph->node_list[node_id].edge_list[edge_id];
        _clear(res, 1 << n, 1.0);
        for (int i = 0; i < edge.v_list.size(); ++i) {
            int v = edge.v_list[i]; auto* sub = match->sub_list[i];
            auto* sub_res = getTripleScore(v, sub);
            for (int j = 0; j < (1 << n); ++j) res[j] *= sub_res[j];
        }
        std::vector<RandomSemanticsScore> weight_list(n);
        for (int i = 0; i < n; ++i) weight_list[i] = model_list[i >> 1]->weight_list[node_id][edge_id][edge_id];
        selector::random::updateViolateProb(res, weight_list);
    }

    void updateProduct(RandomSemanticsScore* x, RandomSemanticsScore* y, RandomSemanticsScore* res) {
        for (int i = 0; i < (1 << m_num); ++i) {
            for (int j = 0; j < (1 << m_num - 1); ++j) {
                res[two2four[i] + (two2four[j] << 1)] += x[i] * y[j];
            }
        }
    }

    RandomSemanticsScore* getTripleScore(int node_id, TopDownGraphMatchStructure* match) {
        auto feature = std::to_string(node_id) + "@" + match->program->toString();
        if (triple_cache.count(feature)) return triple_cache[feature];
        auto* cache = triple_cache[feature] = new RandomSemanticsScore[1 << n];
        for (int i = 0; i < (1 << n); ++i) cache[i] = 0.0;
        auto& node = graph->node_list[node_id]; int p_edge_id = match->edge_id;
        auto& p_edge = node.edge_list[p_edge_id];
        std::vector<int> nt_list, t_list;
        for (int i = 0; i < node.edge_list.size(); ++i) {
            if (!selector::random::isTerminate(node.edge_list[i])) nt_list.push_back(i);
            else t_list.push_back(i);
        }

        auto* sub_res = new RandomSemanticsScore[1 << n];
        getTripleResult(node_id, p_edge_id, match, sub_res);
        auto* tmp = new RandomSemanticsScore[1 << n];
        auto* pair_tmp = new RandomSemanticsScore[1 << n];


        //LOG(INFO) << "learn for " << feature;
        //for (int i = 0; i < (1 << n); ++i) std::cout << sub_res[i] << " "; std::cout << std::endl;
        //last_model->print(false);

        { // x is terminate
            auto match_res = cache_pool->getTerminateResult(node_id, inp);
            for (int x_pos = 0; x_pos < t_list.size(); ++x_pos) {
                int x_id = t_list[x_pos];
                std::vector<RandomSemanticsScore> weight_list(m_num);
                for (int i = 0; i < (1 << m_num); ++i) pair_tmp[i] = match_res[x_pos][i];
                for (int y_id: nt_list) {
                    for (int i = 0; i < m_num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][x_id][y_id];
                    selector::random::buildSetProduct(tmp, weight_list);
                    for (int i = 0; i < (1 << m_num); ++i) pair_tmp[i] += node.edge_list[y_id].weight * tmp[i];
                }
                weight_list.pop_back();
                if (x_id == p_edge_id) for (int i = 0; i < (1 << m_num - 1); ++i) tmp[i] = 1.0;
                else {
                    for (int i = 0; i + 1 < m_num; ++i)
                        weight_list[i] = model_list[i]->weight_list[node_id][x_id][p_edge_id];
                    selector::random::buildSetProduct(tmp, weight_list);
                }
                for (int i = 0; i < (1 << m_num - 1); ++i) tmp[i] *= node.edge_list[x_id].weight;
                updateProduct(pair_tmp, tmp, cache);
                /*LOG(INFO) << "x " << node.edge_list[x_id].semantics->getName();
                for (int i = 0; i < (1 << m_num); ++i) std::cout << pair_tmp[i] << " "; std::cout << std::endl;
                for (int i = 0; i < (1 << m_num - 1); ++i) std::cout << tmp[i] << " "; std::cout << std::endl;*/
            }
            //for (int i = 0; i < (1 << n); ++i) std::cout << cache[i] << " "; std::cout << std::endl;
            for (auto* x: match_res) delete[] x;
        }

        { // x is not terminate
            for (int x_id: nt_list) {
                for (int i = 0; i < (1 << m_num); ++i) pair_tmp[i] = 0.0;
                std::vector<RandomSemanticsScore> weight_list(m_num);
                for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                    if (y_id == x_id) {
                        if (y_id == p_edge_id) {
                            _clear(tmp, 1 << m_num, 0.0);
                            auto w = p_edge.weight * p_edge.weight;
                            for (int i = 0; i < (1 << n); ++i) cache[i] += sub_res[i] * w;
                        } else getPairResult(node_id, x_id, tmp);
                    } else {
                        for (int i = 0; i < m_num; ++i)
                            weight_list[i] = model_list[i]->weight_list[node_id][x_id][y_id];
                        selector::random::buildSetProduct(tmp, weight_list);
                    }
                    for (int i = 0; i < (1 << m_num); ++i) pair_tmp[i] += tmp[i] * node.edge_list[y_id].weight;
                }
                if (x_id != p_edge_id) {
                    weight_list.pop_back();
                    for (int i = 0; i + 1 < m_num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][x_id][p_edge_id];
                    selector::random::buildSetProduct(tmp, weight_list);
                } else {
                    for (int i = 0; i < (1 << m_num - 1); ++i) tmp[i] = sub_res[two2four[i] << 1];
                }
                for (int i = 0; i < (1 << m_num - 1); ++i) tmp[i] *= node.edge_list[x_id].weight;
                updateProduct(pair_tmp, tmp, cache);
            }
        }
        delete[] sub_res; delete[] tmp; delete[] pair_tmp;
        return cache;
    }

    RandomSemanticsScore getScore(TopDownGraphMatchStructure* match) {
        auto* res = getTripleScore(0, match);
        return res[(1 << n) - 1];
    }
};

RandomSemanticsScore OptimizedDifferentPairLearnedScorer::getScore(const PProgram &p, const Example &inp) {
    auto* model = learnModel(inp); model_list.push_back(model);
    auto* match = fg->getMatchStructure(p);
    auto* holder = new OptimizedDifferentPairDPHolder(fg->graph, model_list, fg->getFlattenInput(inp), cache_pool);
    auto res = holder->getScore(match);
    delete match; delete holder; model_list.pop_back();
    return res;
}