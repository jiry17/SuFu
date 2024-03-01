//
// Created by pro on 2022/6/27.
//
#include "istool/selector/random/learned_scorer.h"

TripleCacheItem::TripleCacheItem(TopDownContextGraph::Node *_node): node(_node), example_num(0) {
    index_list.resize(node->edge_list.size(), -1);
    for (int i = 0; i < node->edge_list.size(); ++i) {
        auto& edge = node->edge_list[i];
        if (selector::random::isTerminate(edge)) {
            index_list[i] = terminate_list.size();
            terminate_list.push_back(&edge);
        }
    }
    match_storage.resize(terminate_list.size(), std::vector<int>(terminate_list.size(), 0));
    full_res = nullptr;
    initResList();
}
void TripleCacheItem::initResList() {
    delete[] full_res;
    full_res = new RandomSemanticsScore[1 << example_num];
    for (int i = 0; i < (1 << example_num); ++i) full_res[i] = 0.0;
    for (int x_id = 0; x_id < terminate_list.size(); ++x_id) {
        for (int y_id = 0; y_id < terminate_list.size(); ++y_id) {
            full_res[match_storage[x_id][y_id]] += terminate_list[x_id]->weight * terminate_list[y_id]->weight;
        }
    }
}
TripleCacheItem::~TripleCacheItem() {
    delete[] full_res;
}
std::unordered_map<std::string, std::vector<int> > TripleCacheItem::getEqClass(const DataList &inp) {
    std::unordered_map<std::string, std::vector<int>> class_map;
    for (int i = 0; i < terminate_list.size(); ++i) {
        class_map[selector::random::getTerminateOutput(*terminate_list[i], inp).toString()].push_back(i);
    }
    return class_map;
}
void TripleCacheItem::pushExample(const DataList &inp) {
    auto m = getEqClass(inp);
    for (auto& info: m) {
        for (int x_id: info.second)
            for (int y_id: info.second) match_storage[x_id][y_id] += (1 << example_num);
    }
    ++example_num;
    initResList();
}
void TripleCacheItem::popExample() {
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
void TripleCacheItem::getFullResult(const DataList &inp, RandomSemanticsScore* res) {
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

std::unordered_map<std::string, std::vector<int> > TripleCacheItemWithP::getEqClass(const DataList &inp) {
    std::unordered_map<std::string, std::vector<int>> class_map;
    for (int i = 0; i < terminate_list.size(); ++i) {
        class_map[selector::random::getTerminateOutput(*terminate_list[i], inp).toString()].push_back(i);
    }
    return class_map;
}
TripleCacheItemWithP::TripleCacheItemWithP(TripleCacheItem *item, int node_id, int _p_id, const std::vector<RandomSemanticsModel *> &_model_list):
    p_id(_p_id), model_list(_model_list), match_storage(item->match_storage), node(item->node), terminate_list(item->terminate_list),
    example_num(item->example_num) {
    index_list.resize(terminate_list.size());
    for (int i = 0; i < item->index_list.size(); ++i) {
        if (item->index_list[i] != -1) index_list[item->index_list[i]] = i;
    }
    p_weight_list.resize(index_list.size(), nullptr);
    assert(example_num == model_list.size());
    //std::cout << node_id << " " << p_id << " " << example_num << " " << index_list.size() << " " << std::endl;
    for (int t_id = 0; t_id < index_list.size(); ++t_id) {
        int edge_id = index_list[t_id]; //std::cout << edge_id << " " << terminate_list[t_id] << std::endl;
        std::vector<RandomSemanticsScore> weight_list(example_num);
        for (int i = 0; i < example_num; ++i) {
            weight_list[i] = edge_id == p_id ? 1.0 : model_list[i]->weight_list[node_id][edge_id][p_id];
        }
        p_weight_list[t_id] = new RandomSemanticsScore[1 << example_num];
        selector::random::buildSetProduct(p_weight_list[t_id], weight_list);
        for (int i = 0; i < (1 << example_num); ++i) {
            p_weight_list[t_id][i] *= terminate_list[t_id]->weight;
        }
        //std::cout << terminate_list[t_id]->semantics->getName() << ": ";
        // for (int i = 0; i < (1 << example_num); ++i) std::cout << p_weight_list[t_id][i] << " "; std::cout << std::endl;
    }
    int n = example_num * 3;
    two2eight.resize(1 << example_num); two2eight[0] = 0;
    for (int i = 1; i < (1 << example_num); ++i) two2eight[i] = (two2eight[i >> 1] << 3) + (i & 1);
    full_res = new RandomSemanticsScore[1 << n];
    //std::cout << "f size " << (1 << n) << std::endl;
    for (int i = 0; i < (1 << n); ++i) full_res[i] = 0.0;
    for (int i = 0; i < match_storage.size(); ++i) {
        for (int j = 0; j < match_storage.size(); ++j) {
            updateProduct(full_res, i, j, 0);
        }
    }
}
void TripleCacheItemWithP::updateProduct(RandomSemanticsScore *x, int i, int j, int bias) {
    bias += two2eight[match_storage[i][j]];
    auto* y = p_weight_list[i], *z = p_weight_list[j];
    for (int a = 0; a < (1 << example_num); ++a)
        for (int b = 0; b < (1 << example_num); ++b) {
            // std::cout << "pos " << bias << " " << a << " " << b << " " << example_num << " " << bias + (two2eight[a] << 1) + (two2eight[b] << 2) << std::endl;
            x[bias + (two2eight[a] << 1) + (two2eight[b] << 2)] += y[a] * z[b];
        }
}
void TripleCacheItemWithP::limitedSuffixSum(RandomSemanticsScore *x, int n) {
    for (int i = 0, k = 0; k < example_num; i += 3, ++k) {
        assert(i < n);
        for (int j = 0; j < (1 << n); ++j)
            if ((j & (1 << i)) == 0) x[j] += x[j + (1 << i)];
    }
}
void TripleCacheItemWithP::getFullResult(const DataList &inp, RandomSemanticsScore *res) {
    int n = example_num * 3 + 1;
    for (int i = 0; i < (1 << n - 1); ++i) res[i] = full_res[i];
    for (int i = (1 << n - 1); i < (1 << n); ++i) res[i] = 0.0;
    auto m = getEqClass(inp);
    for (auto& info: m) {
        for (int x_id: info.second)
            for (int y_id: info.second) {
                updateProduct(res, x_id, y_id, 1 << (n - 1));
            }
    }
    limitedSuffixSum(res, n);
}
TripleCacheItemWithP::~TripleCacheItemWithP() {
    delete[] full_res;
    for (auto* x: p_weight_list) delete[] x;
}
TripleCache::TripleCache(TopDownContextGraph *_graph): graph(_graph) {
    for (auto& node: graph->node_list) {
        item_list.push_back(new TripleCacheItem(&node));
    }
}
void TripleCache::_buildP(int node_id, TopDownGraphMatchStructure *p, const std::vector<RandomSemanticsModel*>& model_list) {
    auto feature = std::to_string(node_id) + "@" + p->program->toString();
    if (p_map.find(feature) != p_map.end()) return;
    // std::cout << "build for " << feature << std::endl;
    p_map[feature] = new TripleCacheItemWithP(item_list[node_id], node_id, p->edge_id, model_list);
    auto& edge = graph->node_list[node_id].edge_list[p->edge_id];
    for (int i = 0; i < edge.v_list.size(); ++i) _buildP(edge.v_list[i], p->sub_list[i], model_list);
}
void TripleCache::setP(TopDownGraphMatchStructure *p, const std::vector<RandomSemanticsModel*>& model_list) {
    for (auto& info: p_map) delete info.second;
    p_map.clear(); _buildP(0, p, model_list);
}
void TripleCache::pushExample(const DataList &inp) {
    for (auto* item: item_list) item->pushExample(inp);
}
void TripleCache::popExample() {
    for (auto* item: item_list) item->popExample();
}
TripleCache::~TripleCache() {
    for (auto& info: p_map) delete info.second;
    for (auto* item: item_list) delete item;
}
void TripleCache::getPairResult(int node_id, const DataList &inp, RandomSemanticsScore *res) {
    item_list[node_id]->getFullResult(inp, res);
}
void TripleCache::getTripleResult(int node_id, TopDownGraphMatchStructure *match, const DataList &inp, RandomSemanticsScore *res) {
    auto feature = std::to_string(node_id) + "@" + match->program->toString();
    assert(p_map.count(feature));
    p_map[feature]->getFullResult(inp, res);
}

namespace {
    void _clear(RandomSemanticsScore* x, int n, RandomSemanticsScore w = 0.0) {
        for (int i = 0; i < n; ++i) x[i] = w;
    }
}

class OptimizedTripleDPHolder {
public:
    std::vector<RandomSemanticsModel*> model_list;
    TopDownContextGraph* graph;
    DataList inp;
    RandomSemanticsModel* last_model;
    std::vector<RandomSemanticsScore*> pair_cache;
    std::unordered_map<std::string, RandomSemanticsScore*> triple_cache;
    int n, m_num;
    TripleCache* cache_pool;
    std::vector<int> two2eight, two2four;

    OptimizedTripleDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list,
            const DataList& _inp, TripleCache* _cache_pool): graph(_graph), model_list(_model_list),
            n(_model_list.size() * 3 - 2), m_num(_model_list.size()), cache_pool(_cache_pool), inp(_inp) {
        last_model = model_list[model_list.size() - 1];
        pair_cache.resize(graph->node_list.size(), nullptr);
        two2eight.resize(1 << m_num, 0);
        two2four.resize(1 << m_num, 0);
        for (int i = 1; i < (1 << m_num); ++i) {
            two2four[i] = (two2four[i >> 1] << 2) + (i & 1);
            two2eight[i] = (two2eight[i >> 1] << 3) + (i & 1);
        }
    }
    ~OptimizedTripleDPHolder() {
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
            cache_pool->getPairResult(node_id, inp, tmp);
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
        for (int i = 0; i < n; ++i) weight_list[i] = model_list[i / 3]->weight_list[node_id][edge_id][edge_id];
        selector::random::updateViolateProb(res, weight_list);
    }

    void getCrossResult(int node_id, int x_id, int y_id, int num, RandomSemanticsScore* res) {
        std::vector<RandomSemanticsScore> weight_list(num);
        for (int i = 0; i < num; ++i) weight_list[i] = model_list[i]->weight_list[node_id][x_id][y_id];
        selector::random::buildSetProduct(res, weight_list);
    }

    void addProduct(int n, int m, RandomSemanticsScore* x, RandomSemanticsScore* y, RandomSemanticsScore weight, RandomSemanticsScore* res) {
        for (int j = 0; j < (1 << m); ++j) {
            int base = two2four[j] << 1; auto w = weight * y[j];
            for (int i = 0; i < (1 << n); ++i) {
                res[base + two2four[i]] += w * x[i];
            }
        }
    }

    void addABandC(RandomSemanticsScore* AB, RandomSemanticsScore* C, RandomSemanticsScore* res) {
        for (int a = 0; a < (1 << m_num); ++a) {
            for (int b = 0; b < (1 << m_num - 1); ++b) {
                int base = two2eight[a] + (two2eight[b] << 1);
                auto w = AB[two2four[a] + (two2four[b] << 1)];
                for (int c = 0; c < (1 << m_num - 1); ++c) {
                    res[base + (two2eight[c] << 2)] += w * C[c];
                }
            }
        }
    }
    void addACandB(RandomSemanticsScore* AC, RandomSemanticsScore* B, RandomSemanticsScore* res) {
        for (int a = 0; a < (1 << m_num); ++a) {
            for (int c = 0; c < (1 << m_num - 1); ++c) {
                int base = two2eight[a] + (two2eight[c] << 2);
                auto w = AC[two2four[a] + (two2four[c] << 1)];
                for (int b = 0; b < (1 << m_num - 1); ++b) {
                    res[base + (two2eight[b] << 1)] += w * B[b];
                }
            }
        }
    }

    RandomSemanticsScore* getTripleScore(int node_id, TopDownGraphMatchStructure* match) {
        auto feature = std::to_string(node_id) + "@" + match->program->toString();
        // std::cout << feature << std::endl;
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
        auto* p_pair_res = new RandomSemanticsScore[1 << m_num - 1];
        for (int i = 0; i < (1 << m_num - 1); ++i) {
            p_pair_res[i] = sub_res[two2eight[i] << 1];
        }

        { // both x y are terminate
            cache_pool->getTripleResult(node_id, match, inp, cache);
        }

        { // not all terminate
            int t = (m_num << 1) + 1;
            auto* tmp = new RandomSemanticsScore[1 << t];
            auto* xy = new RandomSemanticsScore[1 << m_num];
            auto* xp = new RandomSemanticsScore[1 << m_num];
            auto* yp = new RandomSemanticsScore[1 << m_num];
            // y id not terminate
            for (int y_id: nt_list) {
                for (int i = 0; i < (1 << t); ++i) tmp[i] = 0.0;
                for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
                    if (x_id == y_id) {
                        if (x_id == p_edge_id) {
                            auto w = p_edge.weight * p_edge.weight;
                            for (int i = 0; i < (1 << n); ++i) cache[i] += sub_res[i] * w;
                            //std::cout << "all equal " << sub_res[(1 << n) - 1] << " " << w << std::endl;
                        } else {
                            getPairResult(node_id, x_id, xy);
                            getCrossResult(node_id, x_id, p_edge_id, m_num - 1, xp);
                            addProduct(m_num, m_num - 1, xy, xp, node.edge_list[y_id].weight, tmp);
                        }
                    } else {
                        getCrossResult(node_id, x_id, y_id, m_num, xy);
                        if (x_id == p_edge_id) {
                            addProduct(m_num, m_num - 1, xy, p_pair_res, node.edge_list[x_id].weight, tmp);
                        } else {
                            getCrossResult(node_id, x_id, p_edge_id, m_num - 1, xp);
                            addProduct(m_num, m_num - 1, xy, xp, node.edge_list[x_id].weight, tmp);
                        }
                    }
                }
                for (int i = 0; i < (1 << t); ++i) tmp[i] *= node.edge_list[y_id].weight;
                if (y_id == p_edge_id) addABandC(tmp, p_pair_res, cache);
                else {
                    getCrossResult(node_id, y_id, p_edge_id, m_num - 1, yp);
                    addABandC(tmp, yp, cache);
                }
            }
            // x is not terminate but y is
            for (int x_id: nt_list) {
                for (int i = 0; i < (1 << t); ++i) tmp[i] = 0.0;
                for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                    if (!selector::random::isTerminate(node.edge_list[y_id])) continue;
                    getCrossResult(node_id, x_id, y_id, m_num, xy);
                    if (y_id == p_edge_id) {
                        addProduct(m_num, m_num - 1, xy, p_pair_res, node.edge_list[y_id].weight, tmp);
                    } else {
                        getCrossResult(node_id, y_id, p_edge_id, m_num - 1, yp);
                        addProduct(m_num, m_num - 1, xy, yp, node.edge_list[y_id].weight, tmp);
                    }
                }
                for (int i = 0; i < (1 << t); ++i) tmp[i] *= node.edge_list[x_id].weight;
                if (x_id == p_edge_id) addACandB(tmp, p_pair_res, cache);
                else {
                    getCrossResult(node_id, x_id, p_edge_id, m_num - 1, xp);
                    addACandB(tmp, xp, cache);
                }
            }
            delete[] tmp; delete[] xy; delete[] xp; delete[] yp;
        }
        delete[] sub_res;
        return cache;
    }

    RandomSemanticsScore getScore(TopDownGraphMatchStructure* match) {
        // std::cout << "start" << std::endl;
        auto* res = getTripleScore(0, match);
        return res[(1 << n) - 1];
        //auto* res = getPairScore(0);
        //return res[(1 << m_num) - 1];
    }
};

OptimizedTripleLearnedScorer::OptimizedTripleLearnedScorer(Env *env, FlattenGrammar *fg, RandomSemanticsLearner *_learner):
        LearnedScorer(env, fg, _learner), cache_pool(new TripleCache(fg->graph)) {
}
void OptimizedTripleLearnedScorer::pushExample(const Example &inp) {
    LearnedScorer::pushExample(inp); cache_pool->pushExample(fg->getFlattenInput(inp));
    delete match; match = nullptr;
}
void OptimizedTripleLearnedScorer::popExample() {
    LearnedScorer::popExample(); cache_pool->popExample();
    delete match; match = nullptr;
}
OptimizedTripleLearnedScorer::~OptimizedTripleLearnedScorer() {
    delete cache_pool;
}
void OptimizedTripleLearnedScorer::getMatch(const PProgram &p) {
    if (match && match->program->toString() == p->toString()) return;
    delete match; match = fg->getMatchStructure(p);
    cache_pool->setP(match, model_list);
}
RandomSemanticsScore OptimizedTripleLearnedScorer::getScore(const PProgram &p, const Example &inp) {
    getMatch(p);
    auto* model = learnModel(inp); model_list.push_back(model);
    auto* holder = new OptimizedTripleDPHolder(fg->graph, model_list, fg->getFlattenInput(inp), cache_pool);
    auto res = holder->getScore(match);
    delete holder; model_list.pop_back();
    return res;
}