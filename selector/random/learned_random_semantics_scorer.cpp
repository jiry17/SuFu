//
// Created by pro on 2022/6/4.
//

#include "istool/selector/random/random_semantics_scorer.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

using namespace selector::random;

LearnedCachedSemanticsScorer::LearnedCachedSemanticsScorer(Env *_env, FlattenGrammar *_fg, RandomSemanticsLearner* _learner):
        IncrementalRandomSemanticsScorer(_env, _fg), learner(_learner) {
}
void LearnedCachedSemanticsScorer::pushExample(const Example &inp) {
    IncrementalRandomSemanticsScorer::pushExample(inp);
    model_list.push_back(getModel(inp));
}
void LearnedCachedSemanticsScorer::popExample() {
    IncrementalRandomSemanticsScorer::popExample();
    for (int i = 1; i < model_list.size(); ++i) model_list[i - 1] = model_list[i];
    model_list.pop_back();
}
RandomSemanticsModel * LearnedCachedSemanticsScorer::getModel(const DataList &inp) {
    auto feature = data::dataList2String(inp);
    if (model_cache.count(feature)) {
        return model_cache[feature];
    }
    return model_cache[feature] = learner->learn(inp);
}
using namespace selector::random;

namespace {
    bool _isEmptyCache(RandomSemanticsScore* A) {
        return A[0] < -0.5;
    }
    std::string _twoState2String(int state, int n) {
        std::string res;
        for (int i = 0; i < n; ++i) {
            res += std::to_string(state % 2); state >>= 1;
        }
        return res;
    }
    void updateViolateProb(RandomSemanticsScore* res, const std::vector<RandomSemanticsScore>& weight_list, int n) {
        for (int i = 0; i < n; ++i) {
            for (int S = (1 << n) - 1; S >= 0; --S) {
                if (S & (1 << i)) {
                    res[S] = res[S] * (1 - weight_list[i]) + res[S - (1 << i)] * weight_list[i];
                }
            }
        }
    }

    /*class _LearnedDPHolder {
    public:
        TopDownContextGraph* graph;
        Example inp;
        int n;
        std::vector<RandomSemanticsModel*> model_list;

        std::vector<RandomSemanticsScore*> cache_list;
        CachePool* match_info_cache;

        _LearnedDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list, const Example& _inp, CachePool* _match_info_cache):
                graph(_graph), inp(_inp), model_list(_model_list), n(_model_list.size()), match_info_cache(_match_info_cache) {
            cache_list.resize(graph->node_list.size());
            for (int i = 0; i < cache_list.size(); ++i) {
                cache_list[i] = new RandomSemanticsScore[1 << n];
                for (int j = 0; j < (1 << n); ++j) cache_list[i][j] = -1;
            }
        }
        ~_LearnedDPHolder() {
            for (auto* array: cache_list) delete[] array;
        }

        bool isTerminateSemantics(Semantics* sem) {
            return dynamic_cast<ConstSemantics*>(sem) || dynamic_cast<ParamSemantics*>(sem);
        }
        void getTwoSubRes(const std::vector<int>& v_list, RandomSemanticsScore* res) {
            for (int i = 0; i < (1 << n); ++i) res[i] = 1.0;
            for (auto& v: v_list) {
                auto* sub_res = solveFGMatch(v);
                for (int i = 0; i < (1 << n); ++i) res[i] *= sub_res[i];
            }
        }
        RandomSemanticsScore* solveFGMatch(int node_id) {
            auto* cache = cache_list[node_id];
            auto& node = graph->node_list[node_id];
            if (!_isEmptyCache(cache)) return cache;
            for (int i = 0; i < (1 << n); ++i) cache[i] = 0;
            // LOG(INFO) << "solve match " << node_id << std::endl;
            auto* tmp = new RandomSemanticsScore[1 << n];
            { // solve terminal case
                match_info_cache->getPairMatchRes(node_id, inp, tmp);
                for (int i = 1; i <= n; ++i) {
                    for (int S = 0; S < (1 << n); ++S)
                        if (S & (1 << i - 1)) tmp[S - (1 << i - 1)] += tmp[S];
                }
                for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i];
            }
            {
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    auto& edge = node.edge_list[edge_id];
                    if (isTerminateSemantics(edge.semantics.get())) continue;
                    auto weight = edge.weight * edge.weight;
                    getTwoSubRes(edge.v_list, tmp);
                    std::vector<RandomSemanticsScore> weight_list(n);
                    for (int i = 0; i < n; ++i) weight_list[i] = model_list[i]->weight_list[node_id][edge_id][edge_id];
                    updateViolateProb(tmp, weight_list, n);
                    for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i] * weight;
                }
                // LOG(INFO) << "Matched case " << node_id << " " << cache[(1 << n) - 1];
            }
            { // solve unmatched case
                for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
                    auto& x = node.edge_list[x_id];
                    if (isTerminateSemantics(x.semantics.get())) continue;
                    for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                        auto& y = node.edge_list[y_id];
                        if (!isTerminateSemantics(y.semantics.get()) && x_id <= y_id) continue;
                        for (int i = 0; i < (1 << n); ++i) tmp[i] = 0.0;
                        tmp[0] = 1;
                        for (int i = 0; i < n; ++i) tmp[1 << i] = model_list[i]->weight_list[node_id][x_id][y_id];
                        for (int S = 2; S < (1 << n); ++S) {
                            int T = S & (-S);
                            if (S != T) tmp[S] = tmp[T] * tmp[S - T];
                        }
                        auto weight = x.weight * y.weight * 2;
                        for (int i = 0; i < (1 << n); ++i) cache[i] += weight * tmp[i];
                    }
                }
            }
            delete tmp;
            return cache;
        }

        double _getPairScore() {
            auto* w = solveFGMatch(0);
            return w[(1 << n) - 1];
        }
    };*/



    class _LearnedDPHolderWithP {
    public:
        TopDownContextGraph* graph;
        Example inp;
        int n;
        std::vector<RandomSemanticsModel*> model_list;
        TopDownGraphMatchStructure* p_match;
        RandomSemanticsModel* last_model;
        std::unordered_map<std::string, RandomSemanticsScore*> cache_map;
        std::vector<RandomSemanticsScore> cache_list;

        _LearnedDPHolderWithP(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list, const Example& _inp,
                TopDownGraphMatchStructure* _p_match):
                graph(_graph), inp(_inp), model_list(_model_list), n((_model_list.size() << 1) - 1),
                p_match(_p_match) {
            cache_list.resize(graph->node_list.size());
            for (int i = 0; i < cache_list.size(); ++i) cache_list[i] = -1;
            last_model = model_list[model_list.size() - 1];
            // last_model->print();
        }
        ~_LearnedDPHolderWithP() {
            for (auto& info: cache_map) delete info.second;
        }

        bool isTerminateSemantics(Semantics* sem) {
            return dynamic_cast<ConstSemantics*>(sem) || dynamic_cast<ParamSemantics*>(sem);
        }
        Data getTerminateOutput(Semantics* sem) {
            auto* ps = dynamic_cast<ParamSemantics*>(sem);
            if (ps) return inp[ps->id];
            auto* cs = dynamic_cast<ConstSemantics*>(sem);
            if (cs) return cs->w;
            LOG(FATAL) << "Semantics " << sem->getName() << " is not terminate";
        }
        RandomSemanticsScore getLastMatch(int node_id) {
            if (cache_list[node_id] > -0.5) return cache_list[node_id];
            auto& weight_matrix = model_list[model_list.size() - 1]->weight_list[node_id];
            auto& node = graph->node_list[node_id];
            RandomSemanticsScore res = 0.0;
            { // solve terminal case
                std::unordered_map<std::string, RandomSemanticsScore> value_map;
                for (auto& edge: node.edge_list) {
                    if (isTerminateSemantics(edge.semantics.get())) {
                        value_map[getTerminateOutput(edge.semantics.get()).toString()] += edge.weight;
                    }
                }
                for (auto& value: value_map) res += value.second * value.second;
            }
            {
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    auto& edge = node.edge_list[edge_id];
                    if (isTerminateSemantics(edge.semantics.get())) continue;
                    res += edge.weight * edge.weight * getLastMatch(node_id, edge_id);
                }
            }
            { // solve unmatched case
                for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
                    auto& x = node.edge_list[x_id];
                    if (isTerminateSemantics(x.semantics.get())) continue;
                    for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                        auto& y = node.edge_list[y_id];
                        if (!isTerminateSemantics(y.semantics.get()) && x_id <= y_id) continue;
                        res += weight_matrix[x_id][y_id] * 2 * node.edge_list[x_id].weight * node.edge_list[y_id].weight;
                    }
                }
            }
            // std::cout << "last match " << node_id << " " << res << std::endl;
            return cache_list[node_id] = res;
        }

        RandomSemanticsScore getLastMatch(int node_id, int edge_id) {
            auto& edge = graph->node_list[node_id].edge_list[edge_id];
            RandomSemanticsScore sub_equal = 1.0;
            for (auto v: edge.v_list) sub_equal *= getLastMatch(v);
            return (sub_equal + (1 - sub_equal) * last_model->weight_list[node_id][edge_id][edge_id]);
        }

        RandomSemanticsScore* solve(int node_id, TopDownGraphMatchStructure* match) {
            auto feature = std::to_string(node_id) + "@" + match->program->toString();
            if (cache_map.count(feature)) return cache_map[feature];
            auto* res = new RandomSemanticsScore[(1 << n)]; cache_map[feature] = res;
            for (int i = 0; i < (1 << n); ++i) res[i] = 0.0;
            int p_edge_id = match->edge_id; auto& node = graph->node_list[node_id];
            auto& p_edge = node.edge_list[p_edge_id];

            auto* sub_res = new RandomSemanticsScore[1 << n];
            for (int i = 0; i < (1 << n); ++i) sub_res[i] = 1.0;
            for (int i = 0; i < p_edge.v_list.size(); ++i) {
                auto *sub = solve(p_edge.v_list[i], match->sub_list[i]);
                for (int j = 0; j < (1 << n); ++j) sub_res[j] *= sub[j];
            }
            std::vector<RandomSemanticsScore> weight_list(n);
            for (int i = 0; i < n; ++i) weight_list[i] = model_list[i >> 1]->weight_list[node_id][p_edge_id][p_edge_id];
            updateViolateProb(sub_res, weight_list, n);
            auto* tmp = new RandomSemanticsScore[1 << n];

            for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
                for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                    for (int i = 0; i < (1 << n); ++i) tmp[i] = 0.0;
                    auto factor = 1.0 * node.edge_list[x_id].weight * node.edge_list[y_id].weight;
                    if (x_id != p_edge_id && y_id != p_edge_id) {
                        tmp[0] = 1.0;
                        for (int i = 0; i + 1 < model_list.size(); ++i) {
                            tmp[(1 << i * 2)] = model_list[i]->weight_list[node_id][p_edge_id][x_id];
                            tmp[(1 << i * 2 + 1)] = model_list[i]->weight_list[node_id][p_edge_id][y_id];
                        }
                        if (x_id != y_id) tmp[(1 << n - 1)] = last_model->weight_list[node_id][x_id][y_id];
                        else tmp[(1 << n - 1)] = getLastMatch(node_id, x_id);
                        for (int S = 1; S < (1 << n); ++S) {
                            int T = S & (-S); tmp[S] = tmp[S - T] * tmp[T];
                        }
                    } else if (x_id == p_edge_id && y_id == p_edge_id) {
                        for (int i = 0; i < (1 << n); ++i) tmp[i] = sub_res[i];
                    } else {
                        int mask = 0, sign = (x_id == p_edge_id), other = x_id + y_id - p_edge_id;
                        for (int i = 0; i + 1 < model_list.size(); ++i) {
                            mask |= (1 << i * 2 + (sign ^ 1));
                            tmp[1 << i * 2 + sign] = model_list[i]->weight_list[node_id][p_edge_id][other];
                        }
                        tmp[(1 << n - 1)] = last_model->weight_list[node_id][x_id][y_id];
                        tmp[0] = 1.0;
                        for (int S = 1; S < (1 << n); ++S) {
                            int T = S & mask;
                            if (!T) {
                                T = S & (-S); tmp[S] = tmp[S - T] * tmp[T];
                            } else if (T == S) tmp[S] = sub_res[S];
                            else tmp[S] = tmp[T] * tmp[S - T];
                        }
                    }
                    for (int i = 0; i < (1 << n); ++i) res[i] += tmp[i] * factor;
                    //std::cout << feature << " " << x_id << " " << y_id << std::endl;
                    //for (int i = 0; i < (1 << n); ++i) std::cout << tmp[i] << " "; std::cout << std::endl;
                }
            }
            return res;
        }

        double _getTripleScore() {
            auto* w = solve(0, p_match);
            return w[(1 << n) - 1];
        }
    };
}


RandomSemanticsScore LearnedCachedSemanticsScorer::getPairScore(const Example &inp) {
    /*global::recorder.start("score");
    auto* tmp_model = getModel(inp);
    model_list.push_back(tmp_model);
    auto* holder = new _LearnedDPHolder(fg->graph, model_list, fg->getFlattenInput(inp), cache);
    auto res = holder->_getPairScore();
    delete holder; model_list.pop_back();
    global::recorder.end("score");
    return res;*/
    LOG(FATAL) << "Pair score of LearnedCachedSemanticsScorer is not implemented";
}

RandomSemanticsScore LearnedCachedSemanticsScorer::getTripleScore(const PProgram &p, const Example &inp) {
    global::recorder.start("score");
    auto* tmp_model = getModel(inp);
    model_list.push_back(tmp_model);
    auto* match = fg->getMatchStructure(p);
    auto* holder = new _LearnedDPHolderWithP(fg->graph, model_list, fg->getFlattenInput(inp), match);
    auto res = holder->_getTripleScore();
    delete holder; model_list.pop_back();
    global::recorder.end("score");
    return res;
}



LearnedCachedSemanticsScorer::~LearnedCachedSemanticsScorer() noexcept {
    for (const auto& info: model_cache) {
        delete info.second;
    }
}