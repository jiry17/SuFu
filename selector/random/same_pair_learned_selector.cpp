//
// Created by pro on 2022/6/7.
//

#include "istool/selector/random/learned_scorer.h"
#include "glog/logging.h"
#include "istool/basic/config.h"

LearnCacheItem::LearnCacheItem(const std::vector<RandomSemanticsScore *> &_score_list): score_list(_score_list) {
}
LearnCacheItem::~LearnCacheItem() {
    for (auto* list: score_list) delete[] list;
}

LearnedScorer::LearnedScorer(Env *_env, FlattenGrammar *_fg, RandomSemanticsLearner *_learner):
    env(_env), fg(_fg), learner(_learner) {
}
void LearnedScorer::pushExample(const Example &inp) {
    model_list.push_back(learner->learn(inp));
}
void LearnedScorer::popExample() {
    for (int i = 1; i < model_list.size(); ++i) model_list[i - 1] = model_list[i];
    model_list.pop_back();
}
RandomSemanticsModel * LearnedScorer::learnModel(const Example &inp) {
    auto feature = data::dataList2String(inp);
    if (model_cache.count(feature)) return model_cache[feature];
    return model_cache[feature] = learner->learn(inp);
}
LearnedScorer::~LearnedScorer() noexcept {
    delete learner;
    for (auto& info: model_cache) delete info.second;
}

void SamePairLearnedScorer::clearCache() {
    for (auto& info: pair_info_cache) delete info.second;
    pair_info_cache.clear();
    current_program = {};
}
void SamePairLearnedScorer::buildCache(int node_id, TopDownGraphMatchStructure *p) {
    std::string feature = std::to_string(node_id) + "@" + p->program->toString();
    if (pair_info_cache.count(feature)) return;
    auto& node = fg->graph->node_list[node_id];
    int p_edge_id = p->edge_id; auto& p_edge = fg->graph->node_list[node_id].edge_list[p_edge_id];
    for (int i = 0; i < p_edge.v_list.size(); ++i) {
        buildCache(p_edge.v_list[i], p->sub_list[i]);
    }

    std::vector<RandomSemanticsScore*> score_list;
    int n = model_list.size();
    for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
        auto* res = new RandomSemanticsScore[1 << n];
        res[0] = 1.0;
        if (edge_id != p_edge_id) {
            for (int i = 0; i < n; ++i) {
                res[1 << i] = model_list[i]->weight_list[node_id][p_edge_id][edge_id];
            }
        } else for (int i = 0; i < n; ++i) res[1 << i] = 1.0;
        for (int S = 1; S < (1 << n); ++S) {
            int T = S & (-S); res[S] = res[T] * res[S - T];
        }
        for (int i = 0; i < (1 << n); ++i) res[i] *= node.edge_list[edge_id].weight;
        score_list.push_back(res);
    }
    pair_info_cache[feature] = new LearnCacheItem(score_list);
}
SamePairLearnedScorer::~SamePairLearnedScorer() noexcept {
    clearCache();
    for (auto& info: model_cache) delete info.second;
    delete learner;
}
void SamePairLearnedScorer::initCache(const PProgram& p) {
    if (current_program && current_program->toString() == p->toString()) return;
    clearCache();
    current_program = p;
    auto* match = fg->getMatchStructure(p);
    buildCache(0, match);
}

void selector::random::updateViolateProb(RandomSemanticsScore *res, const std::vector<RandomSemanticsScore> &weight_list) {
    int n = weight_list.size();
    for (int i = 0; i < n; ++i) {
        for (int S = (1 << n) - 1; S >= 0; --S) {
            if (S & (1 << i)) {
                res[S] = res[S] * (1 - weight_list[i]) + res[S - (1 << i)] * weight_list[i];
            }
        }
    }
}
void selector::random::buildSetProductWithMask(RandomSemanticsScore *res, int n, int mask) {
    for (int S = 0; S < n; ++S) {
        int T = S & (mask);
        if (T == 0) {
            T = S & (-S); res[S] = res[T] * res[S - T];
        } else if (T != S) res[S] = res[T] * res[S - T];
    }
}

namespace {
    class _OptimizedLearnedDPHolder {
    public:
        TopDownContextGraph* graph;
        Example inp;
        int n, m;
        std::vector<RandomSemanticsModel*> model_list;
        TopDownGraphMatchStructure* p_match;
        RandomSemanticsModel* last_model;
        std::unordered_map<std::string, RandomSemanticsScore*> cache_map;
        std::vector<RandomSemanticsScore> cache_list;
        std::unordered_map<std::string, LearnCacheItem*> cache_pool;
        std::vector<int> four_weight;

        _OptimizedLearnedDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list, const Example& _inp,
                              TopDownGraphMatchStructure* _p_match, const std::unordered_map<std::string, LearnCacheItem*>& _cache_pool):
                graph(_graph), inp(_inp), model_list(_model_list), n((_model_list.size() << 1) - 1), cache_pool(_cache_pool),
                p_match(_p_match) {
            cache_list.resize(graph->node_list.size());
            for (int i = 0; i < cache_list.size(); ++i) cache_list[i] = -1;
            last_model = model_list[model_list.size() - 1];
            m = model_list.size() - 1; four_weight.resize(1 << m);
            four_weight[0] = 0;
            for (int i = 1; i < (1 << m); ++i) four_weight[i] = (four_weight[i >> 1] << 2) + (i & 1);
            // last_model->print();
        }
        ~_OptimizedLearnedDPHolder() {
            for (auto& info: cache_map) delete[] info.second;
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

        void updateSquare(RandomSemanticsScore* x, RandomSemanticsScore * y, RandomSemanticsScore* res, int bias, RandomSemanticsScore weight = 1.0) {
            auto* tmp = new RandomSemanticsScore[1 << m];
            for (int i = 0; i < (1 << m); ++i) tmp[i] = x[i] * weight;
            for (int i = 0; i < (1 << m); ++i)
                for (int j = 0; j < (1 << m); ++j) {
                    res[four_weight[i] + (four_weight[j] << 1) + bias] += tmp[i] * y[j];
                }
            delete[] tmp;
        }

        RandomSemanticsScore* solve(int node_id, TopDownGraphMatchStructure* match) {
            auto feature = std::to_string(node_id) + "@" + match->program->toString();
            if (cache_map.count(feature)) return cache_map[feature];
            auto* res = new RandomSemanticsScore[(1 << n)]; cache_map[feature] = res;
            auto* pair_info = cache_pool[feature];
            for (int i = 0; i < (1 << n); ++i) res[i] = 0.0;
            int p_edge_id = match->edge_id; auto& node = graph->node_list[node_id];
            auto& p_edge = node.edge_list[p_edge_id];
            auto& weight_matrix = last_model->weight_list[node_id];

            /*LOG(INFO) << feature;
            for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                std::cout << node.edge_list[edge_id].semantics->getName();
                for (int i = 0; i < (1 << m); ++i) std::cout << " " << pair_info->score_list[edge_id][i];
                std::cout << std::endl;
            }*/

            auto* sub_res = new RandomSemanticsScore[1 << n];
            for (int i = 0; i < (1 << n); ++i) sub_res[i] = 1.0;
            for (int i = 0; i < p_edge.v_list.size(); ++i) {
                auto *sub = solve(p_edge.v_list[i], match->sub_list[i]);
                for (int j = 0; j < (1 << n); ++j) sub_res[j] *= sub[j];
            }
            std::vector<RandomSemanticsScore> weight_list(n);
            for (int i = 0; i < n; ++i) weight_list[i] = model_list[i >> 1]->weight_list[node_id][p_edge_id][p_edge_id];
            selector::random::updateViolateProb(sub_res, weight_list);

            std::unordered_map<std::string, std::pair<int, RandomSemanticsScore*>> oup_class_res;
            auto* t_sum = new RandomSemanticsScore[1 << m];
            for (int i = 0; i < (1 << m); ++i) t_sum[i] = 0.0;
            auto* pair_equal = new RandomSemanticsScore[1 << m];
            for (int i = 0; i < (1 << m); ++i) pair_equal[i] = sub_res[four_weight[i]] * p_edge.weight;
            for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                if (edge_id != p_edge_id) {
                    for (int i = 0; i < (1 << m); ++i) t_sum[i] += pair_info->score_list[edge_id][i];
                } else {
                    for (int i = 0; i < (1 << m); ++i) t_sum[i] += pair_equal[i];
                }
            }
            updateSquare(t_sum, t_sum, res, 0);
            delete[] t_sum;

            { // case 1: x y both terminate
                for (int i = 0; i < node.edge_list.size(); ++i) {
                    if (!isTerminateSemantics(node.edge_list[i].semantics.get())) continue;
                    auto oup = getTerminateOutput(node.edge_list[i].semantics.get()).toString();
                    if (oup_class_res.count(oup) == 0) {
                        auto* now = new RandomSemanticsScore[1 << m];
                        for (int j = 0; j < (1 << m); ++j) now[j] = 0.0;
                        oup_class_res[oup] = {i, now};
                    }
                    auto* now = oup_class_res[oup].second;
                    for (int j = 0; j < (1 << m); ++j) {
                        now[j] += pair_info->score_list[i][j];
                    }
                }
                for (auto& class_info: oup_class_res) {
                    //std::cout << "class "; for (int i = 0; i < (1 << m); ++i) std::cout << class_info.second.second[i] << " "; std::cout << std::endl;
                    updateSquare(class_info.second.second, class_info.second.second, res, (1 << n - 1));
                }
            }
            //std::cout << "part1: ";
            //for (int i = 0; i < (1 << n); ++i) std::cout << res[i] << " "; std::cout << std::endl;


            { // case 2: exact one terminate
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    if (isTerminateSemantics(node.edge_list[edge_id].semantics.get())) continue;
                    RandomSemanticsScore* x = (edge_id == p_edge_id) ? pair_equal : pair_info->score_list[edge_id];
                    for (auto& class_info: oup_class_res) {
                        auto last_weight = weight_matrix[edge_id][class_info.second.first];
                        updateSquare(class_info.second.second, x, res, (1 << n - 1), last_weight);
                        updateSquare(x, class_info.second.second, res, (1 << n - 1), last_weight);
                    }
                }
            }
            //std::cout << "part2: ";
            //for (int i = 0; i < (1 << n); ++i) std::cout << res[i] << " "; std::cout << std::endl;

            { // case 3: both x and y are non-terminate
                std::vector<int> nt_list;
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    if (!isTerminateSemantics(node.edge_list[edge_id].semantics.get())) {
                        nt_list.push_back(edge_id);
                    }
                }
                //std::cout << "sub res: ";
                //for (int i = 0; i < (1 << n); ++i) std::cout << sub_res[i] << " "; std::cout << std::endl;
                for (auto x_id: nt_list) {
                    RandomSemanticsScore *x = (x_id == p_edge_id) ? pair_equal : pair_info->score_list[x_id];
                    for (auto y_id: nt_list) {
                        RandomSemanticsScore *y = (y_id == p_edge_id) ? pair_equal : pair_info->score_list[y_id];
                        if (x_id == y_id) {
                            if (x_id == p_edge_id) {
                                auto factor = p_edge.weight * p_edge.weight;
                                for (int i = (1 << n - 1); i < (1 << n); ++i) res[i] += sub_res[i] * factor;
                            } else updateSquare(x, y, res, (1 << n - 1), getLastMatch(node_id, x_id));
                        } else updateSquare(x, y, res, (1 << n - 1), weight_matrix[x_id][y_id]);
                    }
                }
            }
            //std::cout << "part3: ";
            //for (int i = 0; i < (1 << n); ++i) std::cout << res[i] << " "; std::cout << std::endl;
            delete[] sub_res; delete[] pair_equal;
            for (auto& info: oup_class_res) delete[] info.second.second;

            return res;
        }

        double getTripleScore() {
            auto* w = solve(0, p_match);
            return w[(1 << n) - 1];
        }
    };
}

SamePairLearnedScorer::SamePairLearnedScorer(Env *env, FlattenGrammar *_fg, RandomSemanticsLearner *_learner):
    LearnedScorer(env, _fg, _learner) {
}
void SamePairLearnedScorer::pushExample(const Example &inp) {
    LearnedScorer::pushExample(inp);
    clearCache();
}
void SamePairLearnedScorer::popExample() {
    LearnedScorer::popExample();
    clearCache();
}
RandomSemanticsScore SamePairLearnedScorer::getScore(const PProgram &p, const Example &inp) {
    global::recorder.start("score");
    initCache(p);
    auto* new_model = learnModel(inp);
    model_list.push_back(new_model);
    auto* match = fg->getMatchStructure(p);
    auto* holder = new _OptimizedLearnedDPHolder(fg->graph, model_list, fg->getFlattenInput(inp), match, pair_info_cache);
    auto res = holder->getTripleScore();
    delete match; delete holder;
    model_list.pop_back();
    global::recorder.end("score");
    return res;
}