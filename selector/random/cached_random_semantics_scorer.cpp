//
// Created by pro on 2022/5/19.
//

#include "istool/selector/random/random_semantics_selector.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

using namespace selector::random;

IncrementalRandomSemanticsScorer::IncrementalRandomSemanticsScorer(Env *_env, FlattenGrammar *_fg):
    env(_env), fg(_fg), example_num(0) {
}

void IncrementalRandomSemanticsScorer::pushExample(const Example &inp) {
    ++example_num;
}
void IncrementalRandomSemanticsScorer::popExample() {
    --example_num;
}
IncrementalRandomSemanticsScorer::~IncrementalRandomSemanticsScorer() {
}

BasicCachedRandomSemanticsScorer::BasicCachedRandomSemanticsScorer(Env *env, FlattenGrammar *_fg, RandomSemanticsScore _KOutputSize):
        IncrementalRandomSemanticsScorer(env, _fg), KOutputSize(_KOutputSize) {
    cache = new CachePool(_fg->graph);
}
void BasicCachedRandomSemanticsScorer::pushExample(const Example &inp) {
    cache->pushExample(inp); ++example_num;
}
void BasicCachedRandomSemanticsScorer::popExample() {
    cache->popExample(); --example_num;
}

namespace {
    const int KStateWeight = 5;
    const std::vector<std::vector<int>> KStrictSubStatusList = {
            {}, {0}, {0}, {0}, {0, 1, 2, 3}
    };
    const std::vector<std::vector<int>> KStrictSuperStateList = {
            {1, 2, 3, 4}, {4}, {4}, {4}, {}
    };
    const std::vector<int> _KFreeNum = {0, 1, 1, 1, 2};
    bool _isEmptyCache(RandomSemanticsScore* A) {
        return A[0] < -0.5;
    }
    int _getMask(int status, int mask) {
        if (status == 4) return mask;
        if (mask == 4) return status;
        if (status == mask) return status; else return 0;
    }
    std::vector<std::vector<RandomSemanticsScore>> _getViolateWeightMatrix(RandomSemanticsScore KO) {
        std::vector<std::vector<RandomSemanticsScore>> res(5, std::vector<RandomSemanticsScore>(5, -1));
        res[0][0] = 1;
        for (int i = 1; i <= 3; ++i) {
            res[i][0] = 1 / KO;
            res[i][i] = 1 - 1 / KO;
        }
        res[4][0] = 1 / KO / KO;
        for (int i = 1; i <= 3; ++i) res[4][i] = (1 - 1 / KO) / KO;
        res[4][4] = 1 - 3 / KO + 2 / KO / KO;
        return res;
    }
    std::string _twoState2String(int state, int n) {
        std::string res;
        for (int i = 0; i < n; ++i) {
            res += std::to_string(state % 2); state >>= 1;
        }
        return res;
    }
    std::string _fiveState2String(int state, int n) {
        std::string res;
        for (int i = 0; i < n; ++i) {
            res += std::to_string(state % KStateWeight); state /= KStateWeight;
        }
        return res;
    }

    class _CachedDPHolder {
    public:
        RandomSemanticsScore KOutputSize;
        TopDownContextGraph* graph;
        TopDownGraphMatchStructure* match;
        std::vector<std::vector<RandomSemanticsScore>> status_weight_matrix;
        Example inp;
        std::vector<RandomSemanticsScore> KOPowList, KNegOPowList;
        int n, five_state_num;

        static std::vector<std::pair<int, int>> masked_five_state_info_list[4];
        static std::vector<int> five_weight_list, two2five[4], five_state_eq_num_list;
        std::vector<RandomSemanticsScore*> cache_list;
        CachePool* match_info_cache;
        std::unordered_map<std::string, RandomSemanticsScore*> cache_map_with_p;

        static void updateStaticInfo(int example_num) {
            if (five_weight_list.empty()) five_weight_list.push_back(1);
            for (int i = five_weight_list.size(); i < example_num; ++i) {
                five_weight_list.push_back(five_weight_list[i - 1] * KStateWeight);
            }
            for (int weight = 1; weight < 4; ++weight) {
                if (two2five[weight].empty()) two2five[weight].push_back(0);
                for (int i = two2five[weight].size(); i < (1 << example_num); ++i) {
                    two2five[weight].push_back(two2five[weight][i >> 1] * KStateWeight + ((i & 1) ? weight : 0));
                }
            }

            int state_num = five_weight_list[example_num - 1] * 2;

            if (five_state_eq_num_list.empty()) five_state_eq_num_list.push_back(0);
            for (int i = five_state_eq_num_list.size(); i < state_num; ++i) {
                int tag = i % 5;
                five_state_eq_num_list.push_back(five_state_eq_num_list[i / 5] + (tag > 0) + (tag == 4));
            }

            for (int mask = 1; mask < 4; ++mask) {
                if (masked_five_state_info_list[mask].empty()) masked_five_state_info_list[mask].emplace_back(0, 0);
                std::vector<int> masked_status(5), masked_cost(5);
                for (int i = 0; i < 5; ++i) {
                    masked_status[i] = _getMask(i, mask);
                    masked_cost[i] = _KFreeNum[i] - _KFreeNum[masked_status[i]];
                }
                for (int i = masked_five_state_info_list[mask].size(); i < state_num; ++i) {
                    int pre_state = i / KStateWeight;
                    int last_status = i % KStateWeight;
                    int masked_state = (masked_five_state_info_list[mask][pre_state].first * KStateWeight) + masked_status[last_status];
                    int total_cost = masked_five_state_info_list[mask][pre_state].second + masked_cost[last_status];
                    masked_five_state_info_list[mask].emplace_back(masked_state, total_cost);
                }
            }
        }

        _CachedDPHolder(TopDownContextGraph* _graph, TopDownGraphMatchStructure* _match, int hidden_example_num, const Example& _inp, RandomSemanticsScore _KOutputSize, CachePool* _match_info_cache):
                graph(_graph), inp(_inp), KOutputSize(_KOutputSize), n(hidden_example_num + 1),  match(_match), match_info_cache(_match_info_cache) {
            cache_list.resize(graph->node_list.size());
            for (int i = 0; i < cache_list.size(); ++i) {
                cache_list[i] = new RandomSemanticsScore[1 << n];
                for (int j = 0; j < (1 << n); ++j) cache_list[i][j] = -1;
            }
            KOPowList.resize(n << 1);
            KNegOPowList.resize(n << 1);
            KOPowList[0] = 1; KNegOPowList[0] = 1;
            for (int i = 1; i < KOPowList.size(); ++i) {
                KOPowList[i] = KOPowList[i - 1] / KOutputSize;
                KNegOPowList[i] = KNegOPowList[i - 1] * (KOutputSize - 1) / KOutputSize;
            }
            updateStaticInfo(n);
            five_state_num = five_weight_list[n - 1] * 2;
            status_weight_matrix = _getViolateWeightMatrix(KOutputSize);
        }

        ~_CachedDPHolder() {
            for (auto* array: cache_list) delete[] array;
            for (const auto& item: cache_map_with_p) {
                delete[] item.second;
            }
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
        void updateViolateProb(RandomSemanticsScore* res) {
            for (int S = (1 << n) - 1; S; --S) {
                int size_s = __builtin_popcount(S);
                res[S] *= KNegOPowList[size_s];
                for (int T = (S - 1) & S;; T = (T - 1) & S) {
                    int size_t = __builtin_popcount(T);
                    res[S] += res[T] * KOPowList[size_s - size_t] * KNegOPowList[size_t];
                    if (!T) break;
                }
            }
        }
        RandomSemanticsScore* solveFGMatch(int node_id) {
            auto* cache = cache_list[node_id];
            auto& node = graph->node_list[node_id];
            if (!_isEmptyCache(cache)) return cache;
            for (int i = 0; i < (1 << n); ++i) cache[i] = 0;
            // LOG(INFO) << "solve match " << node_id << std::endl;
            RandomSemanticsScore rem_prob = 1.0;
            auto* tmp = new RandomSemanticsScore[1 << n];
            { // solve terminal case
                match_info_cache->getPairMatchRes(node_id, inp, tmp);
                for (int i = 1; i <= n; ++i) {
                    for (int S = 0; S < (1 << n); ++S)
                        if (S & (1 << i - 1)) tmp[S - (1 << i - 1)] += tmp[S];
                }
                rem_prob -= tmp[0];
                for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i];
            }
            { // solve matched case, can be potentially improved by FWT
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    auto& edge = node.edge_list[edge_id];
                    if (isTerminateSemantics(edge.semantics.get())) continue;
                    auto weight = edge.weight * edge.weight;
                    rem_prob -= weight;
                    getTwoSubRes(edge.v_list, tmp);
                    updateViolateProb(tmp);
                    for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i] * weight;
                }
                // LOG(INFO) << "Matched case " << node_id << " " << cache[(1 << n) - 1];
            }
            { // solve unmatched case
                //LOG(INFO) << "rem prob " << rem_prob;
                for (int i = 0; i < (1 << n); ++i)
                    cache[i] += KOPowList[__builtin_popcount(i)] * rem_prob;
            }
            delete tmp;
            return cache;
        }
        void getFiveSubRes(const std::vector<int>& v_list, TopDownGraphMatchStructure* s, RandomSemanticsScore* res) {
            for (int i = 0; i < five_state_num; ++i) res[i] = 1.0;
            assert(v_list.size() == s->sub_list.size());
            for (int i = 0; i < v_list.size(); ++i) {
                auto* sub_res = solveFGMatchWithP(v_list[i], s->sub_list[i]);
                for (int j = 0; j < five_state_num; ++j) res[j] *= sub_res[j];
            }
        }
        void getNDimensionalSuffixSum(RandomSemanticsScore* res) {
            for (int i = 1; i <= n; ++i) {
                int key = (1 << i - 1);
                for (int j = 0; j < (1 << n); ++j) {
                    if (j & key) {
                        res[j ^ key] += res[j];
                    }
                }
            }
        }
        void updateTwoTerminateToFiveState(RandomSemanticsScore* two, RandomSemanticsScore* res, RandomSemanticsScore* tmp, int key) {
            for (int i = 0; i < (1 << n); ++i) {
                int S = two2five[key][i];
                if (S < five_state_num) tmp[S] = two[i];
            }
            for (int i = 0; i < five_state_num; ++i) {
                auto& info = masked_five_state_info_list[key][i];
                assert(info.first < five_state_num);
                res[i] += tmp[info.first] * KOPowList[info.second];
            }
        }

        RandomSemanticsScore* solveFGMatchWithP(int node_id, TopDownGraphMatchStructure* s) {
            std::string feature = std::to_string(node_id);
            if (s) feature += "@" + s->program->toString();
            if (cache_map_with_p.count(feature)) return cache_map_with_p[feature];
            auto* cache = new RandomSemanticsScore[five_state_num];
            cache_map_with_p[feature] = cache;
            for (int i = 0; i < five_state_num; ++i) cache[i] = 0;
            if (!s) {
                auto* two_res = solveFGMatch(node_id);
                for (int i = 0; i < (1 << n); ++i) cache[two2five[1][i]] = two_res[i];
                return cache;
            }
            int p_edge_id = s->edge_id; auto& node = graph->node_list[node_id];
            auto& p_edge = node.edge_list[p_edge_id];

            auto* p_sub_res = new RandomSemanticsScore[five_state_num];
            getFiveSubRes(p_edge.v_list, s, p_sub_res);
            auto* tmp_two = new RandomSemanticsScore[(1 << n)];
            auto* tmp_five = new RandomSemanticsScore[five_state_num];

            // solve at least two terminate case
            {
                RandomSemanticsScore nt_prob = 0.0;
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    auto &edge = node.edge_list[edge_id];
                    if (!isTerminateSemantics(edge.semantics.get())) nt_prob += edge.weight;
                }
                if (isTerminateSemantics(p_edge.semantics.get())) {
                    // Both f and g are terminate
                    match_info_cache->getTripleMatchRes(node_id, p_edge.semantics.get(), inp, tmp_five);
                    int state_weight = 1;
                    for (int pos = 1; pos <= n; ++pos) {
                        if (pos == n) {
                            for (int i = 0; i < state_weight; ++i) tmp_five[i] += tmp_five[i + state_weight];
                            break;
                        }
                        for (int i = 0; i < five_state_num; ++i) {
                            int status = i / state_weight % KStateWeight;
                            for (int sup_state: KStrictSuperStateList[status]) {
                                tmp_five[i] += tmp_five[i + (sup_state - status) * state_weight];
                            }
                        }
                        state_weight *= KStateWeight;
                    }
                    for (int i = 0; i < five_state_num; ++i) cache[i] += tmp_five[i];

                    // Exactly one of f/g is terminate
                    match_info_cache->getOneSizePairMatchRes(node_id, p_edge.semantics.get(), tmp_two);
                    for (int i = 0; i < (1 << n - 1); ++i) tmp_two[i] *= nt_prob;
                    for (int i = (1 << n - 1); i < (1 << n); ++i) tmp_two[i] = 0;
                    getNDimensionalSuffixSum(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 2);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 3);
                } else {
                    // both f and g are terminate
                    match_info_cache->getPairMatchRes(node_id, inp, tmp_two);
                    getNDimensionalSuffixSum(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 1);
                }
            }

            // solve F = G = P, P must not be terminate
            if (!isTerminateSemantics(p_edge.semantics.get())) {
                auto current_weight = p_edge.weight * p_edge.weight;
                for (int i = 0; i < five_state_num; ++i) {
                    tmp_five[i] = p_sub_res[i] * current_weight;
                }
                int state_weight = 1;
                for (int pos = 0; pos < n; ++pos) {
                    for (int i = five_state_num - 1; i >= 0; --i) {
                        int status = i / state_weight % KStateWeight;
                        tmp_five[i] *= status_weight_matrix[status][status];
                        for (auto sub_status: KStrictSubStatusList[status])
                            tmp_five[i] += tmp_five[i + (sub_status - status) * state_weight] * status_weight_matrix[status][sub_status];
                    }
                    state_weight *= KStateWeight;
                }
                for (int i = 0; i < five_state_num; ++i) cache[i] += tmp_five[i];
            }

            { // solve F = G <> P, F must not be terminate
                auto* tmp = new RandomSemanticsScore[(1 << n)];
                for (int i = 0; i < (1 << n); ++i) tmp[i] = 0;
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++ edge_id) {
                    auto& edge = node.edge_list[edge_id];
                    if (edge_id == p_edge_id || isTerminateSemantics(edge.semantics.get())) continue;
                    getTwoSubRes(edge.v_list, tmp_two);
                    updateViolateProb(tmp_two);
                    RandomSemanticsScore weight = edge.weight * edge.weight;
                    for (int i = 0; i < (1 << n); ++i) tmp[i] += tmp_two[i] * weight;
                }
                updateTwoTerminateToFiveState(tmp, cache, tmp_five, 1);
                delete[] tmp;
            }
            // std::cout << "f=g " << node_id << " " << cache[five_state_num - 1] << std::endl;

            // solve F = P <> G or G = P <> F, P must not be terminate
            if (!isTerminateSemantics(p_edge.semantics.get())) {
                RandomSemanticsScore current_weight = p_edge.weight * (1 - p_edge.weight);
                for (int mask = 2; mask <= 3; ++mask) {
                    for (int i = 0; i < (1 << n); ++i) {
                        if (two2five[mask][i] < five_state_num) tmp_two[i] = p_sub_res[two2five[mask][i]] * current_weight;
                    }
                    updateViolateProb(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, mask);
                }
            }

            { // solve totally unmatched case
                RandomSemanticsScore rem_prob = 1 - cache[0];
                for (int i = 0; i < five_state_num; ++i) {
                    cache[i] += rem_prob * KOPowList[five_state_eq_num_list[i]];
                }
            }
            delete[] tmp_two; delete[] tmp_five; delete[] p_sub_res;
            return cache;
        }

        double _getTripleScore() {
            auto* w = solveFGMatchWithP(0, match);
            return w[five_state_num - 1];
        }

        double _getPairScore() {
            auto* w = solveFGMatch(0);
            return w[(1 << n) - 1];
        }
    };
    std::vector<std::pair<int, int>> _CachedDPHolder::masked_five_state_info_list[4];
    std::vector<int> _CachedDPHolder::five_weight_list, _CachedDPHolder::two2five[4], _CachedDPHolder::five_state_eq_num_list;
}

RandomSemanticsScore BasicCachedRandomSemanticsScorer::getPairScore(const Example& inp) {
    global::recorder.start("score");
    auto* holder = new _CachedDPHolder(fg->graph, nullptr, example_num, fg->getFlattenInput(inp), KOutputSize, cache);
    auto res = holder->_getPairScore();
    global::recorder.end("score");
    delete holder; return res;
}
RandomSemanticsScore BasicCachedRandomSemanticsScorer::getTripleScore(const PProgram &p, const Example& inp) {
    global::recorder.start("score");
    auto* match = fg->getMatchStructure(p);
    auto* holder = new _CachedDPHolder(fg->graph, match, example_num, fg->getFlattenInput(inp), KOutputSize, cache);
    auto res = holder->_getTripleScore();
    global::recorder.end("score");
    delete holder; return res;
}
BasicCachedRandomSemanticsScorer::~BasicCachedRandomSemanticsScorer() {
    // delete fg;
}