//
// Created by pro on 2022/4/27.
//

#include "istool/selector/random/random_semantics_scorer.h"
#include "istool/selector/random/program_adaptor.h"
#include <cassert>
#include "glog/logging.h"

RandomSemanticsScorer::RandomSemanticsScorer(Env *_env, FlattenGrammar *_fg, double _KOutputSize):
    env(_env), fg(_fg), KOutputSize(_KOutputSize) {
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

    class _DPHolder {
    public:
        RandomSemanticsScore KOutputSize;
        TopDownContextGraph* graph;
        TopDownGraphMatchStructure* match;
        std::vector<std::vector<RandomSemanticsScore>> status_weight_matrix;
        std::vector<DataList> inp_list;
        std::vector<RandomSemanticsScore> KOPowList, KNegOPowList;
        int n, five_state_num;
        std::vector<std::pair<int, int>> masked_five_state_info_list[4];
        std::vector<int> five_weight_list, two2five[4], five_state_eq_num_list;
        std::vector<RandomSemanticsScore*> cache_list;
        std::unordered_map<std::string, RandomSemanticsScore*> cache_map_with_p;
        _DPHolder(TopDownContextGraph* _graph, TopDownGraphMatchStructure* _match, const std::vector<DataList>& _inp_list, RandomSemanticsScore _KOutputSize):
            graph(_graph), inp_list(_inp_list), KOutputSize(_KOutputSize), n(_inp_list.size()), match(_match) {
            cache_list.resize(graph->node_list.size());
            for (int i = 0; i < cache_list.size(); ++i) {
                cache_list[i] = new RandomSemanticsScore[1 << n];
                for (int j = 0; j < (1 << n); ++j) cache_list[i][j] = -1;
            }
            KOPowList.resize(inp_list.size() << 1);
            KNegOPowList.resize(inp_list.size() << 1);
            KOPowList[0] = 1; KNegOPowList[0] = 1;
            for (int i = 1; i < KOPowList.size(); ++i) {
                KOPowList[i] = KOPowList[i - 1] / KOutputSize;
                KNegOPowList[i] = KNegOPowList[i - 1] * (KOutputSize - 1) / KOutputSize;
            }

            five_weight_list.resize(n);
            five_weight_list[0] = 1;
            for (int i = 1; i < n; ++i) {
                five_weight_list[i] = five_weight_list[i - 1] * KStateWeight;
            }
            five_state_num = five_weight_list[n - 1] * 2;

            for (int weight = 1; weight < 4; ++weight) {
                two2five[weight].resize(1 << n);
                two2five[weight][0] = 0;
                for (int i = 1; i < (1 << n); ++i) {
                    two2five[weight][i] = two2five[weight][i >> 1] * KStateWeight + ((i & 1) ? weight : 0);
                }
            }
            five_state_eq_num_list.resize(five_state_num);
            for (int i = 0; i < five_state_num; ++i) {
                int tag = i % 5;
                five_state_eq_num_list[i] = five_state_eq_num_list[i / 5] + (tag > 0) + (tag == 4);
            }

            status_weight_matrix = _getViolateWeightMatrix(KOutputSize);

            for (int mask = 1; mask < 4; ++mask) {
                masked_five_state_info_list[mask].resize(five_state_num);
                masked_five_state_info_list[mask][0] = {0, 0};
                std::vector<int> masked_status(5), masked_cost(5);
                for (int i = 0; i < 5; ++i) {
                    masked_status[i] = _getMask(i, mask);
                    masked_cost[i] = _KFreeNum[i] - _KFreeNum[masked_status[i]];
                }
                for (int i = 1; i < five_state_num; ++i) {
                    int pre_state = i / KStateWeight;
                    int last_status = i % KStateWeight;
                    int masked_state = (masked_five_state_info_list[mask][pre_state].first * KStateWeight) + masked_status[last_status];
                    int total_cost = masked_five_state_info_list[mask][pre_state].second + masked_cost[last_status];
                    masked_five_state_info_list[mask][i] = {masked_state, total_cost};
                }
            }
        }

        ~_DPHolder() {
            for (auto* array: cache_list) delete[] array;
            for (const auto& item: cache_map_with_p) {
                delete[] item.second;
            }
        }

        bool isTerminateSemantics(Semantics* sem) {
            return dynamic_cast<ConstSemantics*>(sem) || dynamic_cast<ParamSemantics*>(sem);
        }
        DataList getTerminateOutputList(Semantics* sem) {
            auto* cs = dynamic_cast<ConstSemantics*>(sem);
            if (cs) return {inp_list.size(), cs->w};
            DataList res(inp_list.size());
            auto* ps = dynamic_cast<ParamSemantics*>(sem);
            assert(ps);
            for (int i = 0; i < inp_list.size(); ++i) {
                res[i] = inp_list[i][ps->id];
            }
            return res;
        }
        int getMatchedExampleSet(const DataList& x, const DataList& y) {
            int res = 0;
            for (int i = 0; i < inp_list.size(); ++i) {
                if (x[i] == y[i]) res |= (1 << i);
            }
            return res;
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
                std::vector<std::pair<DataList, RandomSemanticsScore>> terminate_info_list;
                for (const auto &f_edge: node.edge_list) {
                    if (isTerminateSemantics(f_edge.semantics.get())) {
                        terminate_info_list.emplace_back(getTerminateOutputList(f_edge.semantics.get()), f_edge.weight);
                    }
                }
                for (auto& f_info: terminate_info_list) {
                    for (auto& g_info: terminate_info_list) {
                        int S = getMatchedExampleSet(f_info.first, g_info.first);
                        RandomSemanticsScore weight = f_info.second * g_info.second;
                        rem_prob -= weight;
                        for (auto T = S;; T = (T - 1) & S) {
                            cache[T] += weight;
                            if (!T) break;
                        }
                    }
                }
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
            RandomSemanticsScore rem_prob = 1.0;

            auto* p_sub_res = new RandomSemanticsScore[five_state_num];
            getFiveSubRes(p_edge.v_list, s, p_sub_res);
            auto* tmp_two = new RandomSemanticsScore[(1 << n)];
            auto* tmp_five = new RandomSemanticsScore[five_state_num];

            // solve at least two terminate case
            {
                std::vector<std::pair<RandomSemanticsScore, DataList>> terminate_info;
                RandomSemanticsScore nt_prob = 0.0;
                for (int edge_id = 0; edge_id < node.edge_list.size(); ++edge_id) {
                    auto &edge = node.edge_list[edge_id];
                    if (isTerminateSemantics(edge.semantics.get())) {
                        terminate_info.emplace_back(edge.weight, getTerminateOutputList(edge.semantics.get()));
                    } else {
                        nt_prob += edge.weight;
                    }
                }
                if (isTerminateSemantics(p_edge.semantics.get())) {
                    DataList p_oup = getTerminateOutputList(p_edge.semantics.get());
                    // Both f and g are terminate
                    for (int i = 0; i < five_state_num; ++i) tmp_five[i] = 0;
                    for (auto &f_info: terminate_info) {
                        for (auto &g_info: terminate_info) {
                            double weight = f_info.first * g_info.first;
                            rem_prob -= weight;
                            int state = (f_info.second[n - 1] == g_info.second[n - 1]);
                            for (int i = n - 2; i >= 0; --i) {
                                int status = 0;
                                if (p_oup[i] == f_info.second[i] && p_oup[i] == g_info.second[i]) status = 4;
                                else if (p_oup[i] == g_info.second[i]) status = 3;
                                else if (p_oup[i] == f_info.second[i]) status = 2;
                                else if (f_info.second[i] == g_info.second[i]) status = 1;
                                state = state * KStateWeight + status;
                            }
                            assert(state < five_state_num);
                            tmp_five[state] += weight;
                        }
                    }

                    int state_weight = 1;
                    for (int pos = 1; pos <= n; ++pos) {
                        if (pos == n) {
                            for (int i = 0; i < state_weight; ++i) tmp_five[i] += tmp_five[i + state_weight];
                            break;
                        }
                        for (int i = 0; i < five_state_num; ++i) {
                            int status = i / state_weight % KStateWeight;
                            for (int sub_state: KStrictSuperStateList[status]) {
                                tmp_five[i] += tmp_five[i + (sub_state - status) * state_weight];
                            }
                        }
                        state_weight *= KStateWeight;
                    }
                    for (int i = 0; i < five_state_num; ++i) cache[i] += tmp_five[i];

                    // Exactly one of f/g is terminate
                    for (int i = 0; i < (1 << n); ++i) tmp_two[i] = 0;
                    for (auto &f_info: terminate_info) {
                        double weight = f_info.first * nt_prob;
                        rem_prob -= weight * 2;
                        int two_state = 0;
                        for (int i = n - 2; i >= 0; --i) {
                            two_state = (two_state << 1) + (p_oup[i] == f_info.second[i]);
                        }
                        assert(two_state < (1 << n));
                        tmp_two[two_state] += weight;
                    }
                    getNDimensionalSuffixSum(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 2);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 3);
                } else {
                    // both f and g are terminate
                    for (int i = 0; i < (1 << n); ++i) tmp_two[i] = 0.0;
                    for (auto &f_info: terminate_info) {
                        for (auto &g_info: terminate_info) {
                            double weight = f_info.first * g_info.first;
                            rem_prob -= weight;
                            int state = (f_info.second[n - 1] == g_info.second[n - 1]);
                            for (int i = n - 2; i >= 0; --i) {
                                state <<= 1;
                                if (f_info.second[i] == g_info.second[i]) ++state;
                            }
                            tmp_two[state] += weight;
                        }
                    }
                    getNDimensionalSuffixSum(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, 1);
                }
            }

            // solve F = G = P, P must not be terminate
            if (!isTerminateSemantics(p_edge.semantics.get())) {
                auto current_weight = p_edge.weight * p_edge.weight;
                rem_prob -= current_weight;
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
                    RandomSemanticsScore weight = edge.weight * edge.weight; rem_prob -= weight;
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
                    rem_prob -= current_weight;
                    for (int i = 0; i < (1 << n); ++i) {
                        if (two2five[mask][i] < five_state_num) tmp_two[i] = p_sub_res[two2five[mask][i]] * current_weight;
                    }
                    updateViolateProb(tmp_two);
                    updateTwoTerminateToFiveState(tmp_two, cache, tmp_five, mask);
                }
            }

            { // solve totally unmatched case
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
}

DataStorage RandomSemanticsScorer::getFlattenInpStorage(const DataStorage &inp_list) {
    DataStorage res;
    for (auto& inp: inp_list) res.push_back(fg->getFlattenInput(inp));
    return res;
}
RandomSemanticsScore RandomSemanticsScorer::getPairScore(const DataStorage &inp) {
    auto* holder = new _DPHolder(fg->graph, nullptr, getFlattenInpStorage(inp), KOutputSize);
    auto res = holder->_getPairScore();
    delete holder;
    return res;
}
RandomSemanticsScore RandomSemanticsScorer::getTripleScore(const PProgram& p, const DataStorage &inp) {
    auto* match = fg->getMatchStructure(p); assert(match);
    auto* holder = new _DPHolder(fg->graph, match, getFlattenInpStorage(inp), KOutputSize);
    auto res = holder->_getTripleScore();
    delete holder;
    return res;
}
RandomSemanticsScorer::~RandomSemanticsScorer() {
    // delete fg;
}