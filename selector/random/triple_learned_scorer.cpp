//
// Created by pro on 2022/6/11.
//

#include "istool/selector/random/learned_scorer.h"
#include "glog/logging.h"

namespace {
    void _clear(RandomSemanticsScore* res, int n, RandomSemanticsScore v = 0.0) {
        for (int i = 0; i < n; ++i) res[i] = v;
    }
}

class TripleDPHolder {
public:
    std::vector<RandomSemanticsModel*> model_list;
    RandomSemanticsModel* last_model;
    TopDownContextGraph* graph;
    DataList inp;
    std::vector<RandomSemanticsScore*> pair_cache;
    std::unordered_map<std::string, RandomSemanticsScore*> triple_cache;
    int n, m_num;
    std::vector<int> two2eight;
    TripleDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list, const DataList& _inp):
        m_num(_model_list.size()), n(_model_list.size() * 3 - 2), graph(_graph), model_list(_model_list), inp(_inp), last_model(_model_list[_model_list.size() - 1]) {
        pair_cache.resize(graph->node_list.size(), nullptr);
        two2eight.resize(1 << n); two2eight[0] = 0;
        for (int i = 1; i < (1 << n); ++i) two2eight[i] = (two2eight[i >> 1] << 3) + (i & 1);
    }
    ~TripleDPHolder() {
        for (auto* cache: pair_cache) delete[] cache;
        for (const auto& info: triple_cache) delete[] info.second;
    }

    RandomSemanticsScore* getPairResult(int node_id) {
        if (pair_cache[node_id]) return pair_cache[node_id];
        auto* cache = new RandomSemanticsScore[1 << m_num];
        auto& node = graph->node_list[node_id];
        auto* tmp = new RandomSemanticsScore[1 << m_num];
        _clear(cache, 1 << m_num);
        for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
            for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                auto w = node.edge_list[x_id].weight * node.edge_list[y_id].weight;
                if (x_id != y_id) {
                    std::vector<RandomSemanticsScore> weight_list;
                    for (auto* model: model_list) weight_list.push_back(model->weight_list[node_id][x_id][y_id]);
                    selector::random::buildSetProduct(tmp, weight_list);
                } else getPairResult(node_id, x_id, tmp);
                for (int i = 0; i < (1 << m_num); ++i) cache[i] += tmp[i] * w;
            }
        }
        delete[] tmp;
        return cache;
    }

    void getPairResult(int node_id, int edge_id, RandomSemanticsScore* res) {
        _clear(res, 1 << m_num, 1.0);
        auto& edge = graph->node_list[node_id].edge_list[edge_id];
        for (auto v: edge.v_list) {
            auto* sub_res = getPairResult(v);
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

    RandomSemanticsScore* getTripleScore(int node_id, TopDownGraphMatchStructure* match) {
        auto feature = std::to_string(node_id) + "@" + match->program->toString();
        if (triple_cache.count(feature)) return triple_cache[feature];
        auto* cache = triple_cache[feature] = new RandomSemanticsScore[1 << n];
        _clear(cache, 1 << n, 0.0);
        auto& node = graph->node_list[node_id]; int p_edge_id = match->edge_id;
        auto& p_edge = node.edge_list[p_edge_id];

        auto* sub_res = new RandomSemanticsScore[1 << n];
        auto* two_tmp = new RandomSemanticsScore[1 << m_num];
        auto* tmp = new RandomSemanticsScore[1 << n];
        getTripleResult(node_id, p_edge_id, match, sub_res);

        for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
            for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                auto w = node.edge_list[x_id].weight * node.edge_list[y_id].weight;
                if (x_id == y_id) {
                    if (x_id == p_edge_id) {
                        for (int i = 0; i < (1 << n); ++i) tmp[i] = sub_res[i];
                    } else {
                        getPairResult(node_id, x_id, two_tmp);
                        int mask = 0;
                        for (int i = 0; i < m_num; ++i) mask |= (1 << (i * 3));
                        for (int i = 0; i < (1 << m_num); ++i) tmp[two2eight[i]] = two_tmp[i];
                        for (int i = 0; i + 1 < m_num; ++i) {
                            tmp[2 << (i * 3)] = tmp[4 << (i * 3)] = model_list[i]->weight_list[node_id][x_id][p_edge_id];
                        }
                        selector::random::buildSetProductWithMask(tmp, 1 << n, mask);
                    }
                } else {
                    tmp[0] = 1.0;
                    for (int i = 0; i + 1 < m_num; ++i) {
                        tmp[1 << (i * 3)] = model_list[i]->weight_list[node_id][x_id][y_id];
                        tmp[2 << (i * 3)] = model_list[i]->weight_list[node_id][x_id][p_edge_id];
                        tmp[4 << (i * 3)] = model_list[i]->weight_list[node_id][y_id][p_edge_id];
                    }
                    tmp[1 << (n - 1)] = last_model->weight_list[node_id][x_id][y_id];
                    if (x_id != p_edge_id && y_id != p_edge_id) {
                        selector::random::buildSetProductWithMask(tmp, 1 << n, 0.0);
                    } else {
                        int mask = 0, key = (x_id == p_edge_id) ? 2 : 4;
                        for (int i = 0; i + 1 < m_num; ++i) mask |= (key << (i * 3));
                        for (int i = mask; i; i = (i - 1) & mask) tmp[i] = sub_res[i];
                        selector::random::buildSetProductWithMask(tmp, 1 << n, mask);
                    }
                }
                for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i] * w;
            }
        }
        return cache;
    }

    RandomSemanticsScore getScore(TopDownGraphMatchStructure* match) {
        auto* res = getTripleScore(0, match);
        return res[(1 << n) - 1];
    }
};

RandomSemanticsScore TrivialTripleLearnedScorer::getScore(const PProgram &p, const Example &inp) {
    model_list.push_back(learnModel(inp));
    auto* match = fg->getMatchStructure(p);
    auto* holder = new TripleDPHolder(fg->graph, model_list, fg->getFlattenInput(inp));
    auto res = holder->getScore(match);
    delete match; delete holder;
    model_list.pop_back();
    return res;
}
TrivialTripleLearnedScorer::TrivialTripleLearnedScorer(Env *env, FlattenGrammar *fg, RandomSemanticsLearner *_learner):
    LearnedScorer(env, fg, _learner) {
}