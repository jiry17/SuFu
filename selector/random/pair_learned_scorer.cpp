//
// Created by pro on 2022/6/10.
//

#include <istool/selector/random/random_semantics_selector.h>
#include "istool/selector/random/learned_scorer.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

OptimizedPairLeanedScorer::OptimizedPairLeanedScorer(Env *env, FlattenGrammar *fg, RandomSemanticsLearner *_learner):
        LearnedScorer(env, fg, _learner), cache_pool(new DifferentPairCache(fg->graph)) {
}
void OptimizedPairLeanedScorer::pushExample(const Example &inp) {
    LearnedScorer::pushExample(inp); cache_pool->pushExample(fg->getFlattenInput(inp));
}
void OptimizedPairLeanedScorer::popExample() {
    LearnedScorer::popExample(); cache_pool->popExample();
}
OptimizedPairLeanedScorer::~OptimizedPairLeanedScorer() {
    delete cache_pool;
}

namespace {
    void _clear(RandomSemanticsScore* x, int n, RandomSemanticsScore w = 0.0) {
        for (int i = 0; i < n; ++i) x[i] = w;
    }
}

class OptimizedPairDPHolder {
public:
    std::vector<RandomSemanticsModel*> model_list;
    TopDownContextGraph* graph;
    DataList inp;
    std::vector<RandomSemanticsScore*> pair_cache;
    int m_num;
    DifferentPairCache* cache_pool;

    OptimizedPairDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list,
            const DataList& _inp, DifferentPairCache* _cache_pool): graph(_graph), model_list(_model_list),
            m_num(_model_list.size()), cache_pool(_cache_pool), inp(_inp) {
        pair_cache.resize(graph->node_list.size(), nullptr);
    }
    ~OptimizedPairDPHolder() {
        for (auto* x: pair_cache) delete[] x;
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

    RandomSemanticsScore getScore() {
        auto* res = getPairScore(0);
        return res[(1 << m_num) - 1];
    }
};

RandomSemanticsScore OptimizedPairLeanedScorer::getScore(const PProgram &p, const Example &inp) {
    auto* model = learnModel(inp); model_list.push_back(model);
    auto* holder = new OptimizedPairDPHolder(fg->graph, model_list, fg->getFlattenInput(inp), cache_pool);
    auto res = holder->getScore();
    delete holder; model_list.pop_back();
    return res;
}