//
// Created by pro on 2022/4/30.
//

#include "istool/selector/random/random_semantics_scorer_tester.h"
#include "glog/logging.h"

namespace {
    struct _ProgramInfo {
        double prob;
        PProgram program;
        _ProgramInfo(double _prob, const PProgram& _program): prob(_prob), program(_program) {}
    };

    typedef std::vector<_ProgramInfo> _ProgramInfoList;
    typedef std::vector<_ProgramInfoList> _ProgramInfoStorage;

    void _mergeAllProgramInfo(int pos, double prob, ProgramList& tmp, const PSemantics& sem, const _ProgramInfoStorage& storage, _ProgramInfoList& res) {
        if (pos == storage.size()) {
            res.emplace_back(prob, std::make_shared<Program>(sem, tmp));
            return;
        }
        for (auto& info: storage[pos]) {
            tmp[pos] = info.program;
            _mergeAllProgramInfo(pos + 1, prob * info.prob, tmp, sem, storage, res);
        }
    }

    _ProgramInfoList _mergeAllProgramInfo(double weight, const PSemantics& sem, const _ProgramInfoStorage& storage) {
        _ProgramInfoList res;
        ProgramList tmp(storage.size());
        _mergeAllProgramInfo(0, weight, tmp, sem, storage, res);
        return res;
    }

    _ProgramInfoList _collectAllProgramInfo(int node_id, TopDownContextGraph* graph, std::unordered_map<int, _ProgramInfoList>& cache) {
        if (cache.count(node_id)) return cache[node_id];
        _ProgramInfoList res;
        auto& node = graph->node_list[node_id];
        for (auto& edge: node.edge_list) {
            _ProgramInfoStorage storage(edge.v_list.size());
            for (int i = 0; i < edge.v_list.size(); ++i) {
                storage[i] = _collectAllProgramInfo(edge.v_list[i], graph, cache);
            }
            for (auto& merged_info: _mergeAllProgramInfo(edge.weight, edge.semantics, storage)) {
                res.push_back(merged_info);
            }
        }
        return cache[node_id] = res;
    }

    _ProgramInfoList _collectAllProgramInfo(TopDownContextGraph* graph) {
        std::unordered_map<int, _ProgramInfoList> cache;
        _collectAllProgramInfo(0, graph, cache);
        return cache[0];
    }

    _ProgramInfoList _collectAllProgramInfo(TopDownContextGraph* graph, const TopDownContextGraph::Edge& edge, std::unordered_map<int, _ProgramInfoList>& cache) {
        std::vector<_ProgramInfoList> storage(edge.v_list.size());
        for (int i = 0; i < edge.v_list.size(); ++i) {
            storage[i] = _collectAllProgramInfo(edge.v_list[i], graph, cache);
        }
        return _mergeAllProgramInfo(1, edge.semantics, storage);
    }

    RandomSemanticsScore _getPairEqualProb(int node_id, TopDownContextGraph* graph, TopDownGraphMatchStructure* x, TopDownGraphMatchStructure* y,
            RandomSemanticsModel* m) {
        if (x->edge_id != y->edge_id) return m->weight_list[node_id][x->edge_id][y->edge_id];
        auto& edge = graph->node_list[node_id].edge_list[x->edge_id];
        RandomSemanticsScore sub_equal_prob = 1.0;
        for (int i = 0; i < edge.v_list.size(); ++i) {
            sub_equal_prob *= _getPairEqualProb(edge.v_list[i], graph, x->sub_list[i], y->sub_list[i], m);
        }
        return sub_equal_prob + (1 - sub_equal_prob) * m->weight_list[node_id][x->edge_id][y->edge_id];
    }

    RandomSemanticsScore _getSatisfyProb(TopDownContextGraph* graph, TopDownGraphMatchStructure* x_match,
            TopDownGraphMatchStructure* y_match, TopDownGraphMatchStructure* p_match, RandomSemanticsModel* model, LearnedScorerType type) {
        switch (type) {
            case LearnedScorerType::SAME_PAIR:
                return _getPairEqualProb(0, graph, x_match, p_match, model) * _getPairEqualProb(0, graph, y_match, p_match, model);
            case LearnedScorerType::DIFFERENT_PAIR:
                return _getPairEqualProb(0, graph, x_match, p_match, model) * _getPairEqualProb(0, graph, x_match, y_match, model);
            case LearnedScorerType::TRIPLE:
                return _getPairEqualProb(0, graph, x_match, p_match, model) * _getPairEqualProb(0, graph, y_match, p_match, model)
                     * _getPairEqualProb(0, graph, x_match, y_match, model);
        }
    }
}

RandomSemanticsScore selector::test::getLearnedGroundTruth(Env *env, FlattenGrammar *fg, const PProgram &p,
        const std::vector<RandomSemanticsModel *> &model_list, LearnedScorerType type) {
    auto info_list = _collectAllProgramInfo(fg->graph);
    auto* p_match = fg->getMatchStructure(p);
    auto* last_model = model_list[model_list.size() - 1];
    RandomSemanticsScore res = 0.0;
    for (auto& x_info: info_list) {
        auto* x_match = fg->getMatchStructure(x_info.program);
        for (auto& y_info: info_list) {
            auto* y_match = fg->getMatchStructure(y_info.program);
            auto equal_prob = _getPairEqualProb(0, fg->graph, x_match, y_match, last_model);
            for (int i = 0; i + 1 < model_list.size(); ++i) {
                equal_prob *= _getSatisfyProb(fg->graph, x_match, y_match, p_match, model_list[i], type);
            }
            res += equal_prob * x_info.prob * y_info.prob;
            delete y_match;
        }
        delete x_match;
    }
    return res;
}
