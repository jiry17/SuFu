//
// Created by pro on 2022/6/10.
//

#include <istool/selector/random/random_semantics_selector.h>
#include "istool/selector/random/learned_scorer.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

TrivialDifferentPairLearnedScorer::TrivialDifferentPairLearnedScorer(Env *env, FlattenGrammar *fg, RandomSemanticsLearner *_learner):
    LearnedScorer(env, fg, _learner) {
}

namespace {
    void _clear(RandomSemanticsScore* x, int n, RandomSemanticsScore w = 0.0) {
        for (int i = 0; i < n; ++i) x[i] = w;
    }
}

void selector::random::buildSetProduct(RandomSemanticsScore *res, const std::vector<RandomSemanticsScore> &weight_list) {
    res[0] = 1.0; int n = weight_list.size();
    for (int i = 0; i < n; ++i) res[1 << i] = weight_list[i];
    for (int S = 1; S < (1 << n); ++S) {
        int T = S & (-S); res[S] = res[T] * res[S - T];
    }
}
bool selector::random::isTerminate(const TopDownContextGraph::Edge &edge) {
    auto* sem = edge.semantics.get();
    return dynamic_cast<ParamSemantics*>(sem) || dynamic_cast<ConstSemantics*>(sem);
}
Data selector::random::getTerminateOutput(const TopDownContextGraph::Edge &edge, const DataList &inp) {
    auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
    if (ps) return inp[ps->id];
    auto* cs = dynamic_cast<ConstSemantics*>(edge.semantics.get());
    return cs->w;
}
LearnedScorerType selector::random::getScorerType(const std::string &name) {
    if (name == "same") return LearnedScorerType::SAME_PAIR;
    if (name == "diff") return LearnedScorerType::DIFFERENT_PAIR;
    if (name == "triple") return LearnedScorerType::TRIPLE;
    if (name == "pair") return LearnedScorerType::PAIR;
    LOG(FATAL) << "Unknown scorer type " << name;
}
std::pair<LearnedScorerType, int> selector::random::splitScorerName(const std::string &name, Env* env) {
    int i = 0;
    while (i < name.length() && (name[i] < '0' || name[i] > '9')) ++i;
    auto prefix = name.substr(0, i);
    int num = 0;
    while (i < name.length() && name[i] != '@') {
        num = num * 10 + int(name[i]) - int('0'); ++i;
    }
    if (name[i] == '@') {
        int example_num = std::stoi(name.substr(i + 1));
        LOG(INFO) << prefix << " #flatten: " << num << " #example: " << example_num;
        env->setConst(selector::random::KExampleNumLimitName, BuildData(Int, example_num));
    } else {
        env->setConst(selector::random::KExampleNumLimitName, BuildData(Int, 3));
        LOG(INFO) << prefix << " #flatten: " << num << " #example: " << 3;
    }
    return {getScorerType(prefix), num};
}
namespace {
    LearnedScorer* _buildScorer(LearnedScorerType type, Env* env, FlattenGrammar* fg, RandomSemanticsLearner* learner) {
        switch (type) {
            case LearnedScorerType::SAME_PAIR: return new SamePairLearnedScorer(env, fg, learner);
            case LearnedScorerType::DIFFERENT_PAIR: return new OptimizedDifferentPairLearnedScorer(env, fg, learner);
            case LearnedScorerType::TRIPLE: return new OptimizedTripleLearnedScorer(env, fg, learner);
            case LearnedScorerType::PAIR: return new OptimizedPairLeanedScorer(env, fg, learner);
        }
    }
    LearnedScorer* _buildTrivialScorer(LearnedScorerType type, Env* env, FlattenGrammar* fg, RandomSemanticsLearner* learner) {
        switch (type) {
            case LearnedScorerType::SAME_PAIR: return new SamePairLearnedScorer(env, fg, learner);
            case LearnedScorerType::DIFFERENT_PAIR: return new TrivialDifferentPairLearnedScorer(env, fg, learner);
            case LearnedScorerType::TRIPLE: return new TrivialTripleLearnedScorer(env, fg, learner);
        }
    }
}
LearnedScorer* selector::random::buildDefaultScorer(LearnedScorerType type, Env *env, FlattenGrammar *fg) {
    auto* holder = new BasicSampleStructureHolder(env, fg);
    auto* learner = new FixedSampleRandomSemanticsLearner(holder);
    return _buildScorer(type, env, fg, learner);
}
LearnedScorer* selector::random::buildTrivialScorer(LearnedScorerType type, Env *env, FlattenGrammar *fg) {
    auto* holder = new BasicSampleStructureHolder(env, fg);
    auto* learner = new FixedSampleRandomSemanticsLearner(holder);
    return _buildTrivialScorer(type, env, fg, learner);
}
LearnedScorer* selector::random::buildVSAScorer(LearnedScorerType type, Specification* spec, FlattenGrammar *fg, const VSAEnvSetter &setter) {
    auto* holder = new VSASampleStructureHolder(spec, fg, setter);
    auto* learner = new FixedSampleRandomSemanticsLearner(holder);
    return _buildScorer(type, spec->env.get(), fg, learner);
}

void selector::random::setLearnedExampleLimit(LearnedScorerType type, Env* env) {
    /*const int KBaseExampleNum = 9;
    if (type == LearnedScorerType::SAME_PAIR || type == LearnedScorerType::DIFFERENT_PAIR) {
        env->setConst(selector::random::KExampleNumLimitName, BuildData(Int, KBaseExampleNum / 2));
    } else if (type == LearnedScorerType::TRIPLE) {
        env->setConst(selector::random::KExampleNumLimitName, BuildData(Int, KBaseExampleNum / 3));
    }*/
    //env->setConst(selector::random::KExampleNumLimitName, BuildData(Int, 4));
}

class TrivialDPHolder {
public:
    std::vector<RandomSemanticsModel*> model_list;
    TopDownContextGraph* graph;
    DataList inp;
    RandomSemanticsModel* last_model;
    std::vector<RandomSemanticsScore*> pair_cache;
    std::unordered_map<std::string, RandomSemanticsScore*> triple_cache;
    int n, m_num;
    std::vector<int> two2four;
    TrivialDPHolder(TopDownContextGraph* _graph, const std::vector<RandomSemanticsModel*>& _model_list,
            const DataList& _inp): inp(_inp), m_num(_model_list.size()),
        graph(_graph), model_list(_model_list), n(_model_list.size() * 2 - 1) {
        pair_cache.resize(graph->node_list.size(), nullptr);
        last_model = model_list[m_num - 1];
        two2four.resize(1 << m_num); two2four[0] = 0;
        for (int i = 1; i < (1 << m_num); ++i) two2four[i] = (two2four[i >> 1] << 2) + (i & 1);
    }
    ~TrivialDPHolder() {
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
        for (int i = 0; i < n; ++i) weight_list[i] = model_list[i >> 1]->weight_list[node_id][edge_id][edge_id];
        selector::random::updateViolateProb(res, weight_list);
    }

    RandomSemanticsScore* getTripleScore(int node_id, TopDownGraphMatchStructure* match) {
        auto feature = std::to_string(node_id) + "@" + match->program->toString();
        if (triple_cache.count(feature)) return triple_cache[feature];
        auto* cache = triple_cache[feature] = new RandomSemanticsScore[1 << n];
        _clear(cache, 1 << n, 0.0);
        auto& node = graph->node_list[node_id]; int p_edge_id = match->edge_id;
        auto& p_edge = node.edge_list[p_edge_id];

        auto* tmp = new RandomSemanticsScore[1 << n];
        auto* two_tmp = new RandomSemanticsScore[1 << m_num];
        auto* sub_res = new RandomSemanticsScore[1 << n];
        getTripleResult(node_id, p_edge_id, match, sub_res);

        //LOG(INFO) << "Learn for " << feature;

        for (int x_id = 0; x_id < node.edge_list.size(); ++x_id) {
            for (int y_id = 0; y_id < node.edge_list.size(); ++y_id) {
                auto w = node.edge_list[x_id].weight * node.edge_list[y_id].weight;
                if (x_id == y_id) {
                    if (x_id == p_edge_id) {
                        for (int i = 0; i < (1 << n); ++i) tmp[i] = sub_res[i];
                    } else {
                        int mask = 0;
                        for (int i = 0; i < m_num; ++i) mask |= (1 << (i << 1));
                        getPairResult(node_id, x_id, two_tmp);
                        for (int i = 0; i < (1 << m_num); ++i) tmp[two2four[i]] = two_tmp[i];
                        for (int i = 0; i + 1 < m_num; ++i) tmp[2 << (i << 1)] = model_list[i]->weight_list[node_id][x_id][p_edge_id];
                        selector::random::buildSetProductWithMask(tmp, 1 << n, mask);
                    }
                } else if (x_id == p_edge_id) {
                    int mask = 0;
                    for (int i = 0; i + 1 < m_num; ++i) mask |= (2 << (i << 1));
                    for (int i = 0; i < m_num; ++i) tmp[1 << (i << 1)] = model_list[i]->weight_list[node_id][x_id][y_id];
                    for (int i = mask; i; i = (i - 1) & mask) tmp[i] = sub_res[i];
                    tmp[0] = 1.0;
                    selector::random::buildSetProductWithMask(tmp, 1 << n, mask);
                } else {
                    std::vector<RandomSemanticsScore> weight_list(n);
                    for (int i = 0; i < n; ++i) {
                        if (i & 1) weight_list[i] = model_list[i >> 1]->weight_list[node_id][x_id][p_edge_id];
                        else weight_list[i] = model_list[i >> 1]->weight_list[node_id][x_id][y_id];
                    }
                    selector::random::buildSetProduct(tmp, weight_list);
                }
                for (int i = 0; i < (1 << n); ++i) cache[i] += tmp[i] * w;
                //std::cout << node.edge_list[x_id].semantics->getName() << " " << node.edge_list[y_id].semantics->getName() << std::endl;
                //for (int i = 0; i < (1 << n); ++i) std::cout << tmp[i] << " "; std::cout << std::endl;
            }
        }
        delete[] tmp; delete[] two_tmp; delete[] sub_res;
        return cache;
    }

    RandomSemanticsScore getScore(TopDownGraphMatchStructure* match) {
        auto* res = getTripleScore(0, match);
        return res[(1 << n) - 1];
    }


};

RandomSemanticsScore TrivialDifferentPairLearnedScorer::getScore(const PProgram &p, const Example &inp) {
    auto* model = learnModel(inp); model_list.push_back(model);
    auto* match = fg->getMatchStructure(p);
    auto* holder = new TrivialDPHolder(fg->graph, model_list, fg->getFlattenInput(inp));
    auto res = holder->getScore(match);
    delete match; delete holder; model_list.pop_back();
    return res;
}