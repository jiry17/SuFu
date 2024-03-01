//
// Created by pro on 2022/6/7.
//

#ifndef ISTOOL_LEARNED_SCORER_H
#define ISTOOL_LEARNED_SCORER_H

#include "random_semantics_scorer.h"

enum class LearnedScorerType {
    SAME_PAIR,
    DIFFERENT_PAIR,
    TRIPLE,
    PAIR
};

class LearnCacheItem {
public:
    std::vector<RandomSemanticsScore*> score_list;
    LearnCacheItem(const std::vector<RandomSemanticsScore *>& _score_list);
    ~LearnCacheItem();
};

class LearnedScorer {
public:
    RandomSemanticsLearner* learner;
    std::vector<RandomSemanticsModel*> model_list;
    std::unordered_map<std::string, RandomSemanticsModel*> model_cache;
    Env* env; FlattenGrammar* fg;
    LearnedScorer(Env* env, FlattenGrammar* fg, RandomSemanticsLearner* _learner);
    RandomSemanticsModel* learnModel(const Example& inp);
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp) = 0;
    ~LearnedScorer();
};
typedef std::function<LearnedScorer*(Env*, FlattenGrammar*)> LearnedScorerBuilder;

class SamePairLearnedScorer: public LearnedScorer {
public:
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    void buildCache(int node_id, TopDownGraphMatchStructure* p);
    void clearCache();
    void initCache(const PProgram& p);
    SamePairLearnedScorer(Env* env, FlattenGrammar* _fg, RandomSemanticsLearner* _learner);
    PProgram current_program;
    std::unordered_map<std::string, LearnCacheItem*> pair_info_cache;
    virtual ~SamePairLearnedScorer();
};

class TrivialDifferentPairLearnedScorer: public LearnedScorer {
public:
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    TrivialDifferentPairLearnedScorer(Env* env, FlattenGrammar* fg, RandomSemanticsLearner* _learner);
    virtual ~TrivialDifferentPairLearnedScorer() = default;
};

class DifferentPairCacheItem {
    std::unordered_map<std::string, std::vector<int>> getEqClass(const DataList& inp);
public:
    TopDownContextGraph::Node* node;
    std::vector<int> index_list;
    std::vector<TopDownContextGraph::Edge*> terminate_list;
    std::vector<std::vector<int>> match_storage;
    std::vector<RandomSemanticsScore*> res_list;
    RandomSemanticsScore* full_res;
    int example_num;
    void initResList();
    void pushExample(const DataList& inp);
    void popExample();
    std::vector<RandomSemanticsScore*> getTerminateResult(const DataList& inp);
    void getFullResult(const DataList& inp, RandomSemanticsScore* res);
    DifferentPairCacheItem(TopDownContextGraph::Node* _node);
    ~DifferentPairCacheItem();
};

class DifferentPairCache {
public:
    TopDownContextGraph* graph;
    std::vector<DifferentPairCacheItem*> item_list;
    void pushExample(const DataList& inp);
    void popExample();
    std::vector<RandomSemanticsScore*> getTerminateResult(int node_id, const DataList& inp);
    void getFullResult(int node_id, const DataList& inp, RandomSemanticsScore* res);
    DifferentPairCache(TopDownContextGraph* _graph);
    ~DifferentPairCache();
};

class OptimizedDifferentPairLearnedScorer: public LearnedScorer {
public:
    DifferentPairCache* cache_pool;
    virtual void pushExample(const DataList& inp);
    virtual void popExample();
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    OptimizedDifferentPairLearnedScorer(Env* env, FlattenGrammar* fg, RandomSemanticsLearner* _learner);
    virtual ~OptimizedDifferentPairLearnedScorer();
};

class OptimizedPairLeanedScorer: public LearnedScorer {
public:
    DifferentPairCache* cache_pool;
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    OptimizedPairLeanedScorer(Env* env, FlattenGrammar* _fg, RandomSemanticsLearner* _learner);
    virtual ~OptimizedPairLeanedScorer();
};

class TrivialTripleLearnedScorer: public LearnedScorer {
public:
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    TrivialTripleLearnedScorer(Env* env, FlattenGrammar* fg, RandomSemanticsLearner* _learner);
    virtual ~TrivialTripleLearnedScorer() = default;
};

class TripleCacheItem {
public:
    std::unordered_map<std::string, std::vector<int>> getEqClass(const DataList& inp);
public:
    TopDownContextGraph::Node* node;
    std::vector<int> index_list;
    std::vector<TopDownContextGraph::Edge*> terminate_list;
    std::vector<std::vector<int>> match_storage;
    RandomSemanticsScore* full_res;

    int example_num;
    void initResList();
    void pushExample(const DataList& inp);
    void popExample();
    void getFullResult(const DataList& inp, RandomSemanticsScore* res);
    TripleCacheItem(TopDownContextGraph::Node* _node);
    ~TripleCacheItem();
};

class TripleCacheItemWithP {
public:
    std::unordered_map<std::string, std::vector<int>> getEqClass(const DataList& inp);
    void updateProduct(RandomSemanticsScore* x, int i, int j, int bias);
    void limitedSuffixSum(RandomSemanticsScore* x, int n);
public:
    TopDownContextGraph::Node* node;
    std::vector<int> index_list;
    std::vector<TopDownContextGraph::Edge*> terminate_list;
    std::vector<std::vector<int>> match_storage;
    std::vector<int> two2eight;
    int p_id, example_num;
    std::vector<RandomSemanticsModel*> model_list;
    std::vector<RandomSemanticsScore*> p_weight_list;
    RandomSemanticsScore* full_res;
    void getFullResult(const DataList& inp, RandomSemanticsScore* res);
    TripleCacheItemWithP(TripleCacheItem* item, int node_id, int p_id, const std::vector<RandomSemanticsModel*>& model_list);
    ~TripleCacheItemWithP();
};

class TripleCache {
    void _buildP(int node_id, TopDownGraphMatchStructure* p, const std::vector<RandomSemanticsModel*>& _model_list);
public:
    std::unordered_map<std::string, TripleCacheItemWithP*> p_map;
    std::vector<TripleCacheItem*> item_list;
    TopDownContextGraph* graph;
    TripleCache(TopDownContextGraph* _graph);
    void setP(TopDownGraphMatchStructure* p, const std::vector<RandomSemanticsModel*>& model_list);
    void pushExample(const DataList& inp);
    void getPairResult(int node_id, const DataList& inp, RandomSemanticsScore* res);
    void getTripleResult(int node_id, TopDownGraphMatchStructure* match, const DataList& inp, RandomSemanticsScore* res);
    void popExample();
    ~TripleCache();
};

class OptimizedTripleLearnedScorer: public LearnedScorer {
    void getMatch(const PProgram& p);
public:
    TripleCache* cache_pool;
    TopDownGraphMatchStructure* match = nullptr;
    virtual void pushExample(const DataList& inp);
    virtual void popExample();
    virtual RandomSemanticsScore getScore(const PProgram& p, const Example& inp);
    OptimizedTripleLearnedScorer(Env* env, FlattenGrammar* fg, RandomSemanticsLearner* _learner);
    virtual ~OptimizedTripleLearnedScorer();
};


namespace selector::random {
    void updateViolateProb(RandomSemanticsScore* res, const std::vector<RandomSemanticsScore>& weight_list);
    void buildSetProduct(RandomSemanticsScore* res, const std::vector<RandomSemanticsScore>& weight_list);
    void buildSetProductWithMask(RandomSemanticsScore* res, int n, int mask);
    bool isTerminate(const TopDownContextGraph::Edge& edge);
    Data getTerminateOutput(const TopDownContextGraph::Edge& edge, const DataList& inp);
    LearnedScorerType getScorerType(const std::string& name);
    std::pair<LearnedScorerType, int> splitScorerName(const std::string& name, Env* env);
    LearnedScorer* buildTrivialScorer(LearnedScorerType type, Env* env, FlattenGrammar* fg);
    LearnedScorer* buildDefaultScorer(LearnedScorerType type, Env* env, FlattenGrammar* fg);
    LearnedScorer* buildVSAScorer(LearnedScorerType type, Specification* spec, FlattenGrammar* fg, const VSAEnvSetter& setter);
    void setLearnedExampleLimit(LearnedScorerType type, Env* env);
}

#endif //ISTOOL_LEARNED_SCORER_H
