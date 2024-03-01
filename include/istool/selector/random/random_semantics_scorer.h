//
// Created by pro on 2022/4/27.
//

#ifndef ISTOOL_RANDOM_SEMANTICS_SCORER_H
#define ISTOOL_RANDOM_SEMANTICS_SCORER_H

#include "istool/selector/selector.h"
#include "istool/selector/random/grammar_flatter.h"
#include "istool/solver/maxflash/topdown_context_graph.h"
#include "istool/ext/vsa/vsa_extension.h"
#include "istool/ext/vsa/top_down_model.h"

typedef long double RandomSemanticsScore;

namespace selector::random {
    class TerminateEdgeInfo {
    public:
        double weight;
        TerminateEdgeInfo(double _weight);
        virtual Data getOutput(const DataList& inp) const = 0;
        virtual ~TerminateEdgeInfo() = default;
    };

    class ConstTerminateEdgeInfo: public TerminateEdgeInfo {
    public:
        Data w;
        ConstTerminateEdgeInfo(const Data& _w, double _weight);
        virtual Data getOutput(const DataList& inp) const;
        virtual ~ConstTerminateEdgeInfo() = default;
    };

    class ParamTerminateEdgeInfo: public TerminateEdgeInfo {
    public:
        int param_id;
        ParamTerminateEdgeInfo(int _param_id, double _weight);
        virtual Data getOutput(const DataList& inp) const;
        virtual ~ParamTerminateEdgeInfo() = default;
    };

    class PairMatchInfoCache {
        void initRes();
    public:
        std::vector<TerminateEdgeInfo*> edge_list;
        std::vector<std::vector<int>> match_info;
        RandomSemanticsScore * res;
        int example_num;
        PairMatchInfoCache(const TopDownContextGraph::Node& node);
        int getEdgeIdForSemantics(Semantics* semantics);
        void pushExample(const DataList& inp);
        void popExample();
        void getTmpPairStateWeight(const DataList& inp, RandomSemanticsScore* tmp);
        void getOneSideMatchWeight(Semantics* sem, RandomSemanticsScore* tmp);
        ~PairMatchInfoCache();
    };

    class TripleMatchInfoCache {
        static std::vector<int> K2T5[5];
        static std::vector<int> K5Size;
        static void prepare2T5(int example_num);
        int get5State(int fp, int gp, int fg) const;
    public:
        std::vector<TerminateEdgeInfo*> edge_list;
        std::vector<std::vector<int>> match_info;
        int example_num;
        int p_id;
        RandomSemanticsScore* res;
        TripleMatchInfoCache(PairMatchInfoCache* two_cache, Semantics* s);
        void getTmpTripleStateWeight(const DataList& inp, RandomSemanticsScore* tmp);
        ~TripleMatchInfoCache();
    };

    class CachePool {
    public:
        TopDownContextGraph* graph;
        std::vector<PairMatchInfoCache*> pair_cache_list;
        std::unordered_map<std::string, TripleMatchInfoCache*> triple_cache_map;
        CachePool(TopDownContextGraph* _graph);
        void pushExample(const Example& inp);
        void getTripleMatchRes(int node_id, Semantics* s, const Example& inp, RandomSemanticsScore* res);
        void getPairMatchRes(int node_id, const Example& inp, RandomSemanticsScore* res);
        void getOneSizePairMatchRes(int node_id, Semantics* s, RandomSemanticsScore* res);
        void popExample();
        ~CachePool();
    };
}

class RandomSemanticsScorer {
public:
    RandomSemanticsScore getTripleScore(const PProgram& p, const DataStorage& inp_list);
    RandomSemanticsScore getPairScore(const DataStorage& inp_list);
    DataStorage getFlattenInpStorage(const DataStorage& inp_list);
    double KOutputSize;
    FlattenGrammar* fg;
    Env* env;
    RandomSemanticsScorer(Env* _env, FlattenGrammar* _fg, double _KOutputSize);
    ~RandomSemanticsScorer();
};

class IncrementalRandomSemanticsScorer {
public:
    virtual RandomSemanticsScore getTripleScore(const PProgram& p, const Example& inp) = 0;
    virtual RandomSemanticsScore getPairScore(const Example& inp) = 0;
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    int example_num;
    FlattenGrammar* fg;
    Env* env;
    IncrementalRandomSemanticsScorer(Env* env, FlattenGrammar* _fg);
    virtual ~IncrementalRandomSemanticsScorer();
};

typedef std::function<IncrementalRandomSemanticsScorer*(Env* env, FlattenGrammar* fg)> CachedRandomSemanticsScorerBuilder;

class BasicCachedRandomSemanticsScorer: public IncrementalRandomSemanticsScorer {
public:
    RandomSemanticsScore KOutputSize;
    selector::random::CachePool* cache;
    virtual RandomSemanticsScore getTripleScore(const PProgram& p, const Example& inp);
    virtual RandomSemanticsScore getPairScore(const Example& inp);
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    BasicCachedRandomSemanticsScorer(Env* env, FlattenGrammar* _fg, RandomSemanticsScore _KOutputSize);
    ~BasicCachedRandomSemanticsScorer();
};

namespace selector::random {
    typedef float WeightType;
    typedef std::vector<std::vector<WeightType>> EqualWeightMatrix;
    typedef std::pair<Data, DataList> FullOutput;

    class SampleOutputChecker {
    public:
        virtual void setInput(const DataList& inp) = 0;
        virtual bool check(Semantics* sem, const FullOutput& oup) = 0;
        virtual ~SampleOutputChecker() = default;
    };

    class DefaultSampleOutputChecker: public SampleOutputChecker {
    public:
        virtual void setInput(const DataList& inp);
        virtual bool check(Semantics* sem, const FullOutput& oup);
        virtual ~DefaultSampleOutputChecker() = default;
    };

    class VSASampleOutputChecker: public SampleOutputChecker {
    public:
        VSAEnvSetter setter;
        Grammar* grammar;
        Env* env;
        VSAExtension* ext;
        VSASampleOutputChecker(Env* _env, Grammar* _grammar, const VSAEnvSetter& _setter);
        virtual void setInput(const DataList& inp);
        virtual bool check(Semantics* sem, const FullOutput& oup);
        virtual ~VSASampleOutputChecker() = default;
    };
}

class RandomSemanticsModel {
public:
    std::vector<selector::random::EqualWeightMatrix> weight_list;
    TopDownContextGraph* graph;
    RandomSemanticsModel(TopDownContextGraph* _graph, const std::vector<selector::random::EqualWeightMatrix>& _weight_matrix);
    void print(bool is_nt=true);
    ~RandomSemanticsModel() = default;
};

class RandomSemanticsLearner {
public:
    FlattenGrammar* fg;
    TopDownContextGraph* graph;
    Env* env;
    int KSampleNum;
    RandomSemanticsLearner(Env* _env, FlattenGrammar* _fg);
    virtual RandomSemanticsModel* learn(const DataList& inp) = 0;
    virtual ~RandomSemanticsLearner() = default;
};

class BasicRandomSemanticsLearner: public RandomSemanticsLearner {
    std::vector<std::discrete_distribution<int>> dist_list;
    std::vector<int> node_order;
    std::vector<std::vector<std::vector<selector::random::FullOutput>>> sample_res;
    Data run(Semantics* sem, const DataList& sub_oup, const DataList& inp);
    Data sampleProgram(int node_id, const DataList& inp);
    Data sampleProgram(int node_id, int edge_id, const DataList& inp);
    selector::random::SampleOutputChecker* checker;
public:
    BasicRandomSemanticsLearner(Env* _env, FlattenGrammar* _fg, selector::random::SampleOutputChecker* _checker = nullptr);
    virtual RandomSemanticsModel* learn(const DataList& inp);
    ~BasicRandomSemanticsLearner() = default;
};

class SampleStructureHolder {
public:
    struct SampleStructure {
        TopDownContextGraph::Edge* edge;
        std::vector<SampleStructure*> sub_list;
        selector::random::FullOutput oup;
        int index;
        SampleStructure(TopDownContextGraph::Edge* _edge, const std::vector<SampleStructure*>& _sub_list, int _index);
        Data execute(const DataList& inp);
        PProgram getProgram();
    };
    std::vector<std::vector<std::vector<SampleStructure*>>> sample_storage;
    std::vector<std::discrete_distribution<int>> dist_list;
    FlattenGrammar* fg;
    TopDownContextGraph* graph;
    std::vector<int> node_order;
    Env* env;
    int sample_index = 0;
    std::unordered_map<std::string, SampleStructure*> sample_cache;
    virtual SampleStructure* buildStructure(int node_id, int edge_id, const std::vector<SampleStructure*>& sub_list);
    virtual void setOutput(const DataList& inp) = 0;
    virtual void initSample(int sample_num) = 0;
    SampleStructureHolder(Env* _env, FlattenGrammar* _fg);
    virtual ~SampleStructureHolder();
};

class BasicSampleStructureHolder: public SampleStructureHolder {
    SampleStructure* sampleProgram(int node_id);
    SampleStructure* sampleProgram(int node_id, int edge_id);
public:
    BasicSampleStructureHolder(Env* _env, FlattenGrammar* _fg);
    virtual void setOutput(const DataList& inp);
    virtual void initSample(int sample_num);
};

class VSASampleStructureHolder: public SampleStructureHolder {
    SampleStructure* sampleProgram(int node_id);
    SampleStructure* sampleProgram(int node_id, int edge_id);
    bool isInsideVSA(Semantics* sem, int example_id, const Data& oup, const DataList& inp);
    Data run(Semantics* sem, const DataList& sub_res, int inp_id);
    virtual SampleStructure* buildStructure(int node_id, int edge_id, const std::vector<SampleStructure*>& sub_list);
public:
    IOExampleList example_list;
    ExampleList flatten_input_list;
    VSAEnvSetter setter;
    Grammar* init_grammar;
    std::unordered_map<std::string, int> example_index_map;
    VSAExtension* ext;
    DataStorage oup_storage;
    std::unordered_map<std::string, WitnessList> witness_cache;
    VSASampleStructureHolder(Specification* spec, FlattenGrammar* _fg, const VSAEnvSetter& _setter);
    virtual void setOutput(const DataList& inp);
    virtual void initSample(int sample_num);
};

class FixedSampleRandomSemanticsLearner: public RandomSemanticsLearner {
public:
    SampleStructureHolder* holder;
    FixedSampleRandomSemanticsLearner(SampleStructureHolder* holder);
    virtual RandomSemanticsModel* learn(const DataList& inp);
    ~FixedSampleRandomSemanticsLearner();
};

class LearnedCachedSemanticsScorer: public IncrementalRandomSemanticsScorer {
public:
    RandomSemanticsLearner* learner;
    RandomSemanticsModel* getModel(const DataList& inp);
    virtual RandomSemanticsScore getTripleScore(const PProgram& p, const Example& inp);
    virtual RandomSemanticsScore getPairScore(const Example& inp);
    virtual void pushExample(const Example& inp);
    virtual void popExample();
    std::unordered_map<std::string, RandomSemanticsModel*> model_cache;
    std::vector<RandomSemanticsModel*> model_list;
    LearnedCachedSemanticsScorer(Env* _env, FlattenGrammar* _fg, RandomSemanticsLearner* learner);
    ~LearnedCachedSemanticsScorer();
};

namespace selector::random {
    extern const std::string KModelSampleNumName;
    extern const std::string KIsRequireInsideVSAName;
}

#endif //ISTOOL_RANDOM_SEMANTICS_SCORER_H
