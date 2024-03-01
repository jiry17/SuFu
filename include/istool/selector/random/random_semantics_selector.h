//
// Created by pro on 2022/5/3.
//

#ifndef ISTOOL_RANDOM_SEMANTICS_SELECTOR_H
#define ISTOOL_RANDOM_SEMANTICS_SELECTOR_H

#include "istool/selector/selector.h"
#include "istool/selector/random/random_semantics_scorer.h"
#include "istool/selector/random/learned_scorer.h"
#include "istool/ext/z3/z3_verifier.h"
#include "istool/ext/gurobi/gurobi_verifier.h"
#include "istool/selector/samplesy/different_program_generator.h"


// TODO: AdaptiveRandomSemanticsSelector & BasicRandomSemanticsSelector should be used as a field instead of a base class.
class BasicRandomSemanticsSelector {
public:
    Env* env;
    Grammar* g;
    LearnedScorer* scorer;
    int example_num, KExampleNumLimit;
    BasicRandomSemanticsSelector(Env* _env, Grammar* _g, LearnedScorer* _scorer);
    int getBestExampleId(const PProgram& program, const ExampleList& candidate_list);
    void addHistoryExample(const Example& inp);
    ~BasicRandomSemanticsSelector();
};

class AdaptiveRandomSemanticsSelector {
    void initScorer();
public:
    Env* env;
    Grammar* g;
    int current_depth, KExampleNumLimit;
    bool KIsPairScore;
    FlattenGrammarBuilder* builder;
    LearnedScorer* scorer;
    ExampleList example_list;
    LearnedScorerBuilder scorer_builder;
    AdaptiveRandomSemanticsSelector(Env* env, FlattenGrammarBuilder* builder, const LearnedScorerBuilder& _scorer_builder);
    int getBestExampleId(const PProgram& program, const ExampleList& candidate_list);
    void addHistoryExample(const Example& inp);
    ~AdaptiveRandomSemanticsSelector();
};

class FiniteRandomSemanticsSelector: public Verifier, public AdaptiveRandomSemanticsSelector {
public:
    FiniteIOExampleSpace* io_space;
    IOExampleList io_example_list;
    FiniteRandomSemanticsSelector(Specification* spec, FlattenGrammarBuilder* builder, const LearnedScorerBuilder& _scorer_builder);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~FiniteRandomSemanticsSelector() = default;
};

class Z3RandomSemanticsSelector: public Z3Verifier, public AdaptiveRandomSemanticsSelector {
public:
    Z3IOExampleSpace* z3_io_space;
    int KExampleNum;
    Z3RandomSemanticsSelector(Specification* spec, FlattenGrammarBuilder* builder, const LearnedScorerBuilder& _scorer_builder, int _KExampleNum);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~Z3RandomSemanticsSelector() = default;
};

class GurobiRandomSemanticsSelector: public GRBIOVerifier, public AdaptiveRandomSemanticsSelector {
public:
    int KExampleNum;
    GurobiRandomSemanticsSelector(Specification* spec, FlattenGrammarBuilder* builder, const LearnedScorerBuilder& _scorer_builder, int _KExampleNum);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~GurobiRandomSemanticsSelector() = default;
};

namespace selector::random {
    extern const std::string KExampleNumLimitName;
}

#endif //ISTOOL_RANDOM_SEMANTICS_SELECTOR_H
