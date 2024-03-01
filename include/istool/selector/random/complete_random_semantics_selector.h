//
// Created by pro on 2022/5/12.
//

#ifndef ISTOOL_COMPLETE_RANDOM_SEMANTICS_SELECTOR_H
#define ISTOOL_COMPLETE_RANDOM_SEMANTICS_SELECTOR_H

#include "istool/selector/selector.h"
#include "random_semantics_selector.h"

class FiniteCompleteRandomSemanticsSelector: public CompleteSelector, public BasicRandomSemanticsSelector {
public:
    FiniteIOExampleSpace* fio_space;
    IOExampleList io_example_list;
    DifferentProgramGenerator* g;
    int KExampleNum;
    FiniteCompleteRandomSemanticsSelector(Specification* spec, GrammarEquivalenceChecker* _checker, LearnedScorer* scorer, DifferentProgramGenerator* g, int _KExampleSum=1000);
    virtual Example getNextExample(const PProgram& x, const PProgram& y);
    virtual void addExample(const IOExample& example);
    ~FiniteCompleteRandomSemanticsSelector();
};

class Z3CompleteRandomSemanticsSelector: public CompleteSelector, public BasicRandomSemanticsSelector {
    Example getExampleFromInput(const Example& inp_list);
public:
    Z3IOExampleSpace* zio_space;
    IOExampleList io_example_list;
    Z3Extension* ext;
    z3::expr_vector param_list, inp_var_list;
    z3::expr_vector cons_list;
    int KExampleNum;
    Z3CompleteRandomSemanticsSelector(Specification* spec, GrammarEquivalenceChecker* _checker, LearnedScorer* scorer, Program* example_cons, int _KExampleNum);
    virtual Example getNextExample(const PProgram& x, const PProgram& y);
    virtual void addExample(const IOExample& example);
    ~Z3CompleteRandomSemanticsSelector() = default;
};

#endif //ISTOOL_COMPLETE_RANDOM_SEMANTICS_SELECTOR_H
