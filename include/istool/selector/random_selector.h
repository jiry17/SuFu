//
// Created by pro on 2022/2/6.
//

#ifndef ISTOOL_RANDOM_SELECTOR_H
#define ISTOOL_RANDOM_SELECTOR_H

#include "istool/selector/samplesy/different_program_generator.h"
#include "selector.h"
#include "istool/basic/example_sampler.h"
#include "istool/ext/z3/z3_verifier.h"

class FiniteRandomSelector: public CompleteSelector {
public:
    FiniteIOExampleSpace* finite_io_space;
    Env* env;
    DifferentProgramGenerator* g;
    FiniteRandomSelector(Specification* spec, GrammarEquivalenceChecker* _checker, DifferentProgramGenerator* _g);
    virtual Example getNextExample(const PProgram& x, const PProgram& y);
    virtual void addExample(const IOExample& example);
    ~FiniteRandomSelector() = default;
};

class Z3RandomSelector: public CompleteSelector {
public:
    ExampleGenerator* generator;
    int KSampleNum;
    Z3Verifier* verifier;
    IOExampleSpace* io_space;
    Env* env;
    Z3RandomSelector(Specification* spec, GrammarEquivalenceChecker* _checker, Z3Verifier* verifier, ExampleGenerator* _generator, int _KSampleNum=1000);
    virtual Example getNextExample(const PProgram& x, const PProgram& y);
    virtual void addExample(const IOExample& example);
    ~Z3RandomSelector();
};

#endif //ISTOOL_RANDOM_SELECTOR_H
