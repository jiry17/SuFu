//
// Created by pro on 2022/1/28.
//

#ifndef ISTOOL_FINITE_EQUIVALENCE_CHECKER_H
#define ISTOOL_FINITE_EQUIVALENCE_CHECKER_H

#include "istool/selector/selector.h"
#include "different_program_generator.h"
#include <stack>

class FiniteGrammarEquivalenceChecker: public GrammarEquivalenceChecker{
public:
    DifferentProgramGenerator* g;
    IOExampleList example_list;
    FiniteGrammarEquivalenceChecker(DifferentProgramGenerator* _g, FiniteIOExampleSpace* io_space);
    virtual void addExample(const IOExample& example);
    virtual ProgramList getTwoDifferentPrograms();
    virtual ~FiniteGrammarEquivalenceChecker() = default;
};

#endif //ISTOOL_FINITE_EQUIVALENCE_CHECKER_H
