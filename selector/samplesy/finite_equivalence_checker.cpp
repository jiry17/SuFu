//
// Created by pro on 2022/1/28.
//

#include "istool/selector/samplesy/finite_equivalence_checker.h"

FiniteGrammarEquivalenceChecker::FiniteGrammarEquivalenceChecker(DifferentProgramGenerator* _g, FiniteIOExampleSpace* io_space): g(_g) {
    for (auto& example: io_space->example_space) {
        auto io_example = io_space->getIOExample(example);
        example_list.push_back(io_example);
    }
}

void FiniteGrammarEquivalenceChecker::addExample(const IOExample &example) {
    g->addExample(example);
}

ProgramList FiniteGrammarEquivalenceChecker::getTwoDifferentPrograms() {
    ProgramList res;
    for (const auto& example: example_list) {
        res = g->getDifferentProgram(example, 2);
        if (res.size() == 2) return res;
    }
    return res;
}