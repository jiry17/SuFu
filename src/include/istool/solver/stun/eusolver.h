//
// Created by pro on 2022/1/4.
//

#ifndef ISTOOL_EUSOLVER_H
#define ISTOOL_EUSOLVER_H

#include "stun.h"

class EuTermSolver: public TermSolver {
public:
    EuTermSolver(Specification* spec, const PSynthInfo& info);
    virtual ProgramList synthesisTerms(const ExampleList& example_list, TimeGuard* guard = nullptr);
    virtual ~EuTermSolver() = default;
};

class EuUnifier: public Unifier {
public:
    IOExampleSpace* io_space;
    EuUnifier(Specification* spec, const PSynthInfo& info);
    virtual PProgram unify(const ProgramList& term_list, const ExampleList& example_list, TimeGuard* guard = nullptr);
    virtual ~EuUnifier() = default;
};

class EuSolver: public STUNSolver {
public:
    EuSolver(Specification* _spec, const PSynthInfo& tg, const PSynthInfo& cg);
    virtual ~EuSolver() = default;
};

#endif //ISTOOL_EUSOLVER_H
