//
// Created by pro on 2022/1/7.
//

#ifndef ISTOOL_POLYGEN_UNIFIER_H
#define ISTOOL_POLYGEN_UNIFIER_H

#include "istool/solver/stun/stun.h"
#include "istool/solver/polygen/polygen_condition_solver.h"

class PolyGenUnifier: public Unifier {
    PolyGenConditionSolver* solver;
    IOExampleSpace* io_space;
    public:
    PolyGenUnifier(Specification* spec, const PSynthInfo& info, const PBESolverBuilder& _builder);
    virtual PProgram unify(const ProgramList& term_list, const ExampleList& example_list, TimeGuard* guard = nullptr);
    virtual ~PolyGenUnifier();
};

#endif //ISTOOL_POLYGEN_UNIFIER_H
