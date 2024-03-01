//
// Created by pro on 2022/1/22.
//

#ifndef ISTOOL_AUTOLIFTER_H
#define ISTOOL_AUTOLIFTER_H

#include "istool/solver/autolifter/basic/lifting_solver.h"
#include "istool/solver/solver.h"

class AutoLifter: public LiftingSolver {
    PProgram synthesisCombinatorForPartial(PartialLiftingTask* task, const PProgram& f, TimeGuard* guard);
    SingleLiftingRes synthesisSinglePartial(PartialLiftingTask* task, TimeGuard* guard);
    LiftingRes synthesisPartial(const PProgram& p, const PProgram& h, TimeGuard* guard);
    bool isOccur(const PProgram& p, const ProgramList& pool);
    SFSolverBuilder sf_builder;
    SolverBuilder sc_builder;
public:
    AutoLifter(LiftingTask* task, const SFSolverBuilder& _sf_builder, const SolverBuilder& _sc_builder);
    virtual LiftingRes synthesis(TimeGuard* guard);
};

#endif //ISTOOL_AUTOLIFTER_H
