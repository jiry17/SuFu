//
// Created by pro on 2022/1/8.
//

#ifndef ISTOOL_POLYGEN_CONDITION_SOLVER_H
#define ISTOOL_POLYGEN_CONDITION_SOLVER_H

#include "istool/solver/solver.h"

class PolyGenConditionSolver {
public:
    PBESolverBuilder builder;
    PSynthInfo info;
    Specification* spec;
    bool KIsUseTerm;
    PolyGenConditionSolver(Specification *spec, const PSynthInfo &_info, const PBESolverBuilder &_builder);
    ~PolyGenConditionSolver() = default;
    PProgram getCondition(const ProgramList& term_list, const IOExampleList& pos_list,
                          const IOExampleList& neg_list, TimeGuard* guard);
};

namespace solver {
    namespace polygen {
        extern const std::string KIsUseTermName;
    }
}

#endif //ISTOOL_POLYGEN_CONDITION_SOLVER_H
