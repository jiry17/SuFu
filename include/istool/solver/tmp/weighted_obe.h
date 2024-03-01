//
// Created by pro on 2022/8/2.
//

#ifndef ISTOOL_WEIGHTED_OBE_H
#define ISTOOL_WEIGHTED_OBE_H

#include "istool/solver/enum/enum_solver.h"

class WeightedOBESolver: public OBESolver {
public:
    WeightedOBESolver(Specification* _spec, Verifier* _v, ProgramChecker* _is_runnable);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    virtual ~WeightedOBESolver() = default;
};

#endif //ISTOOL_WEIGHTED_OBE_H
