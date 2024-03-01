//
// Created by pro on 2022/2/15.
//

#include "istool/invoker/invoker.h"
#include "istool/solver/enum/enum_solver.h"
#include "istool/solver/tmp/weighted_obe.h"

Solver * invoker::single::buildOBE(Specification *spec, Verifier *v, const InvokeConfig &config) {
    ProgramChecker* runnable = nullptr;
    runnable = config.access("runnable", runnable);
    if (!runnable) runnable = new AllValidProgramChecker();

    OBESolver* obe = nullptr;
    if (config.access("isWeighted", false)) {
        obe = new WeightedOBESolver(spec, v, runnable);
    } else obe = new OBESolver(spec, v, runnable);
    auto* solver = new CEGISSolver(obe, v);
    return solver;
}