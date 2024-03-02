//
// Created by pro on 2022/2/15.
//

#include "istool/invoker/invoker.h"
#include "istool/solver/enum/enum_solver.h"

Solver * invoker::single::buildOBE(Specification *spec, Verifier *v, const InvokeConfig &config) {
    ProgramChecker* runnable = nullptr;
    runnable = config.access("runnable", runnable);
    if (!runnable) runnable = new AllValidProgramChecker();

    OBESolver* obe = new OBESolver(spec, v, runnable);
    auto* solver = new CEGISSolver(obe, v);
    return solver;
}