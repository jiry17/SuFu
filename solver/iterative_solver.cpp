//
// Created by pro on 2022/1/5.
//

#include "istool/solver/solver.h"

Solver * solver::relaxSolver(Solver *solver, TimeGuard *guard) {
    auto* is = dynamic_cast<IterativeSolver*>(solver);
    if (is) return (Solver*)(is->relax(guard));
    return nullptr;
}

PBESolver * solver::relaxSolver(PBESolver *solver, TimeGuard *guard) {
    auto* is = dynamic_cast<IterativeSolver*>(solver);
    if (is) return (PBESolver*)(is->relax(guard));
    return nullptr;
}