//
// Created by pro on 2022/2/16.
//

#include "istool/invoker/invoker.h"
#include "istool/solver/polygen/lia_solver.h"
#include "istool/solver/polygen/dnf_learner.h"
#include "istool/solver/polygen/polygen_cegis.h"
#include "istool/solver/stun/stun.h"

Solver * invoker::single::buildPolyGen(Specification *spec, Verifier *v, const InvokeConfig &config) {
    auto stun_info = solver::divideSyGuSSpecForSTUN(spec->info_list[0], spec->env.get());

    TermSolver* term_solver;
    auto* d = spec->env->getConstRef(solver::lia::KIsGurobiName, BuildData(Bool, true));
    if (d->isTrue()) {
        LOG(INFO) << "Build Normal";
        term_solver = new PolyGenTermSolver(spec, stun_info.first, solver::lia::getLIASolver);
    } else {
        LOG(INFO) << "Build Enumerate";
        term_solver = new EnumeratePolyGenTermSolver(spec, stun_info.first);
    }
    auto* cond_solver = new PolyGenConditionSolver(spec, stun_info.second, [](Specification* spec) -> PBESolver* {return new DNFLearner(spec);});

    auto is_staged = config.access("is_staged", false);
    if (is_staged) {
        return new StagedCEGISPolyGen(spec, term_solver, cond_solver);
    } else {
        return new CEGISPolyGen(spec, term_solver, cond_solver, v);
    }
}

Solver* invoker::single::buildCondSolver(Specification *spec, Verifier *v, const InvokeConfig &config) {
    auto* dnf_learner = new DNFLearner(spec);
    auto* solver = new CEGISSolver(dnf_learner, v);
    return solver;
}

Solver* invoker::single::buildLIASolver(Specification* spec, Verifier* v, const InvokeConfig& config) {
    auto* pbe_solver = solver::lia::getLIASolver(spec);
    auto* cegis_solver = new CEGISSolver(pbe_solver, v);
    return cegis_solver;
}