//
// Created by pro on 2022/1/7.
//

#include "istool/solver/polygen/polygen.h"
#include "istool/solver/polygen/polygen_term_solver.h"
#include "istool/solver/polygen/polygen_unifier.h"

PolyGen::PolyGen(Specification *spec, const PSynthInfo &term_info, const PSynthInfo &unify_info,
        const PBESolverBuilder &domain_solver, const PBESolverBuilder &dnf_builder):
        STUNSolver(spec, term_info, unify_info,
                [=](Specification *spec, const PSynthInfo &info) -> TermSolver * {return new PolyGenTermSolver(spec, info, domain_solver);},
                [=](Specification *spec, const PSynthInfo &info) -> Unifier* {return new PolyGenUnifier(spec, info, dnf_builder);}) {
}