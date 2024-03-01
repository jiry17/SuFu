//
// Created by pro on 2022/1/7.
//

#ifndef ISTOOL_POLYGEN_H
#define ISTOOL_POLYGEN_H

#include "istool/solver/stun/stun.h"

class PolyGen: public STUNSolver {
public:
    PolyGen(Specification* spec, const PSynthInfo& term_info, const PSynthInfo& unify_info,
            const PBESolverBuilder& domain_solver, const PBESolverBuilder& dnf_builder);
    ~PolyGen() = default;
};

#endif //ISTOOL_POLYGEN_H
