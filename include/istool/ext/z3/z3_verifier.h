//
// Created by pro on 2021/12/7.
//

#ifndef ISTOOL_Z3_VERIFIER_H
#define ISTOOL_Z3_VERIFIER_H

#include "istool/basic/verifier.h"
#include "z3_example_space.h"

class Z3Verifier: public Verifier {
public:
    z3::expr_vector getParamVector();
    void prepareZ3Solver(z3::solver& solver, const FunctionContext& info);
    void getExample(const z3::model& model, Example* counter_example);
    Z3ExampleSpace* example_space;
    Z3Extension* ext;
    Z3Verifier(Z3ExampleSpace* _example_space);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~Z3Verifier() = default;
};

#endif //ISTOOL_Z3_VERIFIER_H
