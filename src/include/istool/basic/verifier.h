//
// Created by pro on 2021/12/5.
//

#ifndef ISTOOL_VERIFIER_H
#define ISTOOL_VERIFIER_H

#include "program.h"
#include "example_space.h"
#include "specification.h"

class Verifier {
public:
    virtual bool verify(const FunctionContext& info, Example* counter_example) = 0;
    virtual ~Verifier() = default;
};

typedef std::shared_ptr<Verifier> PVerifier;

class FiniteExampleVerifier: public Verifier {
public:
    FiniteExampleSpace* example_space;
    FiniteExampleVerifier(FiniteExampleSpace* _space);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~FiniteExampleVerifier() = default;
};

#endif //ISTOOL_VERIFIER_H
