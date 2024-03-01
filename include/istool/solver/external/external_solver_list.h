//
// Created by pro on 2022/5/24.
//

#ifndef ISTOOL_EXTERNAL_EUSOLVER_H
#define ISTOOL_EXTERNAL_EUSOLVER_H

#include "external_solver.h"

class ExternalEuSolver: public ExternalSolver {
public:
    ExternalEuSolver(Env* env, const std::string& path="");
    virtual FunctionContext invoke(const std::string &benchmark_file, TimeGuard *guard);
};

class ExternalCVC5: public ExternalSolver {
public:
    ExternalCVC5(Env* env, const std::string& path="");
    virtual FunctionContext invoke(const std::string& benchmark_file, TimeGuard* guard);
};

#endif //ISTOOL_EXTERNAL_EUSOLVER_H
