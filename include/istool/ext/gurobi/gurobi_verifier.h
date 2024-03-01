//
// Created by pro on 2022/5/21.
//

#ifndef ISTOOL_GUROBI_VERIFIER_H
#define ISTOOL_GUROBI_VERIFIER_H

#include "istool/basic/verifier.h"
#include "gurobi_extension.h"
#include "istool/ext/z3/z3_verifier.h"

class GRBIOVerifier: public Verifier {
protected:
    void prepareModel(GRBModel& model, Program* p, const std::vector<GRBVar>& param_list, const GRBVar& oup, const std::vector<GRBVar>& var_list);
    Example getExample(const std::vector<GRBVar>& var_list);
public:
    Z3IOExampleSpace* zio_space;
    GRBExtension* ext;
    GRBIOVerifier(Z3IOExampleSpace* _zio_space);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~GRBIOVerifier() = default;
};

#endif //ISTOOL_GUROBI_VERIFIER_H
