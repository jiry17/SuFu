//
// Created by pro on 2022/5/20.
//

#ifndef ISTOOL_GUROBI_H
#define ISTOOL_GUROBI_H

#include "gurobi_c++.h"
#include "istool/basic/env.h"

class GRBExtension: public Extension {
public:
    GRBEnv env;
    int KIntLimit;
    GRBExtension(int _KIntLimit);
    GRBVar encodeBoolProgram(Program* program, const std::vector<GRBVar>& param_list, GRBModel& model);
    GRBLinExpr encodeIntProgram(Program* program, const std::vector<GRBVar>& param_list, GRBModel& model);
    GRBLinExpr encodeProgram(Program* program, Type* type, const std::vector<GRBVar>& param_list, GRBModel& model);
    GRBLinExpr buildConst(const Data& data);
    GRBVar buildVar(Type* type, GRBModel& model, const std::string& name = "");
    Data getValueFromModel(double w, Type* type);
    Data getValueFromModel(const GRBVar& var, Type* type);
};

namespace ext::gurobi {
    extern const std::string KIntLimitName;
    GRBExtension* getExtension(Env* env);
}


#endif //ISTOOL_GUROBI_H
