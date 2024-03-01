//
// Created by pro on 2021/12/18.
//

#ifndef ISTOOL_Z3_EXTENSION_H
#define ISTOOL_Z3_EXTENSION_H

#include "istool/basic/env.h"
#include "istool/basic/type_system.h"
#include "istool/basic/program.h"
#include "istool/basic/time_guard.h"
#include "z3_type.h"
#include "z3_semantics_manager.h"

#include <list>

class Z3Extension: public Extension {
    std::list<Z3Type*> util_list;
    std::list<Z3SemanticsManager*> semantics_list;
    Z3Type* getZ3Type(Type* type) const;
    Env* env;
    TypeExtension* type_ext;
public:
    Z3Extension(Env* env);
    void registerZ3Type(Z3Type* util);
    void registerSemanticsManager(Z3SemanticsManager* manager);
    void registerOperator(const std::string& name, Z3Semantics* semantics);
    z3::context ctx;

    z3::expr buildVar(Type* type, const std::string& name);
    z3::expr buildConst(const Data& data);
    Z3EncodeRes encodeZ3ExprForSemantics(Semantics* semantics, const Z3EncodeList& inp_list, const Z3EncodeList& param_list);
    Z3EncodeRes encodeZ3ExprForProgram(Program* program, const Z3EncodeList& param_list);
    Z3EncodeRes encodeZ3ExprForConsProgram(Program* program, const FunctionContext& info, const Z3EncodeList& param_list);
    Data getValueFromModel(const z3::model& model, const z3::expr& expr, Type* type, bool is_strict = false);
    void setTimeOut(z3::solver& solver, TimeGuard* guard);
    virtual ~Z3Extension();
};

namespace ext::z3 {
    Z3Extension* getExtension(Env* env);
    Z3EncodeList z3Vector2EncodeList(const ::z3::expr_vector& expr_list);
}


#endif //ISTOOL_Z3_EXTENSION_H
