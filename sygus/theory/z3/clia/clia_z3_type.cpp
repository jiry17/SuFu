//
// Created by pro on 2021/12/20.
//

#include "istool/sygus/theory/z3/clia/clia_z3_type.h"
#include "istool/ext/z3/z3_extension.h"
#include "istool/sygus/theory/basic/clia/clia_type.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"

bool Z3IntType::matchType(Type *type) const {
    return dynamic_cast<TInt*>(type);
}
Data Z3IntType::getValueFromModel(const z3::model &model, const z3::expr &expr, Type *type, bool is_strict) const {
    auto value = model.eval(expr);
    try {
        return Data(std::make_shared<IntValue>(value.get_numeral_int()));
    } catch (z3::exception& e) {
        if (is_strict) return Data();
        return Data(std::make_shared<IntValue>(0));
    }
}
z3::expr Z3IntType::buildConst(const Data &data, z3::context &ctx) const {
    return ctx.int_val(theory::clia::getIntValue(data));
}
z3::expr Z3IntType::buildVar(Type *type, const std::string &name, z3::context &ctx) const {
    return ctx.int_const(name.c_str());
}

bool Z3BoolType::matchType(Type *type) const {
    return dynamic_cast<TBool*>(type);
}
Data Z3BoolType::getValueFromModel(const z3::model &model, const z3::expr &expr, Type *type, bool is_strict) const {
    auto value = model.eval(expr);
    if (value.bool_value() == Z3_L_TRUE) {
        return BuildData(Bool, true);
    }
    if (value.bool_value() == Z3_L_FALSE) {
        return BuildData(Bool, false);
    }
    if (is_strict) return Data();
    return Data(std::make_shared<BoolValue>(true));
}
z3::expr Z3BoolType::buildConst(const Data &data, z3::context &ctx) const {
    return ctx.bool_val(data.isTrue());
}
z3::expr Z3BoolType::buildVar(Type *type, const std::string &name, z3::context &ctx) const {
    return ctx.bool_const(name.c_str());
}

void theory::clia::loadZ3Type(Env *env) {
    auto* ext = ext::z3::getExtension(env);
    ext->registerZ3Type(new Z3IntType());
    ext->registerZ3Type(new Z3BoolType());
}