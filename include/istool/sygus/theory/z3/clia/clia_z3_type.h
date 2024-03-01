//
// Created by pro on 2021/12/20.
//

#ifndef ISTOOL_CLIA_Z3_TYPE_H
#define ISTOOL_CLIA_Z3_TYPE_H

#include "istool/ext/z3/z3_type.h"
#include "istool/basic/env.h"

class Z3IntType: public Z3Type {
public:
    virtual bool matchType(Type* type) const;
    virtual z3::expr buildConst(const Data& data, z3::context& ctx) const;
    virtual z3::expr buildVar(Type* type, const std::string& name, z3::context& ctx) const;
    virtual Data getValueFromModel(const z3::model& model, const z3::expr& expr, Type* type, bool is_strict) const;
    ~Z3IntType() = default;
};

class Z3BoolType: public Z3Type {
public:
    virtual bool matchType(Type* type) const;
    virtual z3::expr buildConst(const Data& data, z3::context& ctx) const;
    virtual z3::expr buildVar(Type* type, const std::string& name, z3::context& ctx) const;
    virtual Data getValueFromModel(const z3::model& model, const z3::expr& expr, Type* type, bool is_strict) const;
};

namespace theory {
    namespace clia {
        void loadZ3Type(Env* env);
    }
}

#endif //ISTOOL_CLIA_Z3_TYPE_H
