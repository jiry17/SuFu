//
// Created by pro on 2021/12/18.
//

#ifndef ISTOOL_Z3_TYPE_H
#define ISTOOL_Z3_TYPE_H

#include "istool/basic/data.h"

class Z3Type {
public:
    virtual bool matchType(Type* type) const = 0;
    virtual z3::expr buildConst(const Data& data, z3::context& ctx) const = 0;
    virtual z3::expr buildVar(Type* type, const std::string& name, z3::context& ctx) const = 0;
    virtual Data getValueFromModel(const z3::model& model, const z3::expr& expr, Type* type, bool is_strict) const = 0;
    virtual ~Z3Type() = default;
};


#endif //ISTOOL_Z3_TYPE_H
