//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_Z3_TYPE_H
#define ISTOOL_BV_Z3_TYPE_H

#include "istool/ext/z3/z3_type.h"
#include "istool/basic/env.h"

class Z3BitVectorType: public Z3Type {
public:
    virtual bool matchType(Type* type) const;
    virtual z3::expr buildConst(const Data& data, z3::context& ctx) const;
    virtual z3::expr buildVar(Type* type, const std::string& name, z3::context& ctx) const;
    virtual Data getValueFromModel(const z3::model& model, const z3::expr& expr, Type* type, bool is_strict) const;
    ~Z3BitVectorType() = default;
};

namespace theory::bv {
    void loadZ3Type(Env* env);
}

#endif //ISTOOL_BV_Z3_TYPE_H
