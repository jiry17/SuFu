//
// Created by pro on 2022/2/22.
//

#ifndef ISTOOL_LIMITED_TYPE_H
#define ISTOOL_LIMITED_TYPE_H

#include "istool/basic/value.h"

class LimitedType {
public:
    virtual bool isValid(Value* value) = 0;
    virtual PType getBaseType(const TypeList& params) = 0;
    virtual ~LimitedType() = default;
};

typedef std::function<bool(Value*)> ValuePredicate;

class RefinedType: public Type, public LimitedType {
public:
    ValuePredicate p;
    PType content;
    RefinedType(const PType& _content, const ValuePredicate& _p = [](Value*){return true;});
    virtual bool isValid(Value* value);
    virtual PType getBaseType(const TypeList& params);
    virtual std::string getName();
    virtual bool equal(Type* type);
    virtual std::string getBaseName();
    virtual TypeList getParams();
    virtual PType clone(const TypeList& params);
    virtual std::string getHaskellName();
    virtual int getTupleLen();
    virtual ~RefinedType() = default;
};

namespace ext::ltype {
    PType getBaseType(Type* type);
}

#endif //ISTOOL_LIMITED_TYPE_H
