//
// Created by pro on 2021/12/19.
//

#ifndef ISTOOL_CLIA_VALUE_H
#define ISTOOL_CLIA_VALUE_H

#include "clia_type.h"
#include "istool/basic/value.h"
#include "istool/basic/type_system.h"

class IntValue: public Value, public ComparableValue {
public:
    int w;
    IntValue(int _w);
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
    virtual bool leq(Value* value) const;
};

class IntValueTypeInfo: public ValueTypeInfo {
    PType int_type;
public:
    IntValueTypeInfo();
    virtual bool isMatch(Value* value);
    virtual PType getType(Value* value);
    virtual ~IntValueTypeInfo() = default;
};

namespace theory {
    namespace clia {
        int getIntValue(const Data& data);
    }
}

#endif //ISTOOL_CLIA_VALUE_H
