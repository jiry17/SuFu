//
// Created by pro on 2021/12/28.
//

#ifndef ISTOOL_STRING_VALUE_H
#define ISTOOL_STRING_VALUE_H

#include "istool/basic/value.h"
#include "istool/basic/data.h"
#include "istool/basic/type_system.h"

class StringValue: public Value {
public:
    std::string s;
    StringValue(const std::string& _s);
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
};

class StringValueTypeInfo: public ValueTypeInfo {
    PType string_type;
public:
    StringValueTypeInfo();
    virtual bool isMatch(Value* value);
    virtual PType getType(Value* value);
    virtual ~StringValueTypeInfo() = default;
};

namespace theory {
    namespace string {
        std::string getStringValue(const Data& d);
    }
}

#endif //ISTOOL_STRING_VALUE_H
