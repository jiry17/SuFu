//
// Created by pro on 2021/11/30.
//

#ifndef ISTOOL_VALUE_H
#define ISTOOL_VALUE_H

#include <string>
#include <memory>
#include "type.h"

class Value {
public:
    Value();
    virtual ~Value() = default;
    virtual std::string toString() const = 0;
    virtual std::string toHaskell(bool in_result = false) const = 0;
    virtual bool equal(Value* value) const = 0;
};

typedef std::shared_ptr<Value> PValue;

class ComparableValue {
public:
    virtual bool leq(Value* value) const = 0;
};

class NullValue: public Value {
public:
    NullValue();
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
};

class BoolValue: public Value {
public:
    bool w;
    BoolValue(bool _w);
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
};

#endif //ISTOOL_VALUE_H
