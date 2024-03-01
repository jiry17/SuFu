//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_VALUE_H
#define ISTOOL_BV_VALUE_H

#include "istool/basic/value.h"
#include "istool/basic/type_system.h"
#include "istool/basic/bitset.h"
#include "unordered_map"

class BitVectorValue: public Value {
public:
    Bitset w;
    BitVectorValue(const Bitset& _w);
    virtual std::string toString() const;
    virtual std::string toHaskell(bool in_result) const;
    virtual bool equal(Value* value) const;
    virtual ~BitVectorValue() = default;
};

class BitVectorTypeInfo: public ValueTypeInfo {
    std::unordered_map<int, PType> type_map;
public:
    virtual bool isMatch(Value* value);
    virtual PType getType(Value* value);
    virtual ~BitVectorTypeInfo() = default;
};

namespace theory::bv {
    Bitset getBitVectorValue(const Data& data);
}

#endif //ISTOOL_BV_VALUE_H
