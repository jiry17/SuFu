//
// Created by pro on 2021/11/30.
//

#include "istool/basic/value.h"
#include "glog/logging.h"

Value::Value() {}
NullValue::NullValue() {}
bool NullValue::equal(Value *value) const {
    auto* nv = dynamic_cast<NullValue*>(value);
    return nv;
}
std::string NullValue::toString() const {
    return "null";
}
std::string NullValue::toHaskell(bool in_result = false) const {
    return toString();
}

BoolValue::BoolValue(bool _w): w(_w){}
bool BoolValue::equal(Value *value) const {
    auto* bv = dynamic_cast<BoolValue*>(value);
    if (!bv) {
        LOG(FATAL) << "Expected BoolValue, but get " << value->toString();
    }
    return bv->w == w;
}
std::string BoolValue::toString() const {
    return w ? "true" : "false";
}
std::string BoolValue::toHaskell(bool in_result = false) const {
    return w ? "(toSym True)" : "(toSym False)";
}