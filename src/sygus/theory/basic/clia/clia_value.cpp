//
// Created by pro on 2021/12/19.
//

#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "glog/logging.h"


IntValue::IntValue(int _w): w(_w) {
}
bool IntValue::equal(Value *value) const {
    auto* iv = dynamic_cast<IntValue*>(value);
    if (!iv) return false;
    return iv->w == w;
}
std::string IntValue::toString() const {
    return std::to_string(w);
}
std::string IntValue::toHaskell(bool in_result = false) const {
    std::string res = "(" + toString() + ")";
    // return "int" + res;
    return res;
}
bool IntValue::leq(Value *value) const {
    auto* iv = dynamic_cast<IntValue*>(value);
    if (!iv) {
        LOG(FATAL) << "Expect IntValue, but get " << value->toString();
    }
    return w <= iv->w;
}

IntValueTypeInfo::IntValueTypeInfo(): int_type(std::make_shared<TInt>()) {}
bool IntValueTypeInfo::isMatch(Value *value) {
    return dynamic_cast<IntValue*>(value);
}
PType IntValueTypeInfo::getType(Value *value) {
    return int_type;
}

int theory::clia::getIntValue(const Data &data) {
    auto* iv = dynamic_cast<IntValue*>(data.get());
    if (!iv) {
        throw SemanticsError();
        // LOG(FATAL) << "Expected IntValue, but get " << data.toString();
    }
    return iv->w;
}