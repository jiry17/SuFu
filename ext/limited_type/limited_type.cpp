//
// Created by pro on 2022/2/22.
//

#include "istool/ext/limited_type/limited_type.h"
#include "glog/logging.h"

RefinedType::RefinedType(const PType &_content, const ValuePredicate &_p): content(_content), p(_p) {}
PType RefinedType::getBaseType(const TypeList &params) {
    return params[0];
}
bool RefinedType::isValid(Value *value) {
    return p(value);
}
TypeList RefinedType::getParams() {
    return {content};
}
std::string RefinedType::getName() {
    return content->getName() + "@p";
}
PType RefinedType::clone(const TypeList &params) {
    return std::make_shared<RefinedType>(params[0], p);
}
std::string RefinedType::getHaskellName() {
    return getName();
}
int RefinedType::getTupleLen() {return 1;}

bool RefinedType::equal(Type *type) {
    LOG(FATAL) << "Method equal for RefinedType is undefined";
}
std::string RefinedType::getBaseName() {
    LOG(FATAL) << "Method getBaseName for RefinedType is undefined";
}

PType ext::ltype::getBaseType(Type *type) {
    TypeList params;
    for (auto& sub_type: type->getParams()) {
        params.push_back(getBaseType(sub_type.get()));
    }
    auto* lt = dynamic_cast<LimitedType*>(type);
    if (lt) return lt->getBaseType(params);
    return type->clone(params);
}