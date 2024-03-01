//
// Created by pro on 2022/2/22.
//

#include "istool/ext/limited_type/limited_int.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "glog/logging.h"

LimitedTInt::LimitedTInt(int _l, int _r): l(_l), r(_r)  {}
std::string LimitedTInt::getName() {
    return "Int[" + std::to_string(l) + "," + std::to_string(r) + "]";
}
PType LimitedTInt::getBaseType(const TypeList& params) {
    return theory::clia::getTInt();
}
bool LimitedTInt::isValid(Value *value) {
    auto* iv = dynamic_cast<IntValue*>(value);
    if (!iv) {
        LOG(FATAL) << "LimitedTInt: Expect IntValue, but get " << value->toString();
    }
    return iv->w >= l && iv->w <= r;
}
PType LimitedTInt::clone(const TypeList& params) {
    return std::make_shared<LimitedTInt>(l, r);
}