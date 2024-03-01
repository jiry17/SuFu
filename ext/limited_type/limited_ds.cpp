//
// Created by pro on 2022/2/22.
//

#include "istool/ext/limited_type/limited_ds.h"
#include "istool/ext/deepcoder/data_value.h"
#include "glog/logging.h"

LimitedTList::LimitedTList(int _max_size, const PType &content):
    max_size(_max_size), TList(content) {
}
PType LimitedTList::clone(const TypeList &type_list) {
    return std::make_shared<LimitedTList>(max_size, type_list[0]);
}
std::string LimitedTList::getName() {
    return "List[" + std::to_string(max_size) + "][" + content->getName() + "]";
}
PType LimitedTList::getBaseType(const TypeList &params) {
    return std::make_shared<TList>(params[0]);
}
bool LimitedTList::isValid(Value *value) {
    auto* lv = dynamic_cast<ListValue*>(value);
    if (!lv) {
        LOG(FATAL) << "LimitedTList: Expect ListValue, btu get " << value->toString();
    }
    return lv->value.size() <= max_size;
}