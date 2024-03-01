//
// Created by pro on 2021/12/29.
//

#include "istool/ext/vsa/witness_value.h"

DirectWitnessValue::DirectWitnessValue(const Data &_d): d(_d) {}
bool DirectWitnessValue::isInclude(const Data &data) const {
    return d == data;
}
std::string DirectWitnessValue::toString() const {
    return d.toString();
}

ListedWitnessValue::ListedWitnessValue(const DataList &_l): l(_l) {}
bool ListedWitnessValue::isInclude(const Data &data) const {
    for (const auto& d: l) {
        if (d == data) return true;
    }
    return false;
}
std::string ListedWitnessValue::toString() const {
    return data::dataList2String(l);
}

bool TotalWitnessValue::isInclude(const Data &data) const {
    return true;
}
std::string TotalWitnessValue::toString() const {
    return "T";
}