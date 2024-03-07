//
// Created by pro on 2021/12/28.
//

#include "istool/sygus/theory/basic/string/string_type.h"

std::string TString::getName() {
    return "String";
}

PType TString::clone(const TypeList &type_list) {
    return std::make_shared<TString>();
}
std::string TString::getHaskellName() {
    return getName();
}

PType theory::string::getTString() {
    static PType t_string;
    if (!t_string) t_string = std::make_shared<TString>();
    return t_string;
}