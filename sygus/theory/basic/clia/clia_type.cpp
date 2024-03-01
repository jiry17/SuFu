//
// Created by pro on 2021/12/19.
//

#include "istool/sygus/theory/basic/clia/clia_type.h"

std::string TInt::getName() {
    return "Int";
}
PType TInt::clone(const TypeList &params) {
    return std::make_shared<TInt>();
}
std::string TInt::getHaskellName() {
    return "SymInteger";
}

PType theory::clia::getTInt() {
    static PType int_type = nullptr;
    if (!int_type) {
        int_type = std::make_shared<TInt>();
    }
    return int_type;
}