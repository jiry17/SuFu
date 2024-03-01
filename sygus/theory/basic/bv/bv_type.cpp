//
// Created by pro on 2022/2/12.
//

#include "istool/sygus/theory/basic/bv/bv_type.h"

std::string TBitVector::getName() {
    return "BitVector[" + std::to_string(size) + "]";
}
TBitVector::TBitVector(int _size): size(_size) {}
PType TBitVector::clone(const TypeList &param) {
    return std::make_shared<TBitVector>(size);
}
std::string TBitVector::getHaskellName() {
    return getName();
}

PType theory::bv::getTBitVector(int size) {
    return std::make_shared<TBitVector>(size);
}
