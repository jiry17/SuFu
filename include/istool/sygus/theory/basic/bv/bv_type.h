//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_TYPE_H
#define ISTOOL_BV_TYPE_H

#include "istool/basic/type.h"

class TBitVector: public SimpleType {
public:
    int size;
    TBitVector(int _size);
    virtual std::string getName();
    virtual PType clone(const TypeList& param);
    virtual std::string getHaskellName();
    virtual ~TBitVector() = default;
};

namespace theory::bv {
    extern PType getTBitVector(int size);
}

#endif //ISTOOL_BV_TYPE_H
