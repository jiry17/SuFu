//
// Created by pro on 2022/2/22.
//

#ifndef ISTOOL_LIMITED_INT_H
#define ISTOOL_LIMITED_INT_H

#include "limited_type.h"
#include "istool/sygus/theory/basic/clia/clia_type.h"

class LimitedTInt: public TInt, public LimitedType {
public:
    int l, r;
    LimitedTInt(int _l, int _r);
    virtual std::string getName();
    virtual PType getBaseType(const TypeList& params);
    virtual PType clone(const TypeList& params);
    virtual bool isValid(Value* value);
    virtual ~LimitedTInt() = default;
};

#endif //ISTOOL_LIMITED_INT_H
