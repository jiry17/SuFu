//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_SEMANTICS_H
#define ISTOOL_BV_SEMANTICS_H

#include "istool/basic/semantics.h"

#define DefineBVSemantics(name) \
class name ## Semantics : public NormalSemantics { \
    int size; \
public: \
    name ## Semantics(int _size); \
    virtual Data run(DataList &&inp_list, ExecuteInfo *info); \
    ~name ## Semantics() = default; \
};

DefineBVSemantics(BVNeg)
DefineBVSemantics(BVNot)
DefineBVSemantics(BVAdd)
DefineBVSemantics(BVSub)
DefineBVSemantics(BVAnd)
DefineBVSemantics(BVOr)
DefineBVSemantics(BVLShr)
DefineBVSemantics(BVAShr)
DefineBVSemantics(BVShl)
DefineBVSemantics(BVXor)

#endif //ISTOOL_BV_SEMANTICS_H
