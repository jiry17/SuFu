//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_Z3_SEMANTICS_H
#define ISTOOL_BV_Z3_SEMANTICS_H

#include "istool/ext/z3/z3_semantics.h"

DefineZ3Semantics(BVNeg)
DefineZ3Semantics(BVNot)
DefineZ3Semantics(BVAdd)
DefineZ3Semantics(BVSub)
DefineZ3Semantics(BVAnd)
DefineZ3Semantics(BVOr)
DefineZ3Semantics(BVLShr)
DefineZ3Semantics(BVAShr)
DefineZ3Semantics(BVShl)
DefineZ3Semantics(BVXor)

namespace theory::bv {
    void loadZ3Semantics(Env* env);
}

#endif //ISTOOL_BV_Z3_SEMANTICS_H
