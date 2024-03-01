//
// Created by pro on 2021/12/20.
//

#ifndef ISTOOL_CLIA_Z3_SEMANTICS_H
#define ISTOOL_CLIA_Z3_SEMANTICS_H

#include "istool/ext/z3/z3_semantics.h"
#include "istool/basic/env.h"

DefineZ3Semantics(IntPlus)
DefineZ3Semantics(IntMinus)
DefineZ3Semantics(IntTimes)
DefineZ3Semantics(IntDiv)
DefineZ3Semantics(IntMod)
DefineZ3Semantics(Lq)
DefineZ3Semantics(Leq)
DefineZ3Semantics(Gq)
DefineZ3Semantics(Geq)
DefineZ3Semantics(Eq)
DefineZ3Semantics(Neq)
DefineZ3Semantics(Ite)

namespace theory::clia {
    void loadZ3Semantics(Env *env);
}



#endif //ISTOOL_CLIA_Z3_SEMANTICS_H
