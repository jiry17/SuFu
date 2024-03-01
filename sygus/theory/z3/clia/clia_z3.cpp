//
// Created by pro on 2021/12/20.
//

#include "istool/sygus/theory/z3/clia/clia_z3.h"
#include "istool/sygus/theory/z3/clia/clia_z3_semantics.h"
#include "istool/sygus/theory/z3/clia/clia_z3_type.h"

void theory::loadZ3CLIA(Env *env) {
    theory::clia::loadZ3Type(env);
    theory::clia::loadZ3Semantics(env);
}