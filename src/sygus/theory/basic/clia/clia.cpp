//
// Created by pro on 2022/1/26.
//

#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/basic/type_system.h"

namespace {
    const int KDefaultINF = 1e8;
}

#define LoadINFSemantics(name, sem) env->setSemantics(name, std::make_shared<sem ## Semantics>(inf))

void theory::loadCLIATheory(Env *env) {
    auto* inf = env->getConstRef(theory::clia::KINFName, Data(std::make_shared<IntValue>(KDefaultINF)));
    LoadINFSemantics("+", IntPlus);
    LoadINFSemantics("-", IntMinus);
    LoadINFSemantics("*", IntTimes);
    LoadSemantics("div", IntDiv);
    LoadSemantics("mod", IntMod);
    LoadSemantics("<", Lq); LoadSemantics("<=", Leq);
    LoadSemantics(">", Gq); LoadSemantics(">=", Geq);
    LoadSemantics("=", Eq); LoadSemantics("ite", Ite);
    LoadSemantics("!=", Neq); LoadSemantics("=b", EqBool);

    auto* ext = type::getTypeExtension(env);
    ext->registerTypeInfo(new IntValueTypeInfo());
}

void theory::clia::setIntINF(Env *env, int inf) {
    env->getConstRef(clia::KINFName, BuildData(Int, inf));
}