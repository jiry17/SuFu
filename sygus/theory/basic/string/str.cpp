//
// Created by pro on 2022/1/26.
//

#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/sygus/theory/basic/string/string_semantics.h"
#include "istool/basic/type_system.h"
#include "glog/logging.h"

void theory::loadStringTheory(Env *env) {
    auto* inf = env->getConstRef(theory::clia::KINFName);
    LoadSemantics("str.++", StringCat); LoadSemantics("str.len", StringLen);
    LoadSemantics("str.at", StringAt); LoadSemantics("str.substr", StringSubStr);
    LoadSemantics("str.prefixof", StringPrefixOf); LoadSemantics("str.suffixof", StringSuffixOf);
    LoadSemantics("str.contains", StringContains); LoadSemantics("str.indexof", StringIndexOf);
    LoadSemantics("str.replace", StringReplace); LoadSemantics("int.to.str", IntToStr);
    env->setSemantics("str.to.int", std::make_shared<StrToIntSemantics>(inf));

    auto* ext = type::getTypeExtension(env);
    ext->registerTypeInfo(new StringValueTypeInfo());
}

