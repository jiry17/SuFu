//
// Created by pro on 2021/12/20.
//

#include "istool/sygus/theory/basic/theory_semantics.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "glog/logging.h"

void theory::loadBasicSemantics(Env *env, TheoryToken token) {
    switch (token) {
        case TheoryToken::CLIA: {
            theory::loadCLIATheory(env);
            return;
        }
        case TheoryToken::STRING: {
            theory::loadStringTheory(env);
            return;
        }
        case TheoryToken::BV: {
            theory::loadBVTheory(env);
            return;
        }
        default:
            LOG(FATAL) << "Unsupported theory";
    }
}