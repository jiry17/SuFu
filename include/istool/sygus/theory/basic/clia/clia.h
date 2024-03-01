//
// Created by pro on 2021/12/20.
//

#ifndef ISTOOL_CLIA_H
#define ISTOOL_CLIA_H

#include "istool/sygus/theory/basic/clia/clia_type.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"

namespace theory {
    namespace clia {
        extern const std::string KINFName;
        void setIntINF(Env* env, int inf);
    }
    void loadCLIATheory(Env* env);
}

#endif //ISTOOL_CLIA_H
