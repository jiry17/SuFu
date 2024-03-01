//
// Created by pro on 2022/2/12.
//

#ifndef ISTOOL_BV_H
#define ISTOOL_BV_H

#include "bv_type.h"
#include "bv_value.h"
#include "istool/basic/env.h"

namespace theory {
    namespace bv {
        void setBitVectorLength(Env *env, int length);
        Data* getBitVectorLengthData(Env* env);
        int getBitVectorLength(Env* env);
    }
    void loadBVTheory(Env* env);
}

#endif //ISTOOL_BV_H
