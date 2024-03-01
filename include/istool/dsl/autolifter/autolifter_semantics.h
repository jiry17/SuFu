//
// Created by pro on 2022/2/22.
//

#ifndef ISTOOL_AUTOLIFTER_SEMANTICS_H
#define ISTOOL_AUTOLIFTER_SEMANTICS_H

#include "istool/basic/semantics.h"

class FMapSemantics: public NormalSemantics {
public:
    Type* F;
    Env* env;
    FMapSemantics(Env* _env, const PType& F);
    virtual Data run(DataList&& inp_list, ExecuteInfo* info);
    virtual ~FMapSemantics() = default;
};

#endif //ISTOOL_AUTOLIFTER_SEMANTICS_H
