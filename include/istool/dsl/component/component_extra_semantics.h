//
// Created by pro on 2022/2/13.
//

#ifndef ISTOOL_COMPONENT_EXTRA_SEMANTICS_H
#define ISTOOL_COMPONENT_EXTRA_SEMANTICS_H

#include "istool/sygus/theory/basic/bv/bv_semantics.h"
#include "istool/ext/z3/z3_semantics.h"

DefineBVSemantics(BVInc)
DefineBVSemantics(BVDec)
DefineBVSemantics(BVAShr31)
DefineBVSemantics(BVULeq)
DefineBVSemantics(BVULq)
DefineBVSemantics(BVEq)
class BVUShrKSemantics: public NormalSemantics {
    int size, k;
public:
    BVUShrKSemantics(int _size, int _k);
    virtual Data run(DataList &&inp_list, ExecuteInfo* info);
    ~BVUShrKSemantics() = default;
};

#define DefineZ3SemanticsWithSize(name) \
class Z3 ## name ## Semantics: public Z3Semantics { \
    Data* size_data; \
public: \
    Z3 ## name ## Semantics(Data* _size_data); \
    virtual Z3EncodeRes encodeZ3Expr(const std::vector<Z3EncodeRes>& inp_list); \
    virtual ~Z3 ## name ## Semantics() = default; \
};

DefineZ3SemanticsWithSize(BVDec)
DefineZ3SemanticsWithSize(BVInc)
DefineZ3Semantics(BVAShr31)
DefineZ3SemanticsWithSize(BVULeq)
DefineZ3SemanticsWithSize(BVULq)
DefineZ3SemanticsWithSize(BVEq)
class Z3BVUShrKSemantics: public Z3Semantics {
    int k;
public:
    Z3BVUShrKSemantics(int _k);
    virtual Z3EncodeRes encodeZ3Expr(const std::vector<Z3EncodeRes>& inp_list);
    virtual ~Z3BVUShrKSemantics() = default;
};

namespace dsl::component {
    void registerExtraComponent(Env* env);
}


#endif //ISTOOL_COMPONENT_EXTRA_SEMANTICS_H
