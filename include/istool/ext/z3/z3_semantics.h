//
// Created by pro on 2021/12/7.
//

#ifndef ISTOOL_Z3_SEMANTICS_H
#define ISTOOL_Z3_SEMANTICS_H

#include "z3++.h"
#include "istool/basic/semantics.h"

struct Z3EncodeRes {
    z3::expr res;
    z3::expr_vector cons_list;
    std::string toString() const;
    Z3EncodeRes(const z3::expr& _res, const z3::expr_vector& _cons_list);
};

typedef std::vector<Z3EncodeRes> Z3EncodeList;

class Z3Semantics {
public:
    virtual Z3EncodeRes encodeZ3Expr(const std::vector<Z3EncodeRes>& inp_list) = 0;
    virtual ~Z3Semantics() = default;
};

typedef std::shared_ptr<Z3Semantics> PZ3Semantics;

#define DefineZ3Semantics(name) \
class Z3 ## name ## Semantics: public Z3Semantics { \
public: \
    virtual Z3EncodeRes encodeZ3Expr(const std::vector<Z3EncodeRes>& inp_list); \
    virtual ~Z3 ## name ## Semantics() = default; \
};

DefineZ3Semantics(Not)
DefineZ3Semantics(And)
DefineZ3Semantics(Or)
DefineZ3Semantics(Imply)
DefineZ3Semantics(Direct)

#define LoadZ3Semantics(ext, name, sem) ext->registerOperator(name, new Z3 ## sem ## Semantics())

class Z3Extension;

namespace ext {
    namespace z3 {
        void loadLogicSemantics(Z3Extension* ext);
    }
}


#endif //ISTOOL_Z3_SEMANTICS_H
