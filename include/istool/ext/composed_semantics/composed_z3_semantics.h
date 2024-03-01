//
// Created by pro on 2022/1/18.
//

#ifndef ISTOOL_COMPOSED_Z3_SEMANTICS_H
#define ISTOOL_COMPOSED_Z3_SEMANTICS_H

#include "istool/ext/z3/z3_semantics_manager.h"

class ComposedZ3SemanticsManager: public Z3SemanticsManager {
public:
    Z3Extension* ext;
    ComposedZ3SemanticsManager(Z3Extension* _ext);
    virtual bool isMatch(Semantics* semantics);
    virtual Z3EncodeRes encodeZ3ExprForSemantics(Semantics* semantics, const std::vector<Z3EncodeRes>& inp_list, const Z3EncodeList& param_list);
    virtual ~ComposedZ3SemanticsManager() = default;
};

namespace ext::z3 {
    void registerComposedManager(Z3Extension* ext);
}

#endif //ISTOOL_COMPOSED_Z3_SEMANTICS_H
