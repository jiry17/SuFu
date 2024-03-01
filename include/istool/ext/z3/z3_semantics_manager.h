//
// Created by pro on 2022/1/17.
//

#ifndef ISTOOL_Z3_SEMANTICS_MANAGER_H
#define ISTOOL_Z3_SEMANTICS_MANAGER_H

#include "z3_semantics.h"

class Z3Extension;

class Z3SemanticsManager {
public:
    virtual bool isMatch(Semantics* semantics) = 0;
    virtual Z3EncodeRes encodeZ3ExprForSemantics(Semantics* semantics, const std::vector<Z3EncodeRes>& inp_list, const Z3EncodeList& param_list) = 0;
    virtual ~Z3SemanticsManager() = default;
};

class BasicZ3SemanticsManager: public Z3SemanticsManager {
public:
    Z3Extension* ext;
    BasicZ3SemanticsManager(Z3Extension* _ext);
    virtual bool isMatch(Semantics* semantics);
    virtual Z3EncodeRes encodeZ3ExprForSemantics(Semantics* semantics, const std::vector<Z3EncodeRes>& inp_list, const Z3EncodeList& param_list);
    virtual ~BasicZ3SemanticsManager() = default;
};

class OperatorZ3SemanticsManager: public Z3SemanticsManager {
public:
    std::unordered_map<std::string, Z3Semantics*> semantics_pool;
    void registerZ3Semantics(const std::string& name, Z3Semantics* semantics);
    virtual bool isMatch(Semantics* semantics);
    virtual Z3EncodeRes encodeZ3ExprForSemantics(Semantics* semantics, const std::vector<Z3EncodeRes>& inp_list, const Z3EncodeList& param_list);
    virtual ~OperatorZ3SemanticsManager();
};

#endif //ISTOOL_Z3_SEMANTICS_MANAGER_H
