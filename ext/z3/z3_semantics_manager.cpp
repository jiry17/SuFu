//
// Created by pro on 2022/1/17.
//

#include "istool/ext/z3/z3_semantics_manager.h"
#include "istool/ext/z3/z3_extension.h"


BasicZ3SemanticsManager::BasicZ3SemanticsManager(Z3Extension *_ext): ext(_ext) {}
bool BasicZ3SemanticsManager::isMatch(Semantics *semantics) {
    return dynamic_cast<ConstSemantics*>(semantics) || dynamic_cast<ParamSemantics*>(semantics);
}
Z3EncodeRes BasicZ3SemanticsManager::encodeZ3ExprForSemantics(Semantics *semantics, const std::vector<Z3EncodeRes> &inp_list, const Z3EncodeList &param_list) {
    auto* ps = dynamic_cast<ParamSemantics*>(semantics);
    if (ps) {
        return param_list[ps->id];
    }
    auto* cs = dynamic_cast<ConstSemantics*>(semantics);
    assert(cs);
    return {ext->buildConst(cs->w), {ext->ctx}};
}

bool OperatorZ3SemanticsManager::isMatch(Semantics *semantics) {
    return semantics_pool.find(semantics->name) != semantics_pool.end();
}
Z3EncodeRes OperatorZ3SemanticsManager::encodeZ3ExprForSemantics(Semantics *semantics, const std::vector<Z3EncodeRes> &inp_list, const Z3EncodeList &param_list) {
    return semantics_pool[semantics->name]->encodeZ3Expr(inp_list);
}
void OperatorZ3SemanticsManager::registerZ3Semantics(const std::string &name, Z3Semantics *semantics) {
    semantics_pool[name] = semantics;
}
OperatorZ3SemanticsManager::~OperatorZ3SemanticsManager() {
    for (const auto& info: semantics_pool) delete info.second;
}