//
// Created by pro on 2022/2/12.
//

#include "istool/sygus/theory/z3/bv/bv_z3_semantics.h"
#include "istool/sygus/theory/z3/clia/clia_z3_semantics.h"
#include "istool/ext/z3/z3_extension.h"

namespace {
    z3::expr_vector _mergeConsList(const z3::expr_vector& x, const z3::expr_vector& y) {
        z3::expr_vector res = x;
        for (int i = 0; i < y.size(); ++i) res.push_back(y[i]);
        return res;
    }
}

Z3EncodeRes Z3BVNegSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {-inp_list[0].res, inp_list[0].cons_list};
}
Z3EncodeRes Z3BVNotSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {~inp_list[0].res, inp_list[0].cons_list};
}
Z3EncodeRes Z3BVAddSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {inp_list[0].res + inp_list[1].res, _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVSubSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {inp_list[0].res - inp_list[1].res, _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVAndSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {inp_list[0].res & inp_list[1].res, _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVOrSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {inp_list[0].res | inp_list[1].res, _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVLShrSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {z3::lshr(inp_list[0].res, inp_list[1].res), _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVShlSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {z3::shl(inp_list[0].res, inp_list[1].res), _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVXorSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {inp_list[0].res ^ inp_list[1].res, _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3EncodeRes Z3BVAShrSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {z3::ashr(inp_list[0].res, inp_list[1].res), _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
void theory::bv::loadZ3Semantics(Env *env) {
    auto* z3_env = ext::z3::getExtension(env);
    LoadZ3Semantics(z3_env, "bvneg", BVNeg); LoadZ3Semantics(z3_env, "bvnot", BVNot);
    LoadZ3Semantics(z3_env, "bvadd", BVAdd); LoadZ3Semantics(z3_env, "bvsub", BVSub);
    LoadZ3Semantics(z3_env, "bvand", BVAnd); LoadZ3Semantics(z3_env, "bvor", BVOr);
    LoadZ3Semantics(z3_env, "bvlshr", BVLShr); LoadZ3Semantics(z3_env, "bvshl", BVShl);
    LoadZ3Semantics(z3_env, "ite", Ite); LoadZ3Semantics(z3_env, "=", Eq);
    LoadZ3Semantics(z3_env, "!=", Neq);
    LoadZ3Semantics(z3_env, "bvxor", BVXor); LoadZ3Semantics(z3_env, "bvashr", BVAShr);
}