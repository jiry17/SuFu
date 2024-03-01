//
// Created by pro on 2021/12/20.
//

#include "istool/sygus/theory/z3/clia/clia_z3_semantics.h"
#include "istool/ext/z3/z3_extension.h"

namespace {
    z3::expr_vector merge(const z3::expr_vector& x, const z3::expr_vector& y) {
        z3::expr_vector res = x;
        for (const auto& cons: y) res.push_back(cons);
        return res;
    }

    z3::expr_vector addPathCondition(const z3::expr& x, const z3::expr_vector& y) {
        z3::expr_vector res(y.ctx());
        for (const auto& cons: y) {
            res.push_back(z3::implies(x, cons));
        }
        return res;
    }
}

Z3EncodeRes Z3IntPlusSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res + y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3IntMinusSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res - y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3IntTimesSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res * y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3IntDivSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    auto res = x.res / y.res; auto cons = merge(x.cons_list, y.cons_list);
    cons.push_back(y.res != 0);
    return {res, cons};
}
Z3EncodeRes Z3IntModSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    auto res = x.res % y.res; auto cons = merge(x.cons_list, y.cons_list);
    cons.push_back(y.res != 0);
    return {res, cons};
}
Z3EncodeRes Z3LqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res < y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3LeqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res <= y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3GqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res > y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3GeqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res >= y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3EqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res == y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3NeqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res != y.res, merge(x.cons_list, y.cons_list)};
}
Z3EncodeRes Z3IteSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    assert(inp_list.size() == 3);
    auto b = inp_list[0], x = inp_list[1], y = inp_list[2];
    assert(b.res.is_bool());
    return {z3::ite(b.res, x.res, y.res), merge(b.cons_list,
            merge(addPathCondition(b.res, x.cons_list), addPathCondition(!b.res, y.cons_list)))};
}

void theory::clia::loadZ3Semantics(Env *env) {
    auto* z3_env = ext::z3::getExtension(env);
    LoadZ3Semantics(z3_env, "+", IntPlus); LoadZ3Semantics(z3_env, "-", IntMinus);
    LoadZ3Semantics(z3_env, "*", IntTimes); LoadZ3Semantics(z3_env, "div", IntDiv);
    LoadZ3Semantics(z3_env, "mod", IntMod); LoadZ3Semantics(z3_env, "<", Lq);
    LoadZ3Semantics(z3_env, "<=", Leq); LoadZ3Semantics(z3_env, ">", Gq);
    LoadZ3Semantics(z3_env, ">=", Geq); LoadZ3Semantics(z3_env, "=", Eq);
    LoadZ3Semantics(z3_env, "!=", Neq); LoadZ3Semantics(z3_env, "ite", Ite);
    LoadZ3Semantics(z3_env, "=b", Eq);
}