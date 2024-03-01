//
// Created by pro on 2021/12/27.
//

#include "istool/ext/z3/z3_semantics.h"
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

Z3EncodeRes Z3NotSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {!inp_list[0].res, inp_list[0].cons_list};
}
Z3EncodeRes Z3AndSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res && y.res, merge(x.cons_list, addPathCondition(x.res, y.cons_list))};
}
Z3EncodeRes Z3OrSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {x.res || y.res, merge(x.cons_list, addPathCondition(!x.res, y.cons_list))};
}
Z3EncodeRes Z3ImplySemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    auto x = inp_list[0], y = inp_list[1];
    return {z3::implies(x.res, y.res), merge(x.cons_list, addPathCondition(x.res, y.cons_list))};
}
Z3EncodeRes Z3DirectSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return inp_list[0];
}

void ext::z3::loadLogicSemantics(Z3Extension *ext) {
    LoadZ3Semantics(ext, "and", And); LoadZ3Semantics(ext, "!", Not);
    LoadZ3Semantics(ext, "&&", And); LoadZ3Semantics(ext, "||", Or);
    LoadZ3Semantics(ext, "or", Or); LoadZ3Semantics(ext, "not", Not);
    LoadZ3Semantics(ext, "=>", Imply); LoadZ3Semantics(ext, "", Direct);
}