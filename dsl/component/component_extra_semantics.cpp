//
// Created by pro on 2022/2/13.
//

#include "istool/dsl/component/component_extra_semantics.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/z3/z3_extension.h"
#include "istool/basic/bitset.h"
#include "glog/logging.h"

typedef unsigned long long ULL;

namespace {
    ULL _bv2ull(const Bitset& w) {
        if (w.size() > 64) {
            LOG(FATAL) << "The current implementation of BVTheory cannot support Bitset larger than 64, but get " << w.size();
        }
        ULL res = 0;
        for (int i = w.size(); i; --i) {
            res = (res << 1) + w[i - 1];
        }
        return res;
    }

    Bitset _ull2bv(ULL w, int size) {
        Bitset res;
        for (int i = 0; i < size; ++i) {
            res.append(w & 1); w >>= 1;
        }
        return res;
    }

#define TBV(size) (theory::bv::getTBitVector(size))
    using theory::bv::getBitVectorValue;
}

BVIncSemantics::BVIncSemantics(int _size): size(_size), NormalSemantics("bvinc", TBV(_size), {TBV(_size)}) {}
Data BVIncSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = _bv2ull(getBitVectorValue(inp_list[0]));
    return BuildData(BitVector, _ull2bv(x + 1, size));
}
BVDecSemantics::BVDecSemantics(int _size): size(_size), NormalSemantics("bvdec", TBV(_size), {TBV(_size)}) {}
Data BVDecSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = _bv2ull(getBitVectorValue(inp_list[0]));
    return BuildData(BitVector, _ull2bv(x - 1, size));
}
BVAShr31Semantics::BVAShr31Semantics(int _size): size(_size), NormalSemantics("bvashr31", TBV(_size), {TBV(_size)}) {}
Data BVAShr31Semantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]); int y = std::min(31, size);
    Bitset res(size, 0);
    for (int i = y; i < size; ++i) {
        res.set(i - y, x[i]);
    }
    auto sign = x[size - 1];
    for (int i = size - int(y); i < size; ++i) {
        res.set(i, sign);
    }
    return BuildData(BitVector, res);
}
BVULeqSemantics::BVULeqSemantics(int _size): size(_size), NormalSemantics("bvuleq", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVULeqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]), y = getBitVectorValue(inp_list[1]);
    auto res = Bitset(size, 0); res.set(0, 1);
    for (int i = x.size(); i > 0; --i) {
        if (x[i - 1] > y[i - 1]) {
            res.set(0, 0); break;
        }
        if (x[i - 1] < y[i - 1]) break;
    }
    return BuildData(BitVector, res);
}
BVULqSemantics::BVULqSemantics(int _size): size(_size), NormalSemantics("bvulq", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVULqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]), y = getBitVectorValue(inp_list[1]);
    auto res = Bitset(size, 0);
    for (int i = x.size(); i > 0; --i) {
        if (x[i - 1] > y[i - 1]) break;
        if (x[i - 1] < y[i - 1]) {
            res.set(0, 1); break;
        }
    }
    return BuildData(BitVector, res);
}
BVEqSemantics::BVEqSemantics(int _size): size(_size), NormalSemantics("bveq", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVEqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto res = Bitset(size, 0);
    if (inp_list[0] == inp_list[1]) res.set(0, 1);
    return BuildData(BitVector, res);
}
BVUShrKSemantics::BVUShrKSemantics(int _size, int _k): size(_size), k(_k), NormalSemantics("bvushr" + std::to_string(_k), TBV(_size), {TBV(_size)}) {}
Data BVUShrKSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]);
    auto res = Bitset(size, 0);
    for (int i = k; i < size; ++i) res.set(i - k, x[i]);
    return BuildData(BitVector, res);
}

namespace {
    z3::expr_vector _mergeConsList(const z3::expr_vector& x, const z3::expr_vector& y) {
        auto res = x;
        for (const auto& w: y) res.push_back(w);
        return res;
    }
}


Z3BVIncSemantics::Z3BVIncSemantics(Data *_size_data): size_data(_size_data) {}
Z3EncodeRes Z3BVIncSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    int size = theory::clia::getIntValue(*size_data);
    auto& ctx = inp_list[0].res.ctx();
    return {inp_list[0].res + ctx.bv_val(1, size), inp_list[0].cons_list};
}
Z3BVDecSemantics::Z3BVDecSemantics(Data *_size_data): size_data(_size_data) {}
Z3EncodeRes Z3BVDecSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    int size = theory::clia::getIntValue(*size_data);
    auto& ctx = inp_list[0].res.ctx();
    return {inp_list[0].res - ctx.bv_val(1, size), inp_list[0].cons_list};
}
Z3EncodeRes Z3BVAShr31Semantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {z3::ashr(inp_list[0].res, 31), inp_list[0].cons_list};
}
Z3BVULeqSemantics::Z3BVULeqSemantics(Data *_size_data): size_data(_size_data) {}
Z3EncodeRes Z3BVULeqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    int size = theory::clia::getIntValue(*size_data);
    auto& ctx = inp_list[0].res.ctx();
    return {z3::ite(z3::ule(inp_list[0].res, inp_list[1].res), ctx.bv_val(1, size), ctx.bv_val(0, size)),
            _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3BVULqSemantics::Z3BVULqSemantics(Data *_size_data): size_data(_size_data) {}
Z3EncodeRes Z3BVULqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    int size = theory::clia::getIntValue(*size_data);
    auto& ctx = inp_list[0].res.ctx();
    return {z3::ite(z3::ult(inp_list[0].res, inp_list[1].res), ctx.bv_val(1, size), ctx.bv_val(0, size)),
            _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3BVEqSemantics::Z3BVEqSemantics(Data *_size_data): size_data(_size_data) {}
Z3EncodeRes Z3BVEqSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    int size = theory::clia::getIntValue(*size_data);
    auto& ctx = inp_list[0].res.ctx();
    return {z3::ite(inp_list[0].res == inp_list[1].res, ctx.bv_val(1, size), ctx.bv_val(0, size)),
            _mergeConsList(inp_list[0].cons_list, inp_list[1].cons_list)};
}
Z3BVUShrKSemantics::Z3BVUShrKSemantics(int _k): k(_k) {}
Z3EncodeRes Z3BVUShrKSemantics::encodeZ3Expr(const std::vector<Z3EncodeRes> &inp_list) {
    return {z3::lshr(inp_list[0].res, k), inp_list[0].cons_list};
}

void dsl::component::registerExtraComponent(Env *env) {
    int size = theory::bv::getBitVectorLength(env);
    env->setSemantics("bvinc", std::make_shared<BVIncSemantics>(size));
    env->setSemantics("bvdec", std::make_shared<BVDecSemantics>(size));
    env->setSemantics("bvashr31", std::make_shared<BVAShr31Semantics>(size));
    env->setSemantics("bvuleq", std::make_shared<BVULeqSemantics>(size));
    env->setSemantics("bvulq", std::make_shared<BVULqSemantics>(size));
    env->setSemantics("bveq", std::make_shared<BVEqSemantics>(size));
    for (int k: {1, 2, 4, 8, 16}) {
        env->setSemantics("bvushr" + std::to_string(k), std::make_shared<BVUShrKSemantics>(size, k));
    }

    auto* ext = ext::z3::getExtension(env);
    auto* size_data = theory::bv::getBitVectorLengthData(env);
    ext->registerOperator("bvinc", new Z3BVIncSemantics(size_data));
    ext->registerOperator("bvdec", new Z3BVDecSemantics(size_data));
    ext->registerOperator("bvashr31", new Z3BVAShr31Semantics());
    ext->registerOperator("bvuleq", new Z3BVULeqSemantics(size_data));
    ext->registerOperator("bvulq", new Z3BVULqSemantics(size_data));
    ext->registerOperator("bveq", new Z3BVEqSemantics(size_data));
    for (int k: {1, 2, 4, 8, 16}) {
        ext->registerOperator("bvushr" + std::to_string(k), new Z3BVUShrKSemantics(k));
    }
}