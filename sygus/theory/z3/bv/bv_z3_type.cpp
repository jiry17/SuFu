//
// Created by pro on 2022/2/12.
//

#include "istool/sygus/theory/z3/bv/bv_z3_type.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "istool/ext/z3/z3_extension.h"
#include "glog/logging.h"

bool Z3BitVectorType::matchType(Type *type) const {
    return dynamic_cast<TBitVector*>(type);
}
z3::expr Z3BitVectorType::buildVar(Type *type, const std::string &name, z3::context &ctx) const {
    auto* bt = dynamic_cast<TBitVector*>(type);
    return ctx.bv_const(name.c_str(), bt->size);
}
z3::expr Z3BitVectorType::buildConst(const Data &data, z3::context &ctx) const {
    auto w = theory::bv::getBitVectorValue(data);
    bool* bits = new bool[w.size()];
    for (int i = 0; i < w.size(); ++i) bits[i] = w[i];
    auto res = ctx.bv_val(w.size(), bits);
    delete[] bits;
    return res;
}
Data Z3BitVectorType::getValueFromModel(const z3::model &model, const z3::expr &expr, Type *type, bool is_strict) const {
    auto* bv = dynamic_cast<TBitVector*>(type);
    int size = bv->size;
    Bitset res(bv->size, 0);
    auto b1 = model.ctx().bv_val(1, size);
    bool is_undef = false;
    for (int i = 0; i < size; ++i) {
        auto bit_expr = ((z3::lshr(expr, i) & b1) == b1);
        auto bit_res = model.eval(bit_expr);
        if (bit_res.bool_value() == Z3_L_UNDEF) {
            is_undef = true; break;
        }
        if (bit_res.bool_value() == Z3_L_TRUE) res.set(i, 1);
    }
    if (is_undef && is_strict) return Data();
    return BuildData(BitVector, res);
}

void theory::bv::loadZ3Type(Env *env) {
    auto* ext = ext::z3::getExtension(env);
    ext->registerZ3Type(new Z3BitVectorType());
}