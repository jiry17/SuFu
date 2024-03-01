//
// Created by pro on 2022/1/18.
//

#include "istool/ext/composed_semantics/witness_value_join_op.h"
#include "istool/sygus/theory/witness/clia/clia_wit_value.h"

WitnessData DefaultSyGuSWitnessValueJoinOp::getJoin(const WitnessData &x, const WitnessData &y) const {
    auto* xv = x.get(), *yv = y.get();
    if (dynamic_cast<TotalWitnessValue*>(xv)) return y;
    if (dynamic_cast<TotalWitnessValue*>(yv)) return x;
    auto* dxv = dynamic_cast<DirectWitnessValue*>(xv);
    if (dxv) {
        if (y->isInclude(dxv->d)) return x; else return nullptr;
    }
    auto* dyv = dynamic_cast<DirectWitnessValue*>(yv);
    if (dyv) {
        if (x->isInclude(dyv->d)) return y; else return nullptr;
    }
    auto* rx = dynamic_cast<RangeWitnessValue*>(xv);
    auto* ry = dynamic_cast<RangeWitnessValue*>(yv);
    assert(rx && ry);
    int l = std::max(rx->l, ry->l);
    int r = std::min(rx->r, ry->r);
    if (l <= r) return std::make_shared<RangeWitnessValue>(l, r);
    return nullptr;
}