//
// Created by pro on 2021/12/30.
//

#include "istool/sygus/theory/witness/clia/clia_wit_value.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

bool RangeWitnessValue::isInclude(const Data &d) const {
    int w = theory::clia::getIntValue(d);
    return l <= w && w <= r;
}
std::string RangeWitnessValue::toString() const {
    return "[" + std::to_string(l) + "," + std::to_string(r) + "]";
}
RangeWitnessValue::RangeWitnessValue(int _l, int _r): l(_l), r(_r) {
    if (l + 1 > r) {
        LOG(FATAL) << "The size of a RangeWitnessValue must be at least 2";
    }
}

WitnessData theory::clia::buildRange(int l, int r) {
    if (l > r) {
        LOG(FATAL) << "Empty range [" << l << "," << r << "]";
    }
    if (l == r) return BuildDirectWData(Int, l);
    return std::make_shared<RangeWitnessValue>(l, r);
}

std::pair<int, int> theory::clia::extractRange(const WitnessData &d) {
    auto* dv = dynamic_cast<DirectWitnessValue*>(d.get());
    if (dv) {
        int w = clia::getIntValue(dv->d);
        return {w, w};
    }
    auto* rv = dynamic_cast<RangeWitnessValue*>(d.get());
    if (rv) return {rv->l, rv->r};
    LOG(FATAL) << "extractRange: witness data " << d->toString() << " is not a range";
}