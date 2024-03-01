//
// Created by pro on 2021/12/30.
//

#ifndef ISTOOL_CLIA_WIT_VALUE_H
#define ISTOOL_CLIA_WIT_VALUE_H

#include "istool/ext/vsa/witness_value.h"

class RangeWitnessValue: public WitnessValue {
public:
    int l, r;
    RangeWitnessValue(int _l, int _r);
    virtual std::string toString() const;
    virtual bool isInclude(const Data& d) const;
    ~RangeWitnessValue() = default;
};

namespace theory {
    namespace clia {
        WitnessData buildRange(int l, int r);
        std::pair<int, int> extractRange(const WitnessData& d);
    }
}

#endif //ISTOOL_CLIA_WIT_VALUE_H
