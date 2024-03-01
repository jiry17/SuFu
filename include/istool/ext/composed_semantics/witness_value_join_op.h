//
// Created by pro on 2022/1/18.
//

#ifndef ISTOOL_WITNESS_JOINTER_H
#define ISTOOL_WITNESS_JOINTER_H

#include "istool/ext/vsa/witness_value.h"

class WitnessValueJoinOp {
public:
    virtual WitnessData getJoin(const WitnessData& x, const WitnessData& y) const = 0;
    virtual ~WitnessValueJoinOp() = default;
};

class DefaultSyGuSWitnessValueJoinOp: public WitnessValueJoinOp {
public:
    virtual WitnessData getJoin(const WitnessData& x, const WitnessData& y) const;
    virtual ~DefaultSyGuSWitnessValueJoinOp() = default;
};

#endif //ISTOOL_WITNESS_JOINTER_H
