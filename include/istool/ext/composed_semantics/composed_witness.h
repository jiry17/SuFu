//
// Created by pro on 2022/1/18.
//

#ifndef ISTOOL_COMPOSED_WITNESS_H
#define ISTOOL_COMPOSED_WITNESS_H

#include "istool/ext/vsa/witness_manager.h"
#include "witness_value_join_op.h"

class ComposedWitnessManager: public WitnessManager {
    virtual WitnessList getWitness(Program* program, const WitnessData& oup, int inp_num);
public:
    WitnessValueJoinOp* join_op;
    VSAExtension* ext;
    ComposedWitnessManager(WitnessValueJoinOp* _join_op, VSAExtension* _ext);
    virtual bool isMatch(Semantics* semantics);
    virtual WitnessList getWitness(Semantics* semantics, const WitnessData& oup, const DataList& inp_list);
    virtual ~ComposedWitnessManager();
};

namespace ext::vsa {
    void registerComposedManager(VSAExtension* ext, WitnessValueJoinOp* op);
    void registerDefaultComposedManager(VSAExtension* ext);
}

#endif //ISTOOL_COMPOSED_WITNESS_H
