//
// Created by pro on 2022/1/17.
//

#include "istool/ext/vsa/witness_manager.h"

bool BasicWitnessManager::isMatch(Semantics *semantics) {
    return dynamic_cast<ConstSemantics*>(semantics) || dynamic_cast<ParamSemantics*>(semantics);
}
WitnessList BasicWitnessManager::getWitness(Semantics *semantics, const WitnessData &oup, const DataList &inp_list) {
    auto* cs = dynamic_cast<ConstSemantics*>(semantics);
    if (cs) {
        if (oup->isInclude(cs->w)) return {{}};
        return {};
    }
    auto* ps = dynamic_cast<ParamSemantics*>(semantics);
    assert(ps);
    if (oup->isInclude(inp_list[ps->id])) return {{}};
    return {};
}

bool OperatorWitnessManager::isMatch(Semantics *semantics) {
    return witness_pool.find(semantics->name) != witness_pool.end();
}
void OperatorWitnessManager::registerWitness(const std::string &name, WitnessFunction *witness) {
    witness_pool[name] = witness;
}
WitnessList OperatorWitnessManager::getWitness(Semantics *semantics, const WitnessData &oup, const DataList &inp_list) {
    auto* wf = witness_pool[semantics->name];
    return wf->witness(oup);
}
OperatorWitnessManager::~OperatorWitnessManager() {
    for (const auto& info: witness_pool) {
        delete info.second;
    }
}