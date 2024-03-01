//
// Created by pro on 2022/1/17.
//

#ifndef ISTOOL_WITNESS_MANAGER_H
#define ISTOOL_WITNESS_MANAGER_H

#include "witness.h"
#include "istool/basic/semantics.h"

class WitnessManager {
public:
    virtual bool isMatch(Semantics* semantics) = 0;
    virtual WitnessList getWitness(Semantics* semantics, const WitnessData& oup, const DataList& inp_list) = 0;
    virtual ~WitnessManager() = default;
};

class BasicWitnessManager: public WitnessManager {
public:
    virtual bool isMatch(Semantics* semantics);
    virtual WitnessList getWitness(Semantics* semantics, const WitnessData& oup, const DataList& inp_list);
    virtual ~BasicWitnessManager() = default;
};

class OperatorWitnessManager: public WitnessManager {
public:
    std::unordered_map<std::string, WitnessFunction*> witness_pool;
    void registerWitness(const std::string& name, WitnessFunction* witness);
    virtual bool isMatch(Semantics* semantics);
    virtual WitnessList getWitness(Semantics* semantics, const WitnessData& oup, const DataList& inp_list);
    virtual ~OperatorWitnessManager();
};



#endif //ISTOOL_WITNESS_MANAGER_H
