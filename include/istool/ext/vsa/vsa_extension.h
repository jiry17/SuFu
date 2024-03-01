//
// Created by pro on 2021/12/29.
//

#ifndef ISTOOL_VSA_EXTENSION_H
#define ISTOOL_VSA_EXTENSION_H

#include "istool/basic/env.h"
#include "istool/basic/grammar.h"
#include "istool/basic/example_space.h"
#include "witness_manager.h"
#include <list>

typedef std::function<void(Grammar*, Env*, const IOExample&)> VSAEnvSetter;

class VSAExtension: public Extension {
public:
    std::list<WitnessManager*> manager_list;
    VSAEnvSetter env_setter;
    Env* env;
    VSAExtension(Env* _env);
    void setEnvSetter(const VSAEnvSetter& _setter);
    void prepareEnv(Grammar* grammar, const IOExample& example);
    void registerWitnessManager(WitnessManager* manager);
    void registerWitnessFunction(const std::string& name, WitnessFunction* semantics);
    WitnessList getWitness(Semantics* semantics, const WitnessData& oup, const DataList& inp_list) const;
    virtual ~VSAExtension();
};

namespace ext::vsa {
    VSAExtension* getExtension(Env* env);
    bool isConsideredByVSA(Program* p, VSAExtension* ext, Grammar* g, const IOExample& example);
}

#endif //ISTOOL_VSA_EXTENSION_H
