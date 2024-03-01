//
// Created by pro on 2021/12/29.
//

#include "istool/ext/vsa/vsa_extension.h"
#include "glog/logging.h"

VSAExtension::VSAExtension(Env* _env): env(_env), env_setter([](Grammar*, Env*, const IOExample&){}) {
    ext::vsa::loadLogicWitness(this);
    manager_list.push_back(new BasicWitnessManager());
}
void VSAExtension::setEnvSetter(const VSAEnvSetter &_setter) {
    env_setter = _setter;
}
void VSAExtension::prepareEnv(Grammar *grammar, const IOExample &example) {
    env_setter(grammar, env, example);
}

VSAExtension::~VSAExtension() {
    for (auto* m: manager_list) delete m;
}

void VSAExtension::registerWitnessManager(WitnessManager *manager) {
    manager_list.push_front(manager);
}
void VSAExtension::registerWitnessFunction(const std::string &name, WitnessFunction *semantics) {
    OperatorWitnessManager* om = nullptr;
    for (auto* m: manager_list) {
        om = dynamic_cast<OperatorWitnessManager*>(m);
        if (om) break;
    }
    if (!om) {
        om = new OperatorWitnessManager();
        registerWitnessManager(om);
    }
    om->registerWitness(name, semantics);
}

WitnessList VSAExtension::getWitness(Semantics *semantics, const WitnessData &oup, const DataList &inp_list) const {
    for (auto* m: manager_list) {
        if (m->isMatch(semantics)) {
            return m->getWitness(semantics, oup, inp_list);
        }
    }
    LOG(FATAL) << "Witness: unsupported semantics " << semantics->name;
}

const std::string KVSAName = "VSA";

VSAExtension * ext::vsa::getExtension(Env *env) {
    auto* res = env->getExtension(KVSAName);
    if (res) {
        auto* tmp = dynamic_cast<VSAExtension*>(res);
        if (!tmp) {
            LOG(FATAL) << "Unmatched extension " << KVSAName;
        }
        return tmp;
    }
    auto* ext = new VSAExtension(env);
    env->registerExtension(KVSAName, ext);
    return ext;
}

namespace {
    bool _isConsideredByVSA(Program* p, const WitnessData& data, VSAExtension* ext, const DataList& inp_list) {
        auto wit_list = ext->getWitness(p->semantics.get(), data, inp_list);
        DataList sub_oup;
        for (auto& sub: p->sub_list) sub_oup.push_back(ext->env->run(sub.get(), inp_list));
        bool is_considered = false;
        for (auto& wit_term: wit_list) {
            bool is_valid = true;
            for (int i = 0; i < sub_oup.size(); ++i) {
                if (!wit_term[i]->isInclude(sub_oup[i])) {
                    is_valid = false; break;
                }
            }
            if (is_valid) {
                is_considered = true; break;
            }
        }
        if (!is_considered) {
            return false;
        }
        for (int i = 0; i < p->sub_list.size(); ++i) {
            if (!_isConsideredByVSA(p->sub_list[i].get(), std::make_shared<DirectWitnessValue>(sub_oup[i]), ext, inp_list)) return false;
        }
        return true;
    }
}

bool ext::vsa::isConsideredByVSA(Program *p, VSAExtension *ext, Grammar *g, const IOExample &example) {
    ext->prepareEnv(g, example);
    auto oup = ext->env->run(p, example.first);
    return _isConsideredByVSA(p, std::make_shared<DirectWitnessValue>(oup), ext, example.first);
}

