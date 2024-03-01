//
// Created by pro on 2023/6/22.
//

#include "istool/incre/analysis/incre_instru_runtime.h"
#include "glog/logging.h"

using namespace incre;

EnvBasedExampleCollector::EnvBasedExampleCollector(const std::vector<std::unordered_set<std::string>> &_cared_vars,
                                                   ProgramData *_program): IncreExampleCollector(_cared_vars, _program) {
    auto label_rule = [](const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& map) -> Data {
        auto* lt = dynamic_cast<TmLabeledLabel*>(term.get()); assert(lt);
        auto res = incre::envRun(lt->content, env, holder, map);
        return Data(std::make_shared<VLabeledCompress>(res, lt->id));
    };
    auto align_rule = [this](const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& map) {
        auto* lt = dynamic_cast<TmLabeledAlign*>(term.get()); assert(lt);
        auto tau_id = lt->id; auto names = this->cared_vars[tau_id];
        std::unordered_map<std::string, Data> local_values;
        for (auto& name: names) {
            local_values[name] = holder->lookup(env, name);
        }
        auto content = incre::envRun(lt->content, env, holder, map);
        this->add(tau_id, local_values, content);
        return content;
    };
    ext_map[TermType::LABEL] = (ExternalEnvRunRule){label_rule};
    ext_map[TermType::ALIGN] = (ExternalEnvRunRule){align_rule};
    ctx = incre::envRun(_program, ext_map);
}

void EnvBasedExampleCollector::collect(const Term &start, const std::unordered_map<std::string, Data> &_global) {
    ctx->initGlobal(_global); current_global = _global;
    incre::envRun(start, ctx->start, ctx->holder, ext_map);
}

EnvBasedExampleCollector::~EnvBasedExampleCollector() noexcept {
    delete ctx;
}