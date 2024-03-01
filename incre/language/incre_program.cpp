//
// Created by pro on 2022/9/18.
//

#include "istool/incre/language/incre_program.h"
#include "istool/solver/polygen/polygen_term_solver.h"
#include "glog/logging.h"
#include <iostream>

using namespace incre;

CommandData::CommandData(CommandType _type, const DecorateSet& _decorate_set):
    type(_type), decorate_set(_decorate_set) {}
CommandType CommandData::getType() const {
    return type;
}

bool CommandData::isDecoratedWith(CommandDecorate deco) const {
    return decorate_set.find(deco) != decorate_set.end();
}

std::string CommandData::toString() {
    std::string res;
    for (auto deco: decorate_set) {
        res += "@" + decorate2String(deco) + " ";
    }
    return res + contentToString();
}

std::string CommandImport::contentToString() const {
    return "import " + name + ";";
}
CommandImport::CommandImport(const std::string &_name, const CommandList &_commands):
    CommandData(CommandType::IMPORT, {}), name(_name), commands(_commands) {
}

CommandBind::CommandBind(const std::string &_name, const Binding &_binding, const DecorateSet& _decorate_set):
    CommandData(CommandType::BIND, _decorate_set), name(_name), binding(_binding) {
}
std::string CommandBind::contentToString() const {
    return name + " = " + binding->toString() + ";";
}

CommandDefInductive::CommandDefInductive(const Ty &__type):
    CommandData(CommandType::DEF_IND, {}), _type(__type) {
    type = dynamic_cast<TyInductive*>(_type.get());
    if (!type) {
        LOG(FATAL) << "Expected TyInductive but get " << _type->toString();
    }
}
std::string CommandDefInductive::contentToString() const {
    auto res = "Inductive " + type->name + " = ";
    for (int i = 0; i < type->constructors.size(); ++i) {
        auto& [name, ty] = type->constructors[i];
        if (i) res += " | ";
        res += name + " " + ty->toString();
    }
    return res + ";";
}

ProgramData::ProgramData(const CommandList &_commands, const IncreConfigMap& _config_map):
    commands(_commands), config_map(_config_map) {}
void ProgramData::print() const {
    for (auto& command: commands) {
        std::cout << command->toString() << std::endl;
    }
}

namespace {
    const std::unordered_map<CommandDecorate, std::string> KDecorateNameMap = {
            {CommandDecorate::INPUT, "Input"}, {CommandDecorate::START, "Start"},
            {CommandDecorate::SYN_ALIGN, "Align"}, {CommandDecorate::SYN_COMBINE, "Combine"},
            {CommandDecorate::SYN_COMPRESS, "Extract"}, {CommandDecorate::SYN_NO_PARTIAL, "NoPartial"}
    };
}

CommandDecorate incre::string2Decorate(const std::string &s) {
    for (auto& [deco, name]: KDecorateNameMap) {
        if (name == s) return deco;
    }
    LOG(FATAL) << "Unknown Decorate " << s;
}

std::string incre::decorate2String(CommandDecorate deco) {
    auto it = KDecorateNameMap.find(deco);
    assert(it != KDecorateNameMap.end());
    return it->second;
}

namespace {
    const std::unordered_map<IncreConfig, std::string> KConfigNameMap = {
            {IncreConfig::COMPOSE_NUM, "ComposeNum"},
            {IncreConfig::VERIFY_BASE, "VerifyBase"},
            {IncreConfig::NON_LINEAR, "NonLinear"},
            {IncreConfig::SAMPLE_SIZE, "SampleSize"},
            {IncreConfig::EXTRA_GRAMMAR, "ExtraGrammar"},
            {IncreConfig::ENABLE_FOLD, "EnableFold"},
            {IncreConfig::SAMPLE_INT_MIN, "SampleIntMin"},
            {IncreConfig::SAMPLE_INT_MAX, "SampleIntMax"},
            {IncreConfig::PRINT_ALIGN, "PrintAlign"},
            {IncreConfig::TERM_NUM, "TermNum"},
            {IncreConfig::CLAUSE_NUM, "ClauseNum"}
    };
}

IncreConfig incre::string2ConfigType(const std::string &s) {
    for (auto& [config, name]: KConfigNameMap) {
        if (name == s) return config;
    }
    LOG(FATAL) << "Unknown Config " << s;
}


#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/solver/polygen/dnf_learner.h"

const std::string config_name::KDataSizeLimitName = "incre@data-size-limit";
const std::string config_name::KExtraGrammarName = "incre@extra-grammar";
const std::string config_name::KIsNonLinearName = "incre@is-non-linear";
const std::string config_name::KIsEnableFoldName = "incre@is-enable-fold";
const std::string config_name::KSampleIntMaxName = "incre@sample-int-max";
const std::string config_name::KSampleIntMinName = "incre@sample-int-min";
const std::string config_name::KPrintAlignName = "incre@print-align";

namespace {
    std::unordered_map<IncreConfig, std::string> KConfigEnvNameMap;

    void _constructEnvNameMap() {
        KConfigEnvNameMap = {
            {IncreConfig::COMPOSE_NUM, solver::autolifter::KComposedNumName},
            {IncreConfig::TERM_NUM, solver::polygen::KMaxTermNumName},
            {IncreConfig::NON_LINEAR, config_name::KIsNonLinearName},
            {IncreConfig::VERIFY_BASE, solver::autolifter::KOccamExampleNumName},
            {IncreConfig::SAMPLE_SIZE, config_name::KDataSizeLimitName},
            {IncreConfig::EXTRA_GRAMMAR, config_name::KExtraGrammarName},
            {IncreConfig::ENABLE_FOLD, config_name::KIsEnableFoldName},
            {IncreConfig::SAMPLE_INT_MIN, config_name::KSampleIntMinName},
            {IncreConfig::SAMPLE_INT_MAX, config_name::KSampleIntMaxName},
            {IncreConfig::PRINT_ALIGN, config_name::KPrintAlignName},
            {IncreConfig::CLAUSE_NUM, solver::polygen::KMaxClauseNumName}
        };
    }
}

void incre::applyConfig(IncreConfig config, const Data &config_value, Env *env) {
    if (KConfigEnvNameMap.empty()) _constructEnvNameMap();
    env->setConst(KConfigEnvNameMap[config], config_value);
}

void incre::applyConfig(ProgramData *program, Env *env) {
    if (KConfigEnvNameMap.empty()) _constructEnvNameMap();
    for (auto& [incre_type, _]: KConfigEnvNameMap) {
        applyConfig(incre_type, program->config_map[incre_type], env);
    }
}
