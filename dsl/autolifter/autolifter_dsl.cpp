//
// Created by pro on 2022/2/21.
//

#include "istool/dsl/autolifter/autolifter_dsl.h"
#include "istool/dsl/autolifter/autolifter_semantics.h"
#include "istool/dsl/clia/clia_dsl.h"
#include "istool/dsl/deepcoder/deepcoder_dsl.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/ext/deepcoder/higher_order_operator.h"
#include "istool/ext/limited_type/limited_ds.h"
#include "istool/ext/limited_type/limited_int.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/basic/type_system.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/limited_type/limited_type.h"
#include "glog/logging.h"
#include <unordered_set>

using namespace dsl::autolifter;

dsl::autolifter::LiftingModConfigInfo::LiftingModConfigInfo(const PProgram &_m, const PType &_F, const std::vector<int> &_extra_consts,
                                                            const std::vector<PSemantics> &_extra_semantics): m(_m), F(_F), extra_consts(_extra_consts), extra_semantics(_extra_semantics) {
}

dsl::autolifter::LiftingConfigInfo::LiftingConfigInfo(const PProgram &_p, const PType &_inp_type, const PEnv& _env, const std::vector<LiftingModConfigInfo> &_mod_list,
                                                      const std::vector<PSemantics> &_extra_semantics): env(_env), p(_p), inp_type(_inp_type),
                                                      mod_list(_mod_list), extra_semantics(_extra_semantics) {
}

namespace {
    int _getRandInt(Env* env, int l, int r) {
        std::uniform_int_distribution<int> dis(l, r);
        return dis(env->random_engine);
    }

    Data _generateData(Env* env, Type* type, TimeGuard* guard) {
        auto* pt = dynamic_cast<TProduct*>(type);
        if (pt) {
            DataList content;
            for (auto& sub: pt->sub_types) content.push_back(_generateData(env, sub.get(), guard));
            return BuildData(Product, content);
        }
        auto* bt = dynamic_cast<TBool*>(type);
        if (bt) return BuildData(Bool, rand() & 1);
        auto* it = dynamic_cast<LimitedTInt*>(type);
        if (it) {
            return BuildData(Int, _getRandInt(env, it->l, it->r));
        }
        auto* lt = dynamic_cast<LimitedTList*>(type);
        if (lt) {
            int size = _getRandInt(env, 0, lt->max_size);
            DataList content;
            for (int i = 0; i < size; ++i) {
                content.push_back(_generateData(env, lt->content.get(), guard));
            }
            return BuildData(List, content);
        }
        auto* rt = dynamic_cast<RefinedType*>(type);
        if (rt) {
            while (1) {
                TimeCheck(guard);
                auto res = _generateData(env, rt->content.get(), guard);
                if (rt->isValid(res.get())) return res;
            }
        }
        LOG(FATAL) << "Unsupported type " << type->getName() << " for AutoLifter";
    }

    class _DefaultExampleGenerator: public ExampleGenerator {
    public:
        Env* env;
        TypeList type_list;
        _DefaultExampleGenerator(Env* _env, const TypeList& _type_list): env(_env), type_list(_type_list) {
        }
        virtual ExampleList generateExamples(TimeGuard* guard) {
            DataList example;
            for (const auto& type: type_list) {
                example.push_back(_generateData(env, type.get(), guard));
            }
            return {example};
        }
    };

    void _getVarList(Type* type, std::unordered_set<std::string>& name_set) {
        auto* vt = dynamic_cast<TVar*>(type);
        if (vt) {
            name_set.insert(vt->name);
            return;
        }
        for (const auto& sub_type: type->getParams()) {
            _getVarList(sub_type.get(), name_set);
        }
    }

    std::vector<std::string> _getVarList(Type* type) {
        std::unordered_set<std::string> name_set;
        _getVarList(type, name_set);
        std::vector<std::string> res(name_set.begin(), name_set.end());
        return res;
    }

    PProgram _buildAutoLifterConsProgram(const PProgram& p, const PProgram& m, const PType& F, Env* env) {
        auto sf = std::make_shared<InvokeSemantics>("f", env);
        auto sc = std::make_shared<InvokeSemantics>("c", env);
        auto pf = std::make_shared<Program>(sf, (ProgramList){});
        auto pp = program::rewriteParam(p, {std::make_shared<Program>(std::make_shared<TmpSemantics>("x"), (ProgramList){})});

        auto cf = std::make_shared<Program>(env->getSemantics("curry"), (ProgramList){pf});
        auto cp = std::make_shared<Program>(std::make_shared<LambdaSemantics>((std::vector<std::string>){"x"}), (ProgramList){pp});
        auto ptf = std::make_shared<Program>(env->getSemantics("tri"), (ProgramList){cp, cf});

        // build left
        auto l = std::make_shared<Program>(env->getSemantics("apply"), (ProgramList){ptf, m});
        auto r = std::make_shared<Program>(std::make_shared<FMapSemantics>(env, F), (ProgramList){ptf});
        r = std::make_shared<Program>(env->getSemantics("apply"), (ProgramList){r, program::buildParam(0)});
        r = std::make_shared<Program>(sc, (ProgramList){r});
        return std::make_shared<Program>(env->getSemantics("="), (ProgramList){l, r});
    }

    NonTerminal* _buildComponentSymbol(const PType& type, std::unordered_map<std::string, NonTerminal*>& symbol_map) {
        auto feature = type->getName();
        if (symbol_map.find(feature) != symbol_map.end()) return symbol_map[feature];
        auto* pt = dynamic_cast<TProduct*>(type.get());
        if (!pt) {
            LOG(FATAL) << "AutoLifter: Unsupported base type " << type->getName();
        }
        auto* symbol = new NonTerminal("Symbol@" + feature, type);
        symbol_map[feature] = symbol;
        for (int i = 0; i < pt->sub_types.size(); ++i) {
            auto* sub = _buildComponentSymbol(pt->sub_types[i], symbol_map);
            sub->rule_list.push_back(new ConcreteRule(std::make_shared<AccessSemantics>(i), {symbol}));
        }
        return symbol;
    }

    Grammar* _insertParam(Grammar* base, const TypeList& param_list) {
        std::unordered_map<std::string, NonTerminal*> symbol_map;
        for (auto* symbol: base->symbol_list) {
            symbol_map[symbol->type->getName()] = symbol;
        }
        for (int i = 0; i < param_list.size(); ++i) {
            auto *param_symbol = _buildComponentSymbol(param_list[i], symbol_map);
            param_symbol->rule_list.push_back(new ConcreteRule(semantics::buildParamSemantics(i, param_list[i]), {}));
        }
        NTList symbol_list;
        for (const auto& item: symbol_map) symbol_list.push_back(item.second);
        return new Grammar(base->start, symbol_list);
    }

    class _DefaultCombinatorGrammarBuilder: public CombinatorGrammarBuilder {
    public:
        Env* env;
        std::vector<int> const_list;
        std::vector<PSemantics> extra_semantics;
        _DefaultCombinatorGrammarBuilder(Env* _env, const std::vector<int>& _const_list, const std::vector<PSemantics>& _extra_semantics):
            env(_env), const_list(_const_list), extra_semantics(_extra_semantics) {
        }
        virtual Grammar* buildGrammar(Program* p, const TypeList& type_list) {
            auto* type_system = type::getTypeExtension(env);
            dsl::clia::CLIAGrammarInfo clia_info({}, type_system->getType(p), const_list, extra_semantics);
            auto* grammar = dsl::clia::getDefaultCLIAGrammar(env, clia_info, false);
            return _insertParam(grammar, {type_list});
        }
        virtual ~_DefaultCombinatorGrammarBuilder() {
        }
    };

    PLiftingModInfo _buildLiftingModInfo(const PProgram& p, const LiftingModConfigInfo& info, Env* env, const PType& inp_type) {
        auto name_list = _getVarList(info.F.get());
        if (name_list.size() > 1) {
            LOG(INFO) << "AutoLifter supports endofunctor only";
        }
        auto full_type = name_list.empty() ? info.F : type::substituteVar(info.F, {{name_list[0], inp_type}});
        auto g = std::make_shared<_DefaultExampleGenerator>(env, (TypeList){full_type});
        auto cons_program = _buildAutoLifterConsProgram(p, info.m, info.F, env);
        auto example_space = std::make_shared<StreamedExampleSpace>(cons_program, g, env);
        auto* grammar_builder = new _DefaultCombinatorGrammarBuilder(env, info.extra_consts, info.extra_semantics);
        return std::make_shared<LiftingModInfo>(info.m, ext::ltype::getBaseType(info.F.get()), example_space, grammar_builder);
    }

    PSynthInfo _buildFInfo(const std::vector<PSemantics>& extra_semantics, const PType& inp_type, Env* env) {
        dsl::deepcoder::DeepCoderGrammarInfo info({}, theory::clia::getTInt(), extra_semantics);
        auto* base_grammar = dsl::deepcoder::getDefaultDeepCoderGrammar(env, info, false);
        auto* grammar = _insertParam(base_grammar, {ext::ltype::getBaseType(inp_type.get())});
        return std::make_shared<SynthInfo>("f", (TypeList){inp_type}, grammar->start->type, grammar);
    }
}

LiftingTask* dsl::autolifter::buildLiftingTask(const LiftingConfigInfo& info) {
    std::vector<PLiftingModInfo> info_list;
    for (auto& mod_config: info.mod_list) {
        info_list.push_back(_buildLiftingModInfo(info.p, mod_config, info.env.get(), info.inp_type));
    }
    auto f_info = _buildFInfo(info.extra_semantics, info.inp_type, info.env.get());
    return new LiftingTask(info_list, info.p, info.p, f_info, info.env);
}

void dsl::autolifter::prepareEnv(Env *env) {
    dsl::clia::prepareEnv(env);
    dsl::deepcoder::prepareEnv(env);
}
