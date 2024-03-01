//
// Created by pro on 2022/2/23.
//

#include "istool/dsl/deepcoder/deepcoder_dsl.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/deepcoder_type_system.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/string_type.h"
#include "glog/logging.h"

using namespace dsl::deepcoder;
#define TLIST ext::ho::getTIntList()
#define TINT theory::clia::getTInt()
#define TBOOL type::getTBool()

dsl::deepcoder::DeepCoderGrammarInfo::DeepCoderGrammarInfo(const TypeList &_param_list, const PType &_oup_type, const std::vector<PSemantics> &_extra_semantics):
    param_list(_param_list), oup_type(_oup_type), extra_semantics(_extra_semantics) {
    if (!oup_type) oup_type = theory::clia::getTInt();
}

namespace {
    std::string _getSymbolName(Type* type) {
        if (dynamic_cast<TList*>(type)) return "list_expr";
        if (dynamic_cast<TInt*>(type) || dynamic_cast<TVar*>(type)) return "int_expr";
        auto* at = dynamic_cast<TArrow*>(type);
        if (at) {
            if (at->inp_types.size() == 2) return "bi_hf";
            if (dynamic_cast<TBool*>(at->oup_type.get())) return "bool_hf";
            return "int_hf";
        }
        LOG(FATAL) << "Default DeepCoder: Unknown type " << type;
    }

    const std::vector<std::string> KDeepCoderBasicSemantics = {
            "take", "drop", "rev", "sort", "map", "filter", "zipwith", "scanl", "scanr", "len", "head",
            "last", "maximum", "minimum", "access", "sum", "neg", "count"
    };

    Rule* _buildCurriedRule(Env* env, const std::string& op_name, const DataList& param_list, const std::string& name) {
        auto* sem = env->getSemantics(op_name).get();
        auto* fs = dynamic_cast<FullExecutedSemantics*>(sem);
        if (!fs) {
            LOG(FATAL) << "Current implementation supports FullExecutedSemantics only";
        }
        auto f = [fs, param_list](DataList&& inp_list, ExecuteInfo* info) {
            DataList full_inp = param_list;
            for (auto& inp: inp_list) full_inp.push_back(inp);
            return fs->run(std::move(full_inp), info);
        };
        auto as = semantics::buildConstSemantics(ext::ho::buildAnonymousData(f, name));
        return new ConcreteRule(as, {});
    }
}

Grammar * dsl::deepcoder::getDefaultDeepCoderGrammar(Env* env, const DeepCoderGrammarInfo& info, bool is_remove_empty) {
    auto* list_expr = new NonTerminal("list_expr", TLIST);
    auto* int_expr = new NonTerminal("int_expr", TINT);
    auto* int_hf = new NonTerminal("int_hf", std::make_shared<TArrow>((TypeList){TINT}, TINT));
    auto* bool_hf = new NonTerminal("bool_hf", std::make_shared<TArrow>((TypeList){TINT}, TBOOL));
    auto* bi_hf = new NonTerminal("bi_hf", std::make_shared<TArrow>((TypeList){TINT, TINT}, TINT));
    NTList symbol_list = {list_expr, int_expr, int_hf, bool_hf, bi_hf};
    std::unordered_map<std::string, NonTerminal*> nt_map;
    for (auto* symbol: symbol_list) nt_map[symbol->name] = symbol;

    std::vector<PSemantics> semantics_list = info.extra_semantics;
    for (const auto& name: KDeepCoderBasicSemantics) semantics_list.push_back(env->getSemantics(name));

    for (const auto& sem: semantics_list) {
        auto* ts = dynamic_cast<TypedSemantics*>(sem.get());
        auto* source = nt_map[_getSymbolName(ts->oup_type.get())];
        NTList param_list;
        for (const auto& inp_type: ts->inp_type_list) {
            param_list.push_back(nt_map[_getSymbolName(inp_type.get())]);
        }
        source->rule_list.push_back(new ConcreteRule(sem, std::move(param_list)));
    }
    int_expr->rule_list.push_back(new ConcreteRule(env->getSemantics("apply"), {bi_hf, int_expr, int_expr}));

    for (int c: {-1, 1, 2}) {
        int_expr->rule_list.push_back(new ConcreteRule(semantics::buildConstSemantics(BuildData(Int, c)), {}));
        if (c == -1) continue;
        for (auto op: {"-", "+"}) {
            int_hf->rule_list.push_back(_buildCurriedRule(env, op, {BuildData(Int, c)}, op + std::to_string(c)));
        }
    }
    int_hf->rule_list.push_back(_buildCurriedRule(env, "neg", {}, "neg"));

    bool_hf->rule_list.push_back(_buildCurriedRule(env, "odd", {}, "odd"));
    bool_hf->rule_list.push_back(_buildCurriedRule(env, "even", {}, "even"));
    bool_hf->rule_list.push_back(_buildCurriedRule(env, "<", {BuildData(Int, 0)}, ">0"));
    bool_hf->rule_list.push_back(_buildCurriedRule(env, ">", {BuildData(Int, 0)}, "<0"));

    for (const auto& bi_name: {"+", "-", "min", "max", "*"}) {
        bi_hf->rule_list.push_back(_buildCurriedRule(env, bi_name, {}, bi_name));
    }

    auto* start = nt_map[_getSymbolName(info.oup_type.get())];
    auto* grammar = new Grammar(start, symbol_list, is_remove_empty);
    return grammar;
}

void dsl::deepcoder::prepareEnv(Env *env) {
    ext::ho::loadDeepCoderSemantics(env);
    theory::loadCLIATheory(env);
    auto* type_ext = type::getTypeExtension(env);
    type::registerTypeSystem(new DeepCoderTypeSystem(type_ext), type_ext);
    type_ext->registerTypeInfo(new DeepCoderValueTypeInfo(type_ext));
}