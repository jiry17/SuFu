//
// Created by pro on 2022/2/22.
//

#include "istool/dsl/clia/clia_dsl.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/z3/clia/clia_z3.h"
#include "glog/logging.h"

void dsl::clia::prepareEnv(Env *env) {
    theory::loadCLIATheory(env);
    theory::loadZ3CLIA(env);
}

dsl::clia::CLIAGrammarInfo::CLIAGrammarInfo(const TypeList &_param_list, const PType &_oup_type,
        const std::vector<int> &_const_list, const std::vector<PSemantics>& _extra_semantics):
    param_list(_param_list), oup_type(_oup_type), const_list(_const_list), extra_semantics(_extra_semantics) {
    if (!oup_type) oup_type = theory::clia::getTInt();
}

namespace {
    bool _isInt(Type* type) {
        if (dynamic_cast<TInt*>(type)) return true;
        assert(dynamic_cast<TBool*>(type));
        return false;
    }
}

Grammar * dsl::clia::getDefaultCLIAGrammar(Env* env, const CLIAGrammarInfo &info, bool is_remove_empty) {
    auto* bs = new NonTerminal("bool", type::getTBool());
    auto* is = new NonTerminal("int", theory::clia::getTInt());

    for (auto bool_value: {false, true}) {
        auto s = semantics::buildConstSemantics(BuildData(Bool, bool_value));
        bs->rule_list.push_back(new ConcreteRule(s, {}));
    }
    for (auto int_value: info.const_list) {
        auto s = semantics::buildConstSemantics(BuildData(Int, int_value));
        is->rule_list.push_back(new ConcreteRule(s, {}));
    }
    for (int i = 0; i < info.param_list.size(); ++i) {
        auto s = semantics::buildParamSemantics(i, info.param_list[i]);
        auto* symbol = _isInt(info.param_list[i].get()) ? is : bs;
        symbol->rule_list.push_back(new ConcreteRule(s, {}));
    }
    auto* start = _isInt(info.oup_type.get()) ? is : bs;

    auto full_op_list = info.extra_semantics;
    for (auto op: {"+", "-", "||", "&&", "!"}) {
        full_op_list.push_back(env->getSemantics(op));
    }
    for (const auto& name: info.extra_semantics) full_op_list.push_back(name);

    for (const auto& sem: full_op_list) {
        auto* ts = dynamic_cast<TypedSemantics*>(sem.get());
        assert(ts);
        NTList sub_list;
        for (const auto& type: ts->inp_type_list) {
            if (_isInt(type.get())) sub_list.push_back(is); else sub_list.push_back(bs);
        }
        auto* s = _isInt(ts->oup_type.get()) ? is : bs;
        s->rule_list.push_back(new ConcreteRule(sem, std::move(sub_list)));
    }
    for (const auto& op: {"<", "<="}) {
        bs->rule_list.push_back(new ConcreteRule(env->getSemantics(op), {is, is}));
    }
    is->rule_list.push_back(new ConcreteRule(env->getSemantics("ite"), {bs, is, is}));

    auto* res = new Grammar(start, {is, bs}, is_remove_empty);
    return res;
}