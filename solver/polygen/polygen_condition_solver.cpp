//
// Created by pro on 2022/1/8.
//

#include "istool/solver/polygen/polygen_condition_solver.h"
#include "glog/logging.h"

namespace {
    bool _isUseTerm(Env* env) {
        auto* data = env->getConstRef(solver::polygen::KIsUseTermName, BuildData(Bool, true));
        return (*data).isTrue();
    }

    PSynthInfo _insertTermsToInfo(const PSynthInfo& info, int term_num, const PType& term_type) {
        Grammar* g = grammar::copyGrammar(info->grammar);
        TypeList inp_type = info->inp_type_list;
        for (auto* symbol: g->symbol_list) {
            if (!term_type->equal(symbol->type.get())) continue;
            for (int i = 0; i < term_num; ++i) {
                int id = i + int(inp_type.size());
                symbol->rule_list.push_back(new ConcreteRule(semantics::buildParamSemantics(id, term_type), {}));
            }
        }
        for (int i = 0; i < term_num; ++i) inp_type.push_back(term_type);
        return std::make_shared<SynthInfo>(info->name, inp_type, type::getTBool(), g);
    }

    void _insertTermsToExample(IOExampleList& example_list, const ProgramList& term_list, Env* env) {
        for (int i = 0; i < example_list.size(); ++i) {
            // todo: handle SemanticsError
            DataList term_results;
            for (const auto& term: term_list) {
                if (dynamic_cast<ParamSemantics*>(term->semantics.get())) continue;
                try {
                    term_results.push_back(env->run(term.get(), example_list[i].first));
                } catch (SemanticsError& e) {
                    term_results.emplace_back();
                }
            }
            for (auto& d: term_results) {
                example_list[i].first.push_back(d);
            }
        }
    }
}

PolyGenConditionSolver::PolyGenConditionSolver(Specification *_spec, const PSynthInfo &cond_info,
                                               const PBESolverBuilder &_builder):
        builder(_builder), KIsUseTerm(_isUseTerm(_spec->env.get())), spec(_spec), info(cond_info) {
}

PProgram PolyGenConditionSolver::getCondition(const ProgramList &term_list, const IOExampleList &pos_list,
                                              const IOExampleList& neg_list, TimeGuard* guard) {
    PSynthInfo cond_info;
    TypeList inp_types = spec->info_list[0]->inp_type_list;
    /*LOG(INFO) << "Term List";
    for (auto& term: term_list) LOG(INFO) << "  " << term->toString();
    for (auto& example: pos_list) LOG(INFO) << example::ioExample2String(example);
    for (auto& example: neg_list) LOG(INFO) << example::ioExample2String(example);
    spec->info_list[0]->grammar->print();*/
    if (!KIsUseTerm) cond_info = info;
    else {
        auto term_type = spec->info_list[0]->oup_type;
        int extra_num = 0;
        for (const auto& term: term_list) {
            if (dynamic_cast<ParamSemantics*>(term->semantics.get())) continue;
            inp_types.push_back(term_type); ++extra_num;
        }
        cond_info = _insertTermsToInfo(info, extra_num, term_type);
    }
    IOExampleList io_example_list;
    for (const auto& example: pos_list) {
        io_example_list.emplace_back(example.first, BuildData(Bool, true));
    }
    for (const auto& example: neg_list) {
        io_example_list.emplace_back(example.first, BuildData(Bool, false));
    }
    if (KIsUseTerm) {
        _insertTermsToExample(io_example_list, term_list, spec->env.get());
    }
    auto example_space = example::buildFiniteIOExampleSpace(io_example_list, spec->info_list[0]->name, spec->env.get(), inp_types);
    auto* cond_spec = new Specification({cond_info}, spec->env, example_space);
    auto* cond_solver = builder(cond_spec);

    ExampleList example_list;
    for (const auto& io_example: io_example_list) {
        example_list.push_back(example::ioExample2Example(io_example));
    }
    auto res = cond_solver->synthesis(example_list, guard).begin()->second;

    delete cond_spec; delete cond_solver;

    if (KIsUseTerm) {
        int n = info->inp_type_list.size();
        ProgramList param_list(n);
        for (auto& term: term_list) {
            if (dynamic_cast<ParamSemantics*>(term->semantics.get())) continue;
            param_list.push_back(term);
        }

        res = program::rewriteParam(res, param_list);
    }
    return res;
}