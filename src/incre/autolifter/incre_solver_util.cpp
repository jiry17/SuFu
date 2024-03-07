//
// Created by pro on 2023/7/18.
//

#include "istool/incre/autolifter/incre_solver_util.h"
#include "istool/solver/polygen/polygen.h"

using namespace incre;
using namespace incre::autolifter;
using namespace incre::autolifter::util;

std::pair<SolverToken, InvokeConfig> util::getSolverToken(Type *oup_type) {
    if (dynamic_cast<TBool *>(oup_type)) return {SolverToken::POLYGEN_CONDITION, {}};
    if (dynamic_cast<TInt *>(oup_type)) {
        invoker::multi::SolverBuilder obe_builder = [](Specification *spec, Verifier *v) {
            auto split_info = solver::divideSpecForSTUN(spec->info_list[0]);
            auto *term_spec = new Specification({split_info.first}, spec->env, spec->example_space);
            return invoker::single::buildOBE(term_spec, v, {});
        };
        invoker::multi::SolverBuilder staged_polygen_solver = [](Specification *spec, Verifier *v) {
            InvokeConfig config;
            config.set("is_staged", true);
            return invoker::single::buildPolyGen(spec, v, config);
        };
        invoker::multi::SolverBuilder lia_solver = [](Specification *spec, Verifier *v) {
            auto split_info = solver::divideSpecForSTUN(spec->info_list[0]);
            auto *term_spec = new Specification({split_info.first}, spec->env, spec->example_space);
            return invoker::single::buildLIASolver(term_spec, v, {});
        };

        InvokeConfig config;
        config.set("solver_list",
                   (std::vector<invoker::multi::SolverBuilder>) {obe_builder, lia_solver, staged_polygen_solver});
        return {SolverToken::MULTI_THREAD, config};
    }
    LOG(FATAL) << "Unsupported type " << oup_type->getName();
}

namespace {
    bool _isDistinguishAllExamples(const std::vector<bool>& is_used, const IOExampleList& example_list) {
        std::unordered_map<std::string, Data> example_map;
        for (auto& [inp, oup]: example_list) {
            DataList sim_inp;
            for (int i = 0; i < is_used.size(); ++i) {
                if (is_used[i]) sim_inp.push_back(inp[i]);
            }
            auto feature = data::dataList2String(sim_inp);
            if (example_map.find(feature) == example_map.end()) {
                example_map[feature] = oup;
            } else if (!(oup == example_map[feature])) {
                return false;
            }
        }
        return true;
    }

    std::pair<std::vector<int>, IOExampleList> _simplifyExampleSpace(const IOExampleList& example_list) {
        assert(example_list.size());
        std::vector<bool> is_used(example_list[0].first.size(), true);
        for (int i = 0; i < is_used.size(); ++i) {
            is_used[i] = false;
            if (!_isDistinguishAllExamples(is_used, example_list)) is_used[i] = true;
        }
        std::vector<int> remained_indices;
        for (int i = 0; i < is_used.size(); ++i) {
            if (is_used[i]) remained_indices.push_back(i);
        }
        IOExampleList simplified_example_list;
        for (auto& example: example_list) {
            DataList simplified_input;
            for (auto ind: remained_indices) simplified_input.push_back(example.first[ind]);
            simplified_example_list.emplace_back(simplified_input, example.second);
        }
        return {remained_indices, simplified_example_list};
    }

    PProgram _recoverProgram(const TypeList& param_type_list, const std::vector<int>& indices, const PProgram& res) {
        ProgramList param_list;
        for (auto ind: indices) param_list.push_back(program::buildParam(ind, param_type_list[ind]));
        return program::rewriteParam(res, param_list);
    }

    Grammar* _simplifyGrammar(Grammar* g, const std::vector<int>& indices) {
        g->indexSymbol(); NTList symbol_list(g->symbol_list.size(), nullptr);
        for (auto* symbol: g->symbol_list) {
            symbol_list[symbol->id] = new NonTerminal(symbol->name, symbol->type);
        }
        std::unordered_map<int, int> index_map;
        for (int i = 0; i < indices.size(); ++i) index_map[indices[i]] = i;
        for (auto* pre_symbol: g->symbol_list) {
            auto* new_symbol = symbol_list[pre_symbol->id];
            for (auto* rule: pre_symbol->rule_list) {
                auto* cr = dynamic_cast<ConcreteRule*>(rule);
                if (cr) {
                    auto* ps = dynamic_cast<ParamSemantics*>(cr->semantics.get());
                    if (ps) {
                        if (index_map.find(ps->id) == index_map.end()) continue;
                        auto new_sem = semantics::buildParamSemantics(index_map[ps->id], ps->oup_type);
                        new_symbol->rule_list.push_back(new ConcreteRule(new_sem, {}));
                        continue;
                    }
                }
                NTList new_param_list;
                for (auto* param: rule->param_list) new_param_list.push_back(symbol_list[param->id]);
                new_symbol->rule_list.push_back(rule->clone(new_param_list));
            }
        }
        return new Grammar(symbol_list[g->start->id], symbol_list);
    }
}

PProgram util::synthesis2Program(const TypeList &inp_type_list, const PType &oup_type, const PEnv &env, Grammar* grammar,
                                 const IOExampleList &example_list) {
    if (dynamic_cast<TBot*>(oup_type.get())) {
        return program::buildConst(Data(std::make_shared<VUnit>()));
    }
    const std::string default_name = "func";
    if (example_list.empty()) {
        return ::grammar::getMinimalProgram(grammar);
    }
    auto [used_indices, simplified_examples] = _simplifyExampleSpace(example_list);
    TypeList simplified_type_list;
    for (auto& index: used_indices) simplified_type_list.push_back(inp_type_list[index]);
    auto example_space = example::buildFiniteIOExampleSpace(simplified_examples, default_name, env.get(), simplified_type_list);
    auto *simplified_grammar = _simplifyGrammar(grammar, used_indices);

    auto info = std::make_shared<SynthInfo>(default_name, simplified_type_list,
                                            simplified_grammar->start->type, simplified_grammar);
    auto *spec = new Specification({info}, env, example_space);
    auto *v = new FiniteExampleVerifier(example_space.get());
    auto config = util::getSolverToken(oup_type.get());

    auto res = invoker::synthesis(spec, v, config.first, nullptr, config.second);
    delete v;
    delete spec;
    delete simplified_grammar;
    return _recoverProgram(inp_type_list, used_indices, res[default_name]);
}

namespace {
    Term _buildSingleStep(const PSemantics& sem, const incre::grammar::SynthesisComponentList& component_list, const TermList& sub_list) {
        for (auto& component: component_list) {
            auto res = component->tryBuildTerm(sem, sub_list);
            if (res) return res;
        }
        if (sem->getName() == "unit" || sem->getName() == "Unit") {
            auto* cs = dynamic_cast<ConstSemantics*>(sem.get());
            assert(cs);
            return std::make_shared<TmValue>(cs->w);
        }
        LOG(FATAL) << "Cannot build IncreTerm for semantics " << sem->getName();
    }
}

Term util::program2Term(Program* program, const incre::grammar::SynthesisComponentList& component_list,
                        const TermList &term_list) {
    auto *ps = dynamic_cast<ParamSemantics *>(program->semantics.get());
    if (ps) return term_list[ps->id];
    TermList sub_list;
    for (const auto& sub: program->sub_list) sub_list.push_back(program2Term(sub.get(), component_list, term_list));
    return _buildSingleStep(program->semantics, component_list, sub_list);
}