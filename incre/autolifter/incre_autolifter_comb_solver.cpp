//
// Created by pro on 2022/11/18.
//

#include <istool/basic/bitset.h>
#include <istool/ext/deepcoder/deepcoder_semantics.h>
#include "istool/incre/autolifter/incre_autolifter_solver.h"
#include "istool/solver/autolifter/basic/streamed_example_space.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/incre/autolifter/incre_solver_util.h"
#include "istool/invoker/invoker.h"
#include "istool/solver/stun/stun.h"
#include "glog/logging.h"
#include <iostream>

using namespace incre;
using namespace incre::autolifter;

namespace {
    struct _OutputCase {
        std::vector<int> path;
        TypedProgram program;
        Env* env;
        _OutputCase(const std::vector<int>& _path, const TypedProgram& _program, Env* _env): path(_path), program(_program), env(_env) {}
        Data extract(const Data& d) const {
            auto res = d;
            for (auto pos: path) {
                auto* cv = dynamic_cast<VCompress*>(res.get());
                if (cv) {
                    try {
                        return env->run(program.second.get(), {cv->content});
                    } catch (SemanticsError& e) {
                        return {};
                    }
                }
                auto* tv = dynamic_cast<VTuple*>(res.get());
                assert(tv);
                res = tv->elements[pos];
            }
            return res;
        }
    };

    void _collectOutputCase(const Ty& type, std::vector<int>& path, std::vector<_OutputCase>& res, const std::vector<FRes>& f_res_list, Env* env) {
        if (type->getType() == TyType::TUPLE) {
            auto* tt = dynamic_cast<TyTuple*>(type.get());
            for (int i = 0; i < tt->fields.size(); ++i) {
                path.push_back(i);
                _collectOutputCase(tt->fields[i], path, res, f_res_list, env);
                path.pop_back();
            }
        } else if (type->getType() == TyType::COMPRESS) {
            auto* ct = dynamic_cast<TyLabeledCompress*>(type.get());
            assert(ct);
            if (f_res_list[ct->id].component_list.size() == 1) {
                res.emplace_back(path, f_res_list[ct->id].component_list[0].program,env);
            } else {
                for (int i = 0; i < f_res_list[ct->id].component_list.size(); ++i) {
                    path.push_back(i);
                    auto &oup_component = f_res_list[ct->id].component_list[i];
                    res.emplace_back(path, oup_component.program, env);
                    path.pop_back();
                }
            }
        } else res.emplace_back(path, std::pair<PType, PProgram>(incre::typeFromIncre(type), nullptr), env);
    }

    std::vector<_OutputCase> _collectOutputCase(int align_id, IncreAutoLifterSolver* solver) {
        auto oup_type = solver->info->align_infos[align_id]->oup_type;
        std::vector<int> path; std::vector<_OutputCase> res;
        _collectOutputCase(oup_type, path, res, solver->f_res_list, solver->env.get());
        return res;
    }

    class CExampleSpace {
    public:
        FExampleSpace* base_example_space;
        PEnv env;
        int align_id;

        IOExampleList example_list;
        TypeList inp_type_list;
        Ty oup_ty;
        std::vector<FRes> f_res_list;
        TypedProgramList compress_program_list;
        int KExampleTimeOut = 10, current_pos, KExampleEnlargeFactor = 2;

        std::vector<std::pair<AuxProgram, DataList*>> inp_cache_list;
        std::vector<std::pair<std::pair<PProgram, std::vector<int>>, DataList*>> oup_cache_list;

        void insertExample(const IOExample& example) {
            example_list.push_back(example);
        }
        /*Data runFRes(const FRes& res, int example_id, const std::vector<int>& path) {
            if (res.component_list.empty()) return Data(std::make_shared<VUnit>());
            if (res.component_list.size() == 1) {
                return base_example_space->runOup(example_id, res.component_list[0].program.second.get(), path);
            }
            DataList elements;
            for (auto& component: res.component_list) {
                auto res = base_example_space->runOup(example_id, component.program.second.get(), path);
                elements.push_back(res);
            }
            return Data(std::make_shared<VTuple>(elements));
        }
        Data runOutput(const Ty& type, int example_id, const Data& d, std::vector<int>& path) {
            if (type->getType() == TyType::TUPLE) {
                auto* tt = dynamic_cast<TyTuple*>(type.get());
                auto* tv = dynamic_cast<VTuple*>(d.get());
                assert(tt && tv && tt->fields.size() == tv->elements.size());
                DataList elements(tt->fields.size());
                for (int i = 0; i < tt->fields.size(); ++i) {
                    path.push_back(i);
                    elements[i] = runOutput(tt->fields[i], example_id, tv->elements[i], path);
                    path.pop_back();
                }
                return BuildData(Product, elements);
            }
            if (type->getType() == TyType::COMPRESS) {
                auto* ct = dynamic_cast<TyLabeledCompress*>(type.get());
                auto* cv = dynamic_cast<VCompress*>(d.get());
                assert(ct && cv);
                return runFRes(f_res_list[ct->id], example_id, path);
            }
            return d;
        }*/
        Data runOutput(const Ty& type, int example_id, std::vector<int>& path, int& cache_id) {
            if (type->getType() == TyType::TUPLE) {
                auto* tt = dynamic_cast<TyTuple*>(type.get());
                assert(tt);
                DataList elements(tt->fields.size());
                for (int i = 0; i < tt->fields.size(); ++i) {
                    path.push_back(i);
                    elements[i] = runOutput(tt->fields[i], example_id, path, cache_id);
                    path.pop_back();
                }
                return BuildData(Product, elements);
            }
            if (type->getType() == TyType::COMPRESS) {
                auto* ct = dynamic_cast<TyLabeledCompress*>(type.get());
                assert(ct); int num = f_res_list[ct->id].component_list.size();
                if (!num) return Data(std::make_shared<VUnit>());
                if (num == 1) return oup_cache_list[cache_id++].second->at(example_id);
                DataList elements(num);
                // LOG(INFO) << "cache " << cache_id << " " << num << " " << oup_cache_list.size();
                for (int i = 0; i < num; ++i) elements[i] = oup_cache_list[cache_id++].second->at(example_id);
                return Data(std::make_shared<VTuple>(elements));
            }
            return oup_cache_list[cache_id++].second->at(example_id);
        }
        void buildExample(int example_id) {
#ifdef DEBUG
            assert(example_id < base_example_space->example_list.size());
#endif
            DataList inp(inp_cache_list.size());
            for (int i = 0; i < inp_cache_list.size(); ++i) {
                inp[i] = inp_cache_list[i].second->at(example_id);
            }
            /*for (auto& compress_program: compress_program_list) {
                auto compress_type = compress_program.first;
                auto* ltc = dynamic_cast<TLabeledCompress*>(compress_type.get());
                if (!ltc) {
                    AuxProgram aux = {compress_program, {nullptr, nullptr}};
                    inp.push_back(base_example_space->runAux(example_id, aux));
                } else {
                    for (auto& aux_program: f_res_list[ltc->id].component_list) {
                        AuxProgram aux = {compress_program, aux_program.program};
                        inp.push_back(base_example_space->runAux(example_id, aux));
                    }
                }
            }*/

            std::vector<int> path; int cache_id = 0;
            auto oup = runOutput(oup_ty, example_id, path, cache_id);
            insertExample({inp, oup});
        }

        void collectOutputCache(const Ty& type, std::vector<int>& path) {
            if (type->getType() == TyType::TUPLE) {
                auto* tt = dynamic_cast<TyTuple*>(type.get());
                for (int i = 0; i < tt->fields.size(); ++i) {
                    path.push_back(i);
                    collectOutputCache(tt->fields[i], path);
                    path.pop_back();
                }
                return;
            }
            if (type->getType() == TyType::COMPRESS) {
                auto* ct = dynamic_cast<TyLabeledCompress*>(type.get());
                for (auto& component: f_res_list[ct->id].component_list) {
                    auto* cache = base_example_space->getOupCache(component.program.second, path, 0);
                    if (!cache) {
                        LOG(INFO) << "Unknown cache";
                        base_example_space->registerOupCache(component.program.second, path, {});
                        cache = base_example_space->getOupCache(component.program.second, path, 0);
                    }
                    oup_cache_list.emplace_back(std::make_pair(component.program.second, path), cache);
                }
                return;
            }
            auto* cache = base_example_space->getOupCache(nullptr, path, 0); assert(cache);
            oup_cache_list.emplace_back(std::make_pair(PProgram(nullptr), path), cache);
        }
        void extendCache(int target_num) {
            for (auto& [aux, cache_item]: inp_cache_list) base_example_space->extendAuxCache(aux, cache_item, target_num);
            for (auto& [comp, cache_item]: oup_cache_list) base_example_space->extendOupCache(comp.first, comp.second, cache_item, target_num);
        }
        CExampleSpace(int _align_id, FExampleSpace* _base_example_space, IncreAutoLifterSolver* source):
                align_id(_align_id), base_example_space(_base_example_space) {
            env = source->env; f_res_list = source->f_res_list;
            oup_ty = source->info->align_infos[align_id]->oup_type;
            compress_program_list = source->compress_res_list[align_id].compress_list;

            int init_example_num = base_example_space->example_list.size();
            current_pos = (init_example_num + 1) / KExampleEnlargeFactor;

            // collect input types
            for (auto& [compress_type, compress_program]: compress_program_list) {
                auto* ltc = dynamic_cast<TLabeledCompress*>(compress_type.get());
                if (!ltc) {
                    inp_type_list.push_back(compress_type);
                } else {
                    for (auto& aux_program: f_res_list[ltc->id].component_list) {
                        inp_type_list.push_back(aux_program.program.first);
                    }
                }
            }

            // Initialize cache list
            for (auto& compress_program: compress_program_list) {
                auto compress_type = compress_program.first;
                auto* ltc = dynamic_cast<TLabeledCompress*>(compress_type.get());
                if (!ltc) {
                    AuxProgram aux = {compress_program, {nullptr, nullptr}};
                    auto* cache_item = base_example_space->getAuxCache(aux, 0);
                    if (!cache_item) {
                        LOG(INFO) << "Unknown cache for " << aux2String(aux);
                        base_example_space->registerAuxCache(aux, {});
                        cache_item = base_example_space->getAuxCache(aux, 0);
                    }
                    inp_cache_list.emplace_back(aux, cache_item);
                } else {
                    for (auto& aux_program: f_res_list[ltc->id].component_list) {
                        AuxProgram aux = {compress_program, aux_program.program};
                        auto* cache_item = base_example_space->getAuxCache(aux, 0);
                        if (!cache_item) {
                            LOG(INFO) << "Unknown cache for " << aux2String(aux);
                            base_example_space->registerAuxCache(aux, {});
                            cache_item = base_example_space->getAuxCache(aux, 0);
                        }
                        inp_cache_list.emplace_back(aux, cache_item);
                    }
                }
            }
            std::vector<int> path;
            collectOutputCache(oup_ty, path);
            //LOG(INFO) << "build " << oup_ty->toString() << " " << oup_cache_list.size();

            extendCache(current_pos);
            for (int i = 0; i < current_pos; ++i) {
                buildExample(i);
            }
        }

        int extendExample() {
            int target_num = current_pos * KExampleEnlargeFactor;
            auto* guard = new TimeGuard(KExampleTimeOut);
            target_num = base_example_space->acquireExample(target_num, guard);
            extendCache(target_num);
            delete guard;
            for (;current_pos < target_num; ++current_pos) {
                buildExample(current_pos);
            }
            return target_num;
        }

        bool isValid(const PProgram& program) {
            for (auto& [inp, oup]: example_list) {
                if (!(env->run(program.get(), inp) == oup)) {
                    return false;
                }
            }
            return true;
        }
    };

    PProgram _mergeComponentProgram(TyData* type, int& pos, const ProgramList& program_list, const std::vector<FRes>& f_res_list, Env* env) {
        if (type->getType() == TyType::COMPRESS) {
            auto* ct = dynamic_cast<TyLabeledCompress*>(type); assert(ct);
            int size = f_res_list[ct->id].component_list.size();
            if (size == 0) return program::buildConst(Data(std::make_shared<VUnit>()));
            if (size == 1) return program_list[pos++];
            ProgramList sub_list;
            for (int i = 0; i < size; ++i) sub_list.push_back(program_list[pos++]);
            return std::make_shared<Program>(env->getSemantics("prod"), sub_list);
        }
        if (type->getType() == TyType::TUPLE) {
            auto* tt = dynamic_cast<TyTuple*>(type);
            ProgramList sub_list;
            for (auto& sub: tt->fields) {
                sub_list.push_back(_mergeComponentProgram(sub.get(), pos, program_list, f_res_list, env));
            }
            return std::make_shared<Program>(env->getSemantics("prod"), sub_list);
        }
        return program_list[pos++];
    }

    PProgram _mergeComponentProgram(TyData* type, const ProgramList& program_list, const std::vector<FRes>& f_res_list, Env* env) {
        int pos = 0;
        auto res = _mergeComponentProgram(type, pos, program_list, f_res_list, env);
        assert(pos == program_list.size());
        return res;
    }

    TypeList _getInpTypes(Grammar* grammar) {
        TypeList res;
        for (auto* symbol: grammar->symbol_list) {
            for (auto* rule: symbol->rule_list) {
                auto* cr = dynamic_cast<ConcreteRule*>(rule); assert(cr);
                auto* ps = dynamic_cast<ParamSemantics*>(cr->semantics.get());
                if (ps) {
                    while (res.size() <= ps->id) res.emplace_back();
                    if (res[ps->id]) assert(res[ps->id]->equal(ps->oup_type.get()));
                    else res[ps->id] = ps->oup_type;
                }
            }
        }
        for (int i = 0; i < res.size(); ++i) {
            if (!res[i]) LOG(WARNING) << "Unused parameter " << i;
        }
        return res;
    }

    typedef std::pair<SolverToken, InvokeConfig> SolverConfig;

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

    std::pair<std::vector<int>, IOExampleList> _simplifyExampleSpace(const IOExampleList& example_list, const std::vector<int>& related_indexes) {
        assert(example_list.size());
        std::vector<bool> is_used(example_list[0].first.size(), false);
        for (auto index: related_indexes) is_used[index] = true;
        for (auto index: related_indexes) {
            is_used[index] = false;
            if (!_isDistinguishAllExamples(is_used, example_list)) is_used[index] = true;
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

    PProgram _synthesis(Grammar* grammar, const IOExampleList& example_list, const PEnv& env, const SolverConfig& config,
                        const TypeList& inp_types, const std::vector<int>& related_indexes) {
        const std::string default_name = "func";
        if (example_list.empty()) {
            return ::grammar::getMinimalProgram(grammar);
        }
        auto [used_indices, simplified_examples] = _simplifyExampleSpace(example_list, related_indexes);
        auto example_space = example::buildFiniteIOExampleSpace(simplified_examples, default_name, env.get(), inp_types);
        auto param_type_list = _getInpTypes(grammar);
        auto* simplified_grammar = _simplifyGrammar(grammar, used_indices);

        auto info = std::make_shared<SynthInfo>(default_name, _getInpTypes(simplified_grammar),
                                                simplified_grammar->start->type, simplified_grammar);
        auto* spec = new Specification({info}, env, example_space);
        auto* v = new FiniteExampleVerifier(example_space.get());

        auto res = invoker::synthesis(spec, v, config.first, nullptr, config.second);
        delete v; delete spec; delete simplified_grammar;
        return _recoverProgram(param_type_list, used_indices, res[default_name]);
    }

    std::string _path2String(const std::vector<int>& path) {
        std::string res = "[";
        for (int i = 0; i < path.size(); ++i) {
            if (i) res += ","; res += std::to_string(path[i]);
        }
        return res + "]";
    }

    RelatedComponents _getComponentList(const std::map<std::vector<int>, std::vector<RelatedComponents>>& records,
                                        const std::vector<int>& path) {
        {
            auto it = records.find(path);
            if (it != records.end()) {
                assert(it->second.size() == 1);
                return it->second[0];
            }
        }
        assert(!path.empty()); int size = path.size();
        auto new_path = path; int last = new_path[size - 1]; new_path.pop_back();
        auto it = records.find(new_path);
        assert(it != records.end() && it->second.size() > last);
        return it->second[last];
    }

    std::vector<int> _getRelatedIndexes(const std::map<std::vector<int>, std::vector<RelatedComponents>>& records,
                                        const std::vector<int>& path, const std::vector<std::pair<int, int>>& param_list) {
        auto related_component = _getComponentList(records, path);
        std::map<std::pair<int, int>, int> index_map;
        for (int i = 0; i < param_list.size(); ++i) index_map[param_list[i]] = i;
        std::vector<int> related_indexes;
        for (auto& related_pair: related_component) {
            assert(index_map.find(related_pair) != index_map.end());
            related_indexes.push_back(index_map[related_pair]);
        }
        return related_indexes;
    }

    PProgram _synthesisCombinator(CExampleSpace* example_space, const std::vector<_OutputCase>& component_info_list, IncreAutoLifterSolver* solver,
                                  const std::map<std::vector<int>, std::vector<RelatedComponents>>& records, const std::vector<std::pair<int, int>>& param_list) {
        ProgramList res_list;
        for (auto& component_info: component_info_list) {
            std::vector<int> related_indexes = _getRelatedIndexes(records, component_info.path, param_list);

            IOExampleList component_example_list;
            for (auto& [inp, oup]: example_space->example_list) {
                auto oup_component = component_info.extract(oup);
                component_example_list.emplace_back(inp, oup_component);
            }

            // synthesis
            auto oup_type = component_info.program.first;
            auto* grammar = solver->buildCombinatorGrammar(example_space->inp_type_list, oup_type, example_space->align_id);

            auto solver_config = incre::autolifter::util::getSolverToken(oup_type.get());
            PProgram main = _synthesis(grammar, component_example_list, example_space->env, solver_config, example_space->inp_type_list, related_indexes);
            /*LOG(INFO) << "Synthesize " << main->toString() << " from ";
            for (int i = 0; i < 10 && i < component_example_list.size(); ++i) {
                LOG(INFO) << "  " << example::ioExample2String(component_example_list[i]);
            }*/
            res_list.push_back(main);
        }
        return _mergeComponentProgram(example_space->oup_ty.get(), res_list, example_space->f_res_list, example_space->env.get());
    }
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

    Term _buildProgram(Program* program, const incre::grammar::SynthesisComponentList& component_list, const TermList& param_list) {
        auto *ps = dynamic_cast<ParamSemantics *>(program->semantics.get());
        if (ps) return param_list[ps->id];
        TermList sub_list;
        for (const auto& sub: program->sub_list) sub_list.push_back(_buildProgram(sub.get(), component_list, param_list));
        return _buildSingleStep(program->semantics, component_list, sub_list);
    }

    bool _isSymbolTerm(TermData* term) {
        return term->getType() == TermType::VALUE || term->getType() == TermType::VAR;
    }
}

Term IncreAutoLifterSolver::synthesisCombinator(int align_id) {
    auto* example_space = new CExampleSpace(align_id, example_space_list[align_id], this);
    auto output_cases = _collectOutputCase(align_id, this);
    {
        LOG(INFO) << "Synthesize for align@" << align_id;
        LOG(INFO) << "Output cases " << output_cases.size();
        for (auto &component: output_cases) {
            std::cout << "  " << _path2String(component.path) << " ";
            if (!component.program.second) std::cout << "null" << std::endl;
            else std::cout << component.program.second->toString() << std::endl;
        }
        LOG(INFO) << "Input list";
        int input_id = 0;
        for (auto &[type, prog]: example_space->compress_program_list) {
            auto *ltc = dynamic_cast<TLabeledCompress *>(type.get());
            if (ltc) {
                for (auto &align_prog: f_res_list[ltc->id].component_list) {
                    std::cout << "  [" << input_id++ << "] " << prog->toString() << " -> " << align_prog.program.second->toString() << std::endl;
                }
            } else {
                std::cout << "  [" << input_id++ << "] " << prog->toString() << std::endl;
            }
        }
    }

    PProgram res = nullptr;
    std::vector<std::pair<int, int>> param_positions;
    for (int i = 0; i < compress_res_list[align_id].compress_list.size(); ++i) {
        auto* lt = dynamic_cast<TLabeledCompress*>(compress_res_list[align_id].compress_list[i].first.get());
        if (lt) {
            auto& f_res = f_res_list[lt->id];
            for (int j = 0; j < f_res.component_list.size(); ++j) param_positions.emplace_back(i, j);
        } else param_positions.emplace_back(i, -1);
    }

    while (!res || !example_space->isValid(res)) {
        res = _synthesisCombinator(example_space, output_cases, this, align_result_records[align_id], param_positions);
        example_space->extendExample();
    }

    // Build Param List
    TermList compress_param_list;
    for (auto& [var_name, var_type]: info->align_infos[align_id]->inp_types) {
        compress_param_list.push_back(std::make_shared<TmVar>(var_name));
    }
    for (auto& [var_name, var_type]: info->example_pool->input_list) {
        compress_param_list.push_back(std::make_shared<TmVar>(var_name));
    }

    std::vector<std::pair<std::string, Term>> binding_list;
    TermList combine_param_list;
    int var_id = 0;

    for (auto& [compress_type, compress_program]: compress_res_list[align_id].compress_list) {
        // LOG(INFO) << "build compress " << compress_program->toString();
        // for (auto& param: compress_param_list) LOG(INFO) << "  param: " << param->toString();
        auto compress_term = _buildProgram(compress_program.get(), info->component_pool.compress_list, compress_param_list);
        if (!_isSymbolTerm(compress_term.get())) {
            std::string compress_name = "c" + std::to_string(var_id++);
            binding_list.emplace_back(compress_name, compress_term);
            compress_term = std::make_shared<TmVar>(compress_name);
        }

        auto* lt = dynamic_cast<TLabeledCompress*>(compress_type.get());
        if (!lt) {
            combine_param_list.push_back(compress_term);
        } else {
            int compress_id = lt->id;
            int component_num = f_res_list[compress_id].component_list.size();
            if (!component_num) continue;
            if (component_num == 1) {
                combine_param_list.push_back(compress_term);
            } else {
                for (int i = 1; i <= component_num; ++i) {
                    combine_param_list.push_back(std::make_shared<TmProj>(compress_term, i));
                }
            }
        }
    }

    auto term = _buildProgram(res.get(), info->component_pool.comb_list, combine_param_list);
    std::reverse(binding_list.begin(), binding_list.end());
    for (auto& [name, binding]: binding_list) {
        term = std::make_shared<TmLet>(name, binding, term);
    }

    return term;
}

#include "istool/basic/config.h"

void IncreAutoLifterSolver::solveCombinators() {
    for (int pass_id = 0; pass_id < info->align_infos.size(); ++pass_id) {
        global::printStageResult("  Synthesizing sketch hole " + std::to_string(pass_id + 1) + "/" + std::to_string(info->align_infos.size()));
        comb_list.push_back(synthesisCombinator(pass_id));
    }
    auto comb_size = 0;
    for (auto& comb: comb_list) comb_size += incre::getTermSize(comb.get());
    global::recorder.record("comb-size", comb_size);
}

namespace {
    PType _getCompressType(IncreInfo *info, int compress_id) {
        for (const auto &align_info: info->align_infos) {
            for (auto &[name, ty]: align_info->inp_types) {
                if (ty->getType() == TyType::COMPRESS) {
                    auto *cty = dynamic_cast<TyLabeledCompress *>(ty.get());
                    if (cty && cty->id == compress_id) return incre::typeFromIncre(cty->content);
                }
            }
        }
        LOG(FATAL) << "Compress #" << compress_id << " not found";
    }
}

TermList IncreAutoLifterSolver::buildFRes() {
    TermList result;
    for (int compress_id = 0; compress_id < f_res_list.size(); ++compress_id) {
        auto compress_type = _getCompressType(info, compress_id);
        std::string compress_name = "ds";
        TermList param_list; param_list.push_back(std::make_shared<TmVar>(compress_name));
        for (auto& [var_name, var_type]: info->example_pool->input_list) {
            param_list.push_back(std::make_shared<TmVar>(var_name));
        }

        TermList fields;
        for (auto& component_info: f_res_list[compress_id].component_list) {
            LOG(INFO) << component_info.program.second->toString();
            fields.push_back(_buildProgram(component_info.program.second.get(), info->component_pool.align_list, param_list));
        }

        if (fields.empty()) {
            auto data = Data(std::make_shared<VUnit>());
            result.push_back(std::make_shared<TmValue>(data));
        } else if (fields.size() == 1) {
            result.push_back(std::make_shared<TmAbs>(compress_name, typeToIncre(compress_type.get()), fields[0]));
        } else {
            auto res = std::make_shared<TmTuple>(fields);
            result.push_back(std::make_shared<TmAbs>(compress_name, typeToIncre(compress_type.get()), res));
        }
    }
    return result;
}