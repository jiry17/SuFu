//
// Created by pro on 2023/7/17.
//

#include "istool/incre/autolifter/incre_nonscalar_autolifter.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/incre/autolifter/incre_solver_util.h"
#include "glog/logging.h"
#include "istool/ext/deepcoder/data_type.h"

using namespace incre::autolifter;
using namespace incre;
namespace {
    struct InpVarInfo {
        Ty type;
        std::string root_name, current_name;
        Term term;
        int depth;
        InpVarInfo() = default;
        InpVarInfo(const Ty& _type, const Term& _term, const std::string& _root_name, const std::string& _current_name, int _depth):
            type(_type), term(_term), root_name(_root_name), current_name(_current_name), depth(_depth) {
        }
    };

    PProgram _synthesisScalar(const TypeList& inp_type_list, const PType& oup_type, const IOExampleList& example_list, IncreNonScalarSolver* solver, int align_id) {
        auto* grammar = solver->info->component_pool.buildCombinatorGrammar(inp_type_list, oup_type, align_id);
        auto res = incre::autolifter::util::synthesis2Program(inp_type_list, oup_type, solver->env, grammar, example_list);
        delete grammar;
        return res;
    }

    // _fill(pattern_list[index], branch_example_list, solver, align_id, scalar_param_list, full_param_list

    Term _fill(const Pattern& pattern, const IOExampleList& example_list, IncreNonScalarSolver* solver, int align_id,
               const PType& type, const TypeList& scalar_inp_type, const TermList& scalar_inp_term, const TermList& full_param_list) {
        /*LOG(INFO) << "fill " << pattern->toString() << " align #" << align_id << " type: " << type->getName();
        LOG(INFO) << "example num " << example_list.size();
        for (int i = 0; i < example_list.size() && i <= 10; ++i) {
            LOG(INFO) << "  " << example::ioExample2String(example_list[i]);
        }
        int kk; std::cin >> kk;*/
        for (auto& [inp, _]: example_list) assert(inp.size() == scalar_inp_type.size());
        switch (pattern->getType()) {
            case PatternType::VAR: {
                auto* pv = dynamic_cast<PtVar*>(pattern.get());
                return full_param_list[std::stoi(pv->name)];
            }
            case PatternType::UNDER_SCORE: {
                auto prog = _synthesisScalar(scalar_inp_type, type, example_list, solver, align_id);
                return util::program2Term(prog.get(), solver->info->component_pool.comb_list, scalar_inp_term);
            }
            case PatternType::TUPLE: {
                auto* pt = dynamic_cast<PtTuple*>(pattern.get());
                auto* tt = dynamic_cast<TProduct*>(type.get());
                assert(pt && tt && pt->pattern_list.size() == tt->sub_types.size());
                TermList sub_list;
                for (int i = 0; i < pt->pattern_list.size(); ++i) {
                    IOExampleList field_example_list;
                    for (auto& [inp, oup]: example_list) {
                        auto* vt = dynamic_cast<VTuple*>(oup.get());
                        assert(vt && vt->elements.size() == pt->pattern_list.size());
                        field_example_list.emplace_back(inp, vt->elements[i]);
                    }
                    sub_list.emplace_back(_fill(pt->pattern_list[i], field_example_list, solver, align_id, tt->sub_types[i],
                                                scalar_inp_type, scalar_inp_term, full_param_list));
                }
                return std::make_shared<TmTuple>(sub_list);
            }
            case PatternType::CONSTRUCTOR: {
                auto* pc = dynamic_cast<PtConstructor*>(pattern.get());
                auto* tt = dynamic_cast<TIncreInductive*>(type.get());
                assert(tt);
                PType sub_type;
                for (auto& [cname, cty]: tt->type->constructors) {
                    if (cname == pc->name) sub_type = incre::typeFromIncre(incre::subst(cty, tt->type->name, tt->_type));
                }
                assert(sub_type);
                IOExampleList content_example_list;
                for (auto& [inp, oup]: example_list) {
                    auto* vi = dynamic_cast<VInductive*>(oup.get());
                    assert(vi && vi->name == pc->name);
                    content_example_list.emplace_back(inp, vi->content);
                }
                auto content_result = _fill(pc->pattern, content_example_list, solver, align_id, sub_type, scalar_inp_type, scalar_inp_term, full_param_list);
                return std::make_shared<TmApp>(std::make_shared<TmVar>(pc->name), content_result);
            }
        }
    }

    bool _isScalar(TyData* type) {
        return type->getType() == TyType::INT || type->getType() == TyType::BOOL || type->getType() == TyType::UNIT;
    }

    Term _synthesisCase(const std::vector<InpVarInfo>& var_info_list, const IOExampleList& example_list,
                        IncreNonScalarSolver* solver, int align_id) {
        LOG(INFO) << "new case with examples: " << example::ioExample2String(example_list[0]);
        auto pattern_list = comb::getDSScheme(example_list, solver->KUnfoldDepth, solver->env.get());

        LOG(INFO) << "patterns:";
        for (auto& pattern: pattern_list) LOG(INFO) << "  " << pattern->toString();

        TypeList scalar_inp_list; TermList scalar_param_list;
        IOExampleList scalar_example_list;
        {
            std::vector<int> scalar_index_list;
            for (int i = 0; i < var_info_list.size(); ++i) {
                if (_isScalar(var_info_list[i].type.get())) {
                    scalar_index_list.push_back(i);
                    scalar_inp_list.push_back(incre::typeFromIncre(var_info_list[i].type));
                    scalar_param_list.push_back(var_info_list[i].term);
                }
            }
            for (auto& [inp, oup]: example_list) {
                DataList new_inp;
                for (auto index: scalar_index_list) new_inp.push_back(inp[index]);
                scalar_example_list.emplace_back(new_inp, oup);
            }
        }


        ProgramList condition_list;
        std::vector<std::vector<int>> branch_index_storage;
        {
            std::vector<int> rem_index_list;
            for (int i = 0; i < example_list.size(); ++i) rem_index_list.push_back(i);
            for (int id = 0; id + 1 < pattern_list.size(); ++id) {
                std::vector<int> pos_index_list, neg_index_list;
                for (auto example_id: rem_index_list) {
                    auto& example = example_list[example_id];
                    if (!comb::isMatchUsage(pattern_list[id], example.second, example.first)) {
                        neg_index_list.emplace_back(example_id);
                    } else {
                        bool is_pos = true;
                        for (int i = id + 1; i < pattern_list.size(); ++i) {
                            if (comb::isMatchUsage(pattern_list[i], example.second, example.first)) {
                                is_pos = false; break;
                            }
                        }
                        if (is_pos) pos_index_list.push_back(example_id);
                    }
                }
                IOExampleList cond_example_list;
                for (auto index: pos_index_list) cond_example_list.emplace_back(scalar_example_list[index].first, BuildData(Bool, true));
                for (auto index: neg_index_list) cond_example_list.emplace_back(scalar_example_list[index].first, BuildData(Bool, false));
                auto cond = _synthesisScalar(scalar_inp_list, std::make_shared<TBool>(), cond_example_list, solver, align_id);
                condition_list.push_back(cond);
                int now = 0;
                std::vector<int> branch_index_list;
                for (auto index: rem_index_list) {
                    if (!solver->env->run(cond.get(), scalar_example_list[index].first).isTrue()) {
                        rem_index_list[++now] = index;
                    } else {
                        branch_index_list.push_back(index);
                    }
                }
                rem_index_list.resize(now);
                branch_index_storage.push_back(branch_index_list);
            }
            branch_index_storage.push_back(rem_index_list);
        }

        TermList case_list; TermList full_param_list;
        auto align_oup_type = incre::typeFromIncre(solver->getFinalType(solver->info->align_infos[align_id]->oup_type));
        for (auto& info: var_info_list) full_param_list.push_back(info.term);
        for (int index = 0; index < branch_index_storage.size(); ++index) {
            IOExampleList branch_example_list;
            for (auto i: branch_index_storage[index]) {
                branch_example_list.push_back(scalar_example_list[i]);
            }
            case_list.push_back(_fill(pattern_list[index], branch_example_list, solver, align_id, align_oup_type, scalar_inp_list, scalar_param_list, full_param_list));
        }

        Term result = case_list[int(case_list.size()) - 1];
        for (int i = int(case_list.size()) - 2; i >= 0; --i) {
            auto cond_term = util::program2Term(condition_list[i].get(), solver->info->component_pool.comb_list, scalar_param_list);
            result = std::make_shared<TmIf>(cond_term, case_list[i], result);
        }
        return result;
    }

    Term _synthesis(int id, const std::vector<InpVarInfo>& var_info_list, const IOExampleList& example_list,
                    IncreNonScalarSolver* solver, int align_id, const std::unordered_map<std::string, int>& unfold_plan) {
        if (id == var_info_list.size()) {
            return _synthesisCase(var_info_list, example_list, solver, align_id);
        }
        // LOG(INFO) << "current type " << var_info_list[id].type->toString();
        switch (var_info_list[id].type->getType()) {
            case TyType::INT:
            case TyType::BOOL:
            case TyType::UNIT: {
                return _synthesis(id + 1, var_info_list, example_list, solver, align_id, unfold_plan);
            }
            case TyType::ARROW:
            case TyType::COMPRESS:
            case TyType::VAR: {
                LOG(FATAL) << "Unsupported input type: " << var_info_list[id].type->toString();
            }
            case TyType::TUPLE: {
                std::vector<InpVarInfo> new_info_list;
                for (int i = 0; i < var_info_list.size(); ++i) {
                    if (i != id) new_info_list.push_back(var_info_list[i]);
                }
                auto& var_info = var_info_list[id];
                auto* tt = dynamic_cast<TyTuple*>(var_info.type.get());
                for (int i = 0; i < tt->fields.size(); ++i) {
                    auto new_term = std::make_shared<TmProj>(var_info.term, i + 1);
                    std::string new_name = var_info.current_name + "@" + std::to_string(i);
                    new_info_list.emplace_back(tt->fields[i], new_term, var_info.root_name, new_name, var_info.depth);
                }
                IOExampleList new_example_list;
                for (auto& [inp, oup]: example_list) {
                    DataList new_inp;
                    for (int i = 0; i < inp.size(); ++i) {
                        if (i != id) new_inp.push_back(inp[i]);
                    }
                    auto* vt = dynamic_cast<VTuple*>(inp[id].get());
                    assert(vt);
                    for (auto& sub_v: vt->elements) new_inp.push_back(sub_v);
                    new_example_list.emplace_back(new_inp, oup);
                }
                return _synthesis(id, new_info_list, new_example_list, solver, align_id, unfold_plan);
            }
            case TyType::IND: {
                auto& var_info = var_info_list[id];
                auto it = unfold_plan.find(var_info.root_name);
                assert(it != unfold_plan.end());
                if (var_info.depth == it->second) {
                    return _synthesis(id + 1, var_info_list, example_list, solver, align_id, unfold_plan);
                }
                std::unordered_set<std::string> involved_list;
                for (auto& [inp, oup]: example_list) {
                    auto* vi = dynamic_cast<VInductive*>(inp[id].get());
                    assert(vi);
                    involved_list.insert(vi->name);
                }
                std::vector<std::pair<Pattern, Term>> case_list;
                for (auto& name: involved_list) {
                    auto case_var_name = var_info.current_name + "@" + std::to_string(0);
                    auto case_pattern = std::make_shared<PtConstructor>(name, std::make_shared<PtVar>(case_var_name));
                    Ty case_type;
                    auto* ti = dynamic_cast<TyInductive*>(var_info.type.get());
                    for (auto& [cname, ctype]: ti->constructors) {
                        if (name == cname) case_type = ctype;
                    }
                    assert(case_type);
                    case_type = incre::subst(case_type, ti->name, var_info.type);
                    auto case_info_list = var_info_list;
                    case_info_list.emplace_back(case_type, std::make_shared<TmVar>(case_var_name), var_info.root_name, case_var_name, var_info.depth + 1);

                    IOExampleList case_example_list;
                    for (auto& [inp, oup]: example_list) {
                        auto* vi = dynamic_cast<VInductive*>(inp[id].get());
                        if (vi->name != name) continue;
                        auto new_inp = inp; new_inp.emplace_back(vi->content);
                        case_example_list.emplace_back(new_inp, oup);
                    }
                    case_list.emplace_back(case_pattern, _synthesis(id + 1, case_info_list, case_example_list, solver, align_id, unfold_plan));
                }
                return std::make_shared<TmMatch>(var_info.term, case_list);
            }
        }
    }
}

Ty IncreNonScalarSolver::getFinalType(const Ty &type) {
    TyList final_type_list;
    for (auto& [ty, _]: align_list) final_type_list.push_back(incre::typeToIncre(ty.get()));
    return incre::getFinalType(type, final_type_list);
}

namespace {
    bool _isValidUnfold(const std::vector<int>& unfold_plan, int oup_depth, const IOExampleList& example_list) {
        std::unordered_map<std::string, std::string> known_scalar;
        for (auto& [inp, oup]: example_list) {
            InputUnfoldInfo inp_info;
            for (int i = 0; i < inp.size(); ++i) {
                autolifter::mergeInputInfo(inp_info, unfoldInput(inp[i], unfold_plan[i]));
            }
            std::unordered_map<std::string, Data> oup_info;
            if (!unfoldOutput(oup, inp_info, oup_depth, oup_info)) return false;
            std::string feature = inp_info.structure_feature + "@" + data::dataList2String(inp_info.scalar_input);
            for (auto& [path, v]: oup_info) {
                auto full_feature = feature + "@" + path;
                auto it = known_scalar.find(full_feature);
                if (it == known_scalar.end()) {
                    known_scalar[full_feature] = v.toString();
                } else if (it->second != v.toString()) {
                    return false;
                }
            }
        }
        return true;
    }

    std::vector<int> _getUnfoldPlan(const IOExampleList& example_list, int depth_limit) {
        assert(!example_list.empty()); int n = example_list[0].first.size();
        std::vector<int> result(n, depth_limit);
        for (int i = 0; i < n; ++i) {
            while (result[i]) {
                result[i]--;
                if (!_isValidUnfold(result, depth_limit, example_list)) {
                    result[i]++; break;
                }
            }
        }
        return result;
    }
}

Term IncreNonScalarSolver::synthesisComb(int align_id) {
    // TODO: somehow extend the example space

    int example_num = 1000;
    auto* guard = new TimeGuard(KExampleTimeOut);
    info->example_pool->generateBatchedExample(align_id, example_num, guard);
    example_num = std::min(example_num, int(info->example_pool->example_pool[align_id].size()));

    std::vector<InpVarInfo> inp_info_list;
    for (auto& [var_name, var_type]: info->align_infos[align_id]->inp_types) {
        //LOG(INFO) << "transform for type " << var_type->toString();
        auto final_var_type = getFinalType(var_type);
        inp_info_list.emplace_back(final_var_type, std::make_shared<TmVar>(var_name), var_name, var_name, 0);
    }
    IOExampleList example_list;
    for (int i = 0; i < example_num; ++i) {
        DataList inp_list;
        auto& example = info->example_pool->example_pool[align_id][i];
        for (auto& info: inp_info_list) {
            auto inp_v = autolifter::executeCompress(example->local_inputs[info.root_name], align_list, env.get());
            inp_list.push_back(inp_v);
        }
        auto oup_v = autolifter::executeCompress(example->oup, align_list, env.get());
        example_list.emplace_back(inp_list, oup_v);
    }

    auto depth_list = _getUnfoldPlan(example_list, this->KUnfoldDepth);
    std::unordered_map<std::string, int> unfold_plan;
    assert(depth_list.size() == inp_info_list.size());
    for (int i = 0; i < inp_info_list.size(); ++i) {
        unfold_plan[inp_info_list[i].root_name] = depth_list[i];
    }
    auto result = _synthesis(0, inp_info_list, example_list, this, align_id, unfold_plan);
    LOG(INFO) << "align #" << align_id << ": " << result->toString();
    return result;
}