//
// Created by pro on 2022/9/26.
//

#include "istool/incre/autolifter/incre_plp_solver.h"
#include "glog/logging.h"
#include "istool/basic/config.h"
#include "istool/incre/trans/incre_trans.h"
#include <iostream>

using namespace incre::autolifter;
using solver::autolifter::MaximalInfoList;
using solver::autolifter::EnumerateInfo;


UnitInfo::UnitInfo(const AuxProgram &_program, const Bitset &_info, bool _is_error): program(_program), info(_info), is_error(_is_error) {
}

AuxProgramEvaluateUtil::AuxProgramEvaluateUtil(PLPTask *_task): task(_task) {
}

namespace {
    class BasicAuxProgramEvaluateUtil: public AuxProgramEvaluateUtil {
    public:
        BasicAuxProgramEvaluateUtil(PLPTask* _task): AuxProgramEvaluateUtil(_task) {
        }
        virtual Data execute(const AuxProgram& program, int example_id) {
            return task->runInp(example_id, program);
        }
        virtual std::vector<AuxProgram> constructAuxProgram(const AuxProgram& program) {
            return {program};
        }
        virtual std::vector<AuxProgram> getDefaultAuxPrograms() {
            return {};
        }
    };

    class VarMergedAuxProgramEvaluateUtil: public AuxProgramEvaluateUtil {
    public:
        std::unordered_map<int, std::pair<int, std::vector<int>>> compress_var_pool;
        std::unordered_map<int, int> compress_var_id;
        bool is_include_direct;
        std::vector<AuxProgram> default_list;

        VarMergedAuxProgramEvaluateUtil(PLPTask* _task, bool _is_include_direct): AuxProgramEvaluateUtil(_task), is_include_direct(_is_include_direct) {
            for (int i = 0; i < task->example_space->value_list.size(); ++i) {
                auto& [name, type] = task->example_space->value_list[i];
                auto* lct = dynamic_cast<incre::TLabeledCompress*>(type.get());
                if (lct) {
                    int id = lct->id;
                    if (compress_var_pool.count(id) == 0) {
                        compress_var_pool[id] = {i, {}};
                    }
                    compress_var_pool[id].second.push_back(i);
                    compress_var_id[i] = id;
                } else if (is_include_direct) {
                    TypedProgram compress(type, program::buildParam(i)), aux(nullptr, nullptr);
                    default_list.emplace_back(compress, aux);
                }
            }
        }
        virtual std::vector<AuxProgram> constructAuxProgram(const AuxProgram& program) {
            auto* sp = dynamic_cast<ParamSemantics*>(program.first.second->semantics.get());
            if (is_include_direct) {
                if (!program.second.first && sp) return {};
            }
            if (!program.second.first) return {program};

            std::vector<AuxProgram> result = {program};
            std::vector<TypedProgram> extract_list = {program.first};
            // Programs derived from the same var
            if (sp) {
                int param_id = sp->id; assert(compress_var_id.count(param_id));
                int compress_id = compress_var_id[param_id];
                auto it = compress_var_pool.find(compress_id);
                assert(it != compress_var_pool.end());
                if (param_id != it->second.first) return {};

                for (int var_id: it->second.second) {
                    if (var_id == sp->id) continue;
                    auto compress_program = std::make_pair(program.first.first, program::buildParam(var_id, sp->oup_type));
                    extract_list.push_back(compress_program);
                    result.emplace_back(compress_program, program.second);
                }
            }

            auto* tc = dynamic_cast<incre::TLabeledCompress*>(program.first.first.get());
            if (config::KIsDefaultSelf && tc && tc->id == task->oup_compress_id) {
                for (auto& extract: extract_list) {
                    result.emplace_back(extract, task->target);
                }
            }

            return result;
        }
        virtual std::vector<AuxProgram> getDefaultAuxPrograms() {
            return default_list;
        }
        virtual Data execute(const AuxProgram& program, int example_id) {
            DataList res;
            for (auto& derived_program: constructAuxProgram(program)) {
                res.push_back(task->runInp(example_id, derived_program));
            }
            return BuildData(Product, res);
        }
    };
}

namespace {
    int KDefaultComposedNum = 3;
    int KDefaultExtraTurnNum = 100;
    int KDefaultVerifyBaseNum = 1000;
    int KDefaultExampleTimeOut = 10;
    int KDefaultEnlargeFactor = 2;
    bool KDefaultIsMergeVar = true;
    bool KDefaultIsIncludeDirect = false;
}

const std::string incre::autolifter::KIsMergeVarName = "IncreAutoLifter@IsMergeVar";
const std::string incre::autolifter::KIsIncludeDirectValueName = "IncreAutoLifter@IsIncludeVar";

IncrePLPSolver::IncrePLPSolver(Env *_env, PLPTask *_task): env(_env), task(_task) {
    auto* d = env->getConstRef(solver::autolifter::KComposedNumName, BuildData(Int, KDefaultComposedNum));
    KComposedNum = theory::clia::getIntValue(*d);
    d = env->getConstRef(solver::autolifter::KExtraTurnNumName, BuildData(Int, KDefaultExtraTurnNum));
    KExtraTurnNum = theory::clia::getIntValue(*d);
    d = env->getConstRef(solver::autolifter::KOccamExampleNumName, BuildData(Int, KDefaultVerifyBaseNum));
    KVerifyBaseNum = theory::clia::getIntValue(*d);
    KExampleTimeOut = KDefaultExampleTimeOut;
    KExampleEnlargeFactor = KDefaultEnlargeFactor;

    d = env->getConstRef(KIsMergeVarName, BuildData(Bool, KDefaultIsMergeVar));
    auto is_var = env->getConstRef(KIsIncludeDirectValueName, BuildData(Bool, KDefaultIsIncludeDirect));
    if (d->isTrue()) evaluate_util = new VarMergedAuxProgramEvaluateUtil(task, is_var->isTrue());
    else evaluate_util = new BasicAuxProgramEvaluateUtil(task);
}

IncrePLPSolver::~IncrePLPSolver() {
    for (auto& info_list: info_storage) {
        for (auto* info: info_list) delete info;
    }
    delete evaluate_util;
}

namespace {
    std::vector<UnitInfo> _randomMerge(const std::vector<std::vector<UnitInfo>>& info_storage, Env* env) {
        std::vector<UnitInfo> res;
        std::vector<int> pos_list(info_storage.size(), 0);
        std::vector<int> source_list;
        for (int i = 0; i < info_storage.size(); ++i) {
            for (int j = 0; j < info_storage[i].size(); ++j) source_list.push_back(i);
        }
        std::shuffle(source_list.begin(), source_list.end(), env->random_engine);
        for (auto i: source_list) {
            res.push_back(info_storage[i][pos_list[i]++]);
        }
        return res;
    }
}

UnitInfo IncrePLPSolver::init(const AuxProgram& program) {
    for (auto& example: error_example_list) {
        try {
            evaluate_util->execute(program, example);
        } catch (SemanticsError& e) {
            return {program, {}, true};
        }
    }
    Bitset info(example_list.size(), false);
    for (int i = 0; i < example_list.size(); ++i) {
        try {
            if (!(evaluate_util->execute(program, example_list[i].first) ==
                  evaluate_util->execute(program, example_list[i].second))) {
                info.set(i, true);
            }
        } catch (const SemanticsError& e) {
            return {program, {}, true};
        }
    }
    return {program, info};
}
std::string IncrePLPSolver::example2String(const std::pair<int, int> &example) {
    auto l_string = task->example_space->example2String(example.first);
    auto r_string = task->example_space->example2String(example.second);
    return "(" + l_string + ", " + r_string + ")";
}

std::vector<AuxProgram> IncrePLPSolver::unfoldComponents(const std::vector<AuxProgram> &program_list) {
    std::unordered_set<std::string> existing_set;
    std::vector<AuxProgram> result;
    auto insert = [&](const AuxProgram& program) {
        auto feature = autolifter::aux2String(program);
        if (existing_set.find(feature) != existing_set.end()) return;
        existing_set.insert(feature);
        result.push_back(program);
    };

    for (auto& program: evaluate_util->getDefaultAuxPrograms()) insert(program);
    for (auto& program: program_list) {
        for (auto& derived_program: evaluate_util->constructAuxProgram(program)) {
            insert(derived_program);
        }
    }
    return result;
}
#include "istool/basic/config.h"
void IncrePLPSolver::addErrorExample(int example_id) {
    global::recorder.start("extend-component");
    for (auto& unit: component_info_list) {
        if (unit.is_error) continue;
        try {
            evaluate_util->execute(unit.program, example_id);
        } catch (const SemanticsError& e) {
            unit.is_error = true;
        }
    }
}

void IncrePLPSolver::addExample(const std::pair<int, int> &example) {
    if (example.first == example.second) {
        addErrorExample(example.first); return;
    }
    global::recorder.start("extend-component");
    for (auto& unit: component_info_list) {
        if (unit.is_error) continue;
        try {
            auto is_valid = !(evaluate_util->execute(unit.program, example.first) ==
                              evaluate_util->execute(unit.program, example.second));
            unit.info.append(is_valid);
        } catch (const SemanticsError& e) {
            unit.is_error = true;
        }
    }
    global::recorder.end("extend-component");
    LOG(INFO) << "#Example: " << example_list.size() << " " << "#Component: " << component_info_list.size();

    /*for (auto& unit: component_info_list) {
        if (dynamic_cast<TBool*>(unit.program.first.first.get()) && aux2String(unit.program).size() <= 13) {
            LOG(INFO) << unit.info.toString() << " " << aux2String(unit.program);
        }
    }*/
    // Get sum (map neg xs)
    /*std::vector<std::string> cared_programs = {"Param2 -> app(sum,app(map,neg,Param0))", "Param2 -> app(sum,Param0)"};
    std::vector<UnitInfo> unit_list;
    for (auto& program: cared_programs) {
        for (auto& unit: component_info_list) {
            if (aux2String(unit.program) == program) {
                LOG(INFO) << unit.info.toString() << " " << program;
                unit_list.push_back(unit);
            }
        }
    }
    if (unit_list.size() == 2) {
        for (int i = 0; i < example_list.size(); ++i) {
            if (unit_list[0].info[i] != unit_list[1].info[i]) {
                LOG(INFO) << "diff example " << example2String(example_list[i]);
                LOG(INFO) << cared_programs[0] << " " << (evaluate_util->execute(unit_list[0].program, example.first)).toString() << " "
                          << (evaluate_util->execute(unit_list[0].program, example.second)).toString();
                LOG(INFO) << cared_programs[0] << " " << (evaluate_util->execute(unit_list[1].program, example.first)).toString() << " "
                          << (evaluate_util->execute(unit_list[1].program, example.second)).toString();
            }
        }
    }*/

    example_list.push_back(example);

    // clear tmp data structures in the previous turn
    for (auto& info_list: info_storage) {
        for (auto* info: info_list) delete info;
    }
    info_storage.clear(); next_component_id = 0; maximal_list.clear();
    for (auto& q: working_list) {
        while (!q.empty()) {
            delete q.front(); q.pop();
        }
    }
    uncovered_info_set.clear(); global_maximal.clear();
}

std::vector<UnitInfo> IncrePLPSolver::mergeUnits(int compress_size, int aux_size) {
    auto* compress_list = task->compress_grammar->acquirePrograms(compress_size);
    if (!compress_list) return {};

    std::vector<TypedProgramList> known_aux_list(task->aux_grammar_list.size());
    std::vector<TypedProgramList*> aux_pointer_list(task->aux_grammar_list.size());
    if (aux_size == 0) {
        for (int i = 0; i < known_aux_list.size(); ++i) {
            for (auto& f_res: task->pre_res_list[i]) {
                known_aux_list[i].push_back(f_res);
            }
            aux_pointer_list[i] = &known_aux_list[i];
        }
    } else {
        for (int i = 0; i < known_aux_list.size(); ++i) {
            aux_pointer_list[i] = task->aux_grammar_list[i]->acquirePrograms(aux_size);
        }
    }

    /*for (auto* aux: aux_pointer_list) {
        if (aux) std::cout << aux->size(); else std::cout << "null";
        std::cout << " ";
    }
    std::cout << std::endl;*/

    std::vector<UnitInfo> res_list;
    for (auto compress_it = compress_list->begin(); compress_it < compress_list->end(); ++compress_it) {
        auto program = *compress_it;
        auto* ltc = dynamic_cast<TLabeledCompress*>(program.first.get());
        if (!ltc) {
            if (aux_size == 0) {
                res_list.push_back(init({program, {nullptr, nullptr}}));
            }
        } else {
            int compress_id = ltc->id; auto* aux_list = aux_pointer_list[compress_id];
            if (!aux_list) continue;
            for (auto aux_it = aux_list->begin(); aux_it < aux_list->end(); ++aux_it) {
                res_list.push_back(init({program, *aux_it}));
            }
        }
    }

    return res_list;
}

namespace {
    const int KDelta = 0;
}

void IncrePLPSolver::getMoreComponent() {

    ++current_size;
    LOG(INFO) << "get more component";

    std::vector<std::vector<UnitInfo>> unit_storage;
    for (int compress_size = 0; compress_size < current_size; ++compress_size) {
        auto merge_result = mergeUnits(compress_size, current_size - compress_size);
        /*LOG(INFO) << "merge " << compress_size << " " << current_size - compress_size << std::endl;
        for (auto& program: merge_result) {
            LOG(INFO) << "  " << aux2String(program.program);
        }*/
        unit_storage.push_back(merge_result);
    }
    if (current_size >= KDelta) unit_storage.push_back(mergeUnits(current_size - KDelta, 0));

    for (auto& unit: _randomMerge(unit_storage, env)) {
        /*if (dynamic_cast<TBool*>(unit.program.first.first.get())) {
            LOG(INFO) << "new bool component " << aux2String(unit.program) << " " << unit.info.toString();
            int kk; std::cin >> kk;
        }*/
        component_info_list.push_back(unit);
    }
}

namespace {
    std::vector<std::pair<int, PProgram>> _merge(const std::vector<std::pair<int, TypedProgram>>& x_list,
            std::vector<std::pair<int, TypedProgram>>& y_list) {
        std::vector<std::pair<int, PProgram>> res;
        for (auto& [id, program]: x_list) res.emplace_back(id, program.second);
        for (auto& [id, program]: y_list) res.emplace_back(id, program.second);
        return res;
    }

    std::string _unitList2String(const std::vector<AuxProgram>& unit_list) {
        std::string res = "[";
        for (int i = 0; i < unit_list.size(); ++i) {
            if (i) res += ",";
            res += aux2String(unit_list[i]);
        }
        return res + "]";
    }
}

#include "istool/basic/config.h"

std::pair<int, int> IncrePLPSolver::verify(const std::vector<AuxProgram> &aux_list) {
    int total_size = 1;
    for (auto& [p_compress, p_aux]: aux_list) {
        total_size += p_compress.second->size();
        if (p_aux.second) total_size += p_aux.second->size();
    }
    verify_num = std::max(verify_num, total_size * KVerifyBaseNum);
    verify_num = task->acquireExample(verify_num, KExampleTimeOut);

    std::vector<DataList*> inp_cache_list(aux_list.size(), nullptr);
    DataStorage new_inp_storage(aux_list.size());
    for (int i = 0; i < aux_list.size(); ++i) {
        inp_cache_list[i] = task->example_space->getAuxCache(aux_list[i], verify_num);
    }
    DataList* oup_cache = task->oup_cache; task->extendOupCache(verify_num);

    std::unordered_map<std::string, std::pair<Data, int>> verify_cache;

    auto deal_example = [&](int example_id) {
        auto oup = oup_cache->at(example_id);
        DataList inp_list(aux_list.size());
        for (int i = 0; i < aux_list.size(); ++i) {
            if (inp_cache_list[i]) inp_list[i] = inp_cache_list[i]->at(example_id);
            else {
                try {
                    // LOG(INFO) << task->example_space->example_list[example_id].toString();
                    auto inp = task->example_space->runAux(example_id, aux_list[i]);
                    new_inp_storage[i].push_back(inp);
                    inp_list[i] = inp;
                } catch (const SemanticsError &e) {
                    return example_id;
                }
            }
        }
        auto feature = data::dataList2String(inp_list);
        if (verify_cache.find(feature) == verify_cache.end()) {
            verify_cache[feature] = {oup, example_id};
            return -1;
        } else {
            auto& [pre_oup, pre_id] = verify_cache[feature];
            if (pre_oup == oup) return -1;
            return pre_id;
        }
    };

    LOG(INFO) << "Prepare finished";

    for (int try_num = 0; try_num < verify_num; ++try_num) {
        verify_pos = (verify_pos + 1) % verify_num;
        auto pre_id = deal_example(verify_pos);
        if (pre_id >= 0) {
            LOG(INFO) << "Find a counterexample after " << try_num << "/" << verify_num;
            return {pre_id, verify_pos};
        }
    }

    for (int i = 0; i < aux_list.size(); ++i) {
        if (!inp_cache_list[i]) {
            DataList pre = new_inp_storage[i];
            for (int j = 0; j < pre.size(); ++j) {
                int pos = (verify_pos + j + 1) % verify_num;
                new_inp_storage[i][pos] = pre[j];
            }
#ifdef DEBUG
            for (int j = 0; j < 10 && j < verify_num; ++j) {
                auto truth = task->example_space->runAux(j, aux_list[i]);
                assert(truth == new_inp_storage[i][j]);
            }
#endif
        }
    }

    int pre_verify_num = verify_num;
    verify_num = verify_num * KExampleEnlargeFactor;
    verify_num = task->acquireExample(verify_num, KExampleTimeOut);

    for (int i = 0; i < aux_list.size(); ++i) {
        if (inp_cache_list[i]) {
            task->example_space->extendAuxCache(aux_list[i], inp_cache_list[i], verify_num);
        }
    }
    task->extendOupCache(verify_num);

    for (verify_pos = pre_verify_num; verify_pos < verify_num; ++verify_pos) {
        auto pre_id = deal_example(verify_pos);
        if (pre_id >= 0) return {pre_id, verify_pos};
    }

    for (int i = 0; i < aux_list.size(); ++i) {
        if (!inp_cache_list[i]) {
            LOG(INFO) << "RegisterAux " << aux2String(aux_list[i]) << " " << new_inp_storage[i].size();
            task->example_space->registerAuxCache(aux_list[i], new_inp_storage[i]);
        }
    }

    return {-1, -1};
}

std::vector<AuxProgram> IncrePLPSolver::extractResultFromInfo(solver::autolifter::EnumerateInfo *info) {
    if (!info) return {};
    std::vector<AuxProgram> res;

    for (auto id: info->ind_list) {
        auto& unit = component_info_list[id];
        res.push_back(unit.program);
    }
    return res;
}

namespace {
    std::string _indList2String(const std::vector<int>& ind_list) {
        std::string res = "[";
        for (int i = 0; i < ind_list.size(); ++i) {
            if (i) res += ","; res += std::to_string(ind_list[i]);
        }
        return res + "]";
    }
}

bool IncrePLPSolver::addUncoveredInfo(solver::autolifter::EnumerateInfo *info) {
    int pos = int(info->ind_list.size()) - 1;
    if (info->ind_list.size() > 1) {
        EnumerateInfo* last = nullptr;
        for (int i = 0; i < info->ind_list.size(); ++i) {
            std::vector<int> sub_ind_list;
            for (int j = 0; j < info->ind_list.size(); ++j) {
                if (i != j) sub_ind_list.push_back(info->ind_list[j]);
            }
            auto feature = _indList2String(sub_ind_list);
            auto it = uncovered_info_set.find(feature);
            if (it == uncovered_info_set.end()) return false;
            last = it->second;
        }
        int last_component = info->ind_list[pos];
        info->info = last->info | component_info_list[last_component].info;
    } else {
        info->info = component_info_list[info->ind_list[0]].info;
    }

    if (!maximal_list[pos].add(info)) return false;
    uncovered_info_set[_indList2String(info->ind_list)] = info;
    info_storage[pos].push_back(info);
    global_maximal.add(info);
    return true;
}

void IncrePLPSolver::constructInfo(solver::autolifter::EnumerateInfo *info) {
    int pos = info->ind_list.size();
    while (working_list.size() <= pos) working_list.emplace_back();
    for (auto* component_info: info_storage[0]) {
        if (component_info->ind_list[0] >= info->ind_list[0]) continue;
        std::vector<int> ind_list = component_info->ind_list;
        for (auto id: info->ind_list) ind_list.push_back(id);
        working_list[pos].push(new EnumerateInfo(ind_list));
    }
}

solver::autolifter::EnumerateInfo * IncrePLPSolver::getNextComponent(int k, TimeGuard *guard) {
    while (working_list.size() <= k) working_list.emplace_back();
    while (info_storage.size() <= k) info_storage.emplace_back();
    while (maximal_list.size() <= k) maximal_list.emplace_back();
    while (k && working_list[k].empty()) --k;
    if (k == 0) {
        while (1) {
            while (next_component_id < component_info_list.size() && component_info_list[next_component_id].is_error) ++next_component_id;
            if (next_component_id < component_info_list.size()) {
                return new EnumerateInfo({next_component_id++});
            } else getMoreComponent();
        }
    }
    auto* res = working_list[k].front(); working_list[k].pop();
    return res;
}

std::pair<solver::autolifter::EnumerateInfo *, solver::autolifter::EnumerateInfo *> IncrePLPSolver::recoverResult(int pos, solver::autolifter::EnumerateInfo *info) {
    for (auto* x: info_storage[pos]) {
        if ((x->info | info->info).count() == x->info.size()) return {x, info};
    }
    assert(false);
}
std::pair<solver::autolifter::EnumerateInfo *, solver::autolifter::EnumerateInfo *> IncrePLPSolver::constructResult(solver::autolifter::EnumerateInfo *info, int limit) {
    int rem = limit - int(info->ind_list.size());
    if (info->info.count() == example_list.size()) return {info, nullptr};
    if (!global_maximal.isExistResult(info)) return {nullptr, nullptr};
    for (int i = 0; i < rem && i < maximal_list.size(); ++i) {
        if (maximal_list[i].isExistResult(info)) {
            return recoverResult(i, info);
        }
    }
    return {nullptr, nullptr};
}

std::vector<AuxProgram> IncrePLPSolver::synthesisFromExample(TimeGuard* guard) {
    if (example_list.empty()) return {};
    std::vector<AuxProgram> best_result;
    int extra_turn_num = 0, current_limit = KComposedNum;

    for (int turn_id = 1;; ++turn_id) {
        TimeCheck(guard);
        if (!best_result.empty()) {
            ++extra_turn_num;
            if (extra_turn_num >= KExtraTurnNum || current_limit == 0) return best_result;
        }
        int k = (turn_id - 1) % ((current_limit + 1) / 2);
        auto* info = getNextComponent(k, guard);
        if (!addUncoveredInfo(info)) {
            delete info; continue;
        }
        auto res = constructResult(info, current_limit);
        if (res.first) {
            auto aux_list = extractResultFromInfo(res.first);
            for (const auto& aux: extractResultFromInfo(res.second)) {
                aux_list.push_back(aux);
            }
            best_result = aux_list;
            extra_turn_num = 0; current_limit = int(best_result.size()) - 1;
        }
        if (info->ind_list.size() < (current_limit + 1) / 2) constructInfo(info);
    }
}

namespace {
    PSemantics _getSemantics(Grammar* grammar, const std::string& name) {
        for (auto* symbol: grammar->symbol_list) {
            for (auto* rule: symbol->rule_list) {
                auto* cr = dynamic_cast<ConcreteRule*>(rule);
                if (cr->semantics->getName() == name) {
                    return cr->semantics;
                }
            }
        }
        LOG(FATAL) << "Operator " << name << " not found";
    }
}

PLPRes IncrePLPSolver::synthesis(TimeGuard *guard) {
    std::cout << std::endl << std::endl << std::endl;
    LOG(INFO) << "solve " << task->example_space->tau_id;
    if (task->target.second) LOG(INFO) << "  " << task->target.second->toString();
    /*for (int i = 0; i < task->aux_grammar_list.size(); ++i) {
        auto* grammar = task->aux_grammar_list[i];
        std::cout << "aux grammar " << i << std::endl;
        grammar->grammar->print();
        std::cout << std::endl;
    }
    std::cout << "compress grammar" << std::endl;
    task->compress_grammar->grammar->print();
    std::cout << std::endl;*/
    auto counter_example = verify(unfoldComponents({}));
    if (counter_example.first == -1) return {};
    LOG(INFO) << "Counter example " << example2String(counter_example);
    addExample(counter_example);

    while (true) {
        // global::recorder.add("#cegis", 1);
        TimeCheck(guard);
        auto candidate_result = unfoldComponents(synthesisFromExample(guard));
        LOG(INFO) << "Candidate result " << _unitList2String(candidate_result);
        LOG(INFO) << KComposedNum << std::endl;
        counter_example = verify(candidate_result);
        if (counter_example.first == -1) return candidate_result;
        addExample(counter_example);
        LOG(INFO) << "Counter example " << example2String(counter_example);
        if (counter_example.second != counter_example.first) {
            LOG(INFO) << task->runOup(counter_example.first).toString() << " " << task->runOup(counter_example.second).toString() << std::endl;
            for (auto unit: candidate_result) {
                LOG(INFO) << "  " << task->runInp(counter_example.first, unit).toString() << " " <<
                          task->runInp(counter_example.second, unit).toString() << std::endl;
            }
        } else {
            LOG(INFO) << "An error example";
        }
    }
}