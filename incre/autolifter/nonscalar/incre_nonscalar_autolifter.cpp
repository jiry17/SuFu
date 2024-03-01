//
// Created by pro on 2023/7/14.
//

#include "istool/incre/autolifter/incre_nonscalar_autolifter.h"
#include "glog/logging.h"
#include "istool/solver/enum/enum.h"
#include "istool/solver/enum/enum_util.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/solver/autolifter/basic/occam_verifier.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/basic/config.h"
#include <iostream>

using namespace incre;
using namespace incre::autolifter;

InputUnfoldInfo::InputUnfoldInfo(const std::unordered_set<std::string> &_ds_input, const DataList &_scalar_input,
                                 const std::string &_feature):
                                 ds_input(_ds_input), scalar_input(_scalar_input), structure_feature(_feature) {
}

namespace {
    bool _isPrimary(const Data& data) {
        return dynamic_cast<VInt*>(data.get()) || dynamic_cast<VBool*>(data.get()) || dynamic_cast<VUnit*>(data.get());
    }
    void _unfoldInput(const Data& data, int depth, InputUnfoldInfo& result) {
        if (_isPrimary(data)) {
            result.scalar_input.push_back(data); return;
        }
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto& sub: vt->elements) {
                _unfoldInput(sub, depth, result);
            }
            return;
        }
        auto* vi = dynamic_cast<VInductive*>(data.get());
        if (vi) {
            result.ds_input.insert(data.toString());
            if (depth) {
                result.structure_feature += "@" + vi->name;
                _unfoldInput(vi->content, depth - 1, result);
            }
            return;
        }
        LOG(FATAL) << "Unsupported input " << data.toString();
    }
}

InputUnfoldInfo autolifter::unfoldInput(const Data &data, int depth) {
    InputUnfoldInfo result;
    _unfoldInput(data, depth, result);
    return result;
}

namespace {
    bool _unfoldOutput(const Data& data, const InputUnfoldInfo& info, int depth, std::unordered_map<std::string, Data>& result, const std::string& path) {
        if (_isPrimary(data)) {
            result[path] = data; return true;
        }
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (int i = 0; i < vt->elements.size(); ++i) {
                if (!_unfoldOutput(vt->elements[i], info, depth, result, path + "@" + std::to_string(i))) {
                    return false;
                }
            }
            return true;
        }
        auto* vi = dynamic_cast<VInductive*>(data.get());
        if (vi) {
            auto feature = data.toString();
            if (info.ds_input.find(feature) != info.ds_input.end()) return true;
            if (!depth) return false;
            return _unfoldOutput(vi->content, info, depth - 1, result, path);
        }
        LOG(FATAL) << "Unsupported output data " << data.toString();
    }
}

bool autolifter::unfoldOutput(const Data &data, const InputUnfoldInfo &info, int depth_limit,
                              std::unordered_map<std::string, Data> &result) {
    result.clear();
    return _unfoldOutput(data, info, depth_limit, result, "");
}

NonScalarExecutionTool::NonScalarExecutionTool(IncreInfo *_info, Env* _env, int _KUnfoldDepth):
    info(_info), env(_env), KUnfoldDepth(_KUnfoldDepth) {
    pool = info->example_pool;
}
BasicNonScalarExecutionTool::BasicNonScalarExecutionTool(IncreInfo *_info, Env* _env, int _KUnfoldDepth):
    NonScalarExecutionTool(_info,  _env, _KUnfoldDepth) {
    local_inp_names.resize(info->align_infos.size());
    for (int i = 0; i < local_inp_names.size(); ++i) {
        auto& align_info = info->align_infos[i];
        for (auto& var: align_info->inp_types) local_inp_names[i].push_back(var.first);
    }
}

void BasicNonScalarExecutionTool::prepareGlobal(int align_id, int example_id) {
    std::unordered_map<std::string, Data> global_map;
    for (auto& [var_name, var_value]: pool->example_pool[align_id][example_id]->global_inputs) {
        global_map[var_name] = var_value;
    }
    pool->ctx->initGlobal(global_map);
}

namespace {
    void _printExample(FullPairExample example, IncreExamplePool* pool) {
        auto& example_x = pool->example_pool[example.align_id][example.x];
        auto& example_y = pool->example_pool[example.align_id][example.y];
        LOG(INFO) << "align id: " << example.align_id;
        LOG(INFO) << "example x: " << example_x->toString();
        LOG(INFO) << "example y: " << example_y->toString();
    }
}

bool NonScalarExecutionTool::isValid(FullPairExample &example, const TypedProgramList &program) {
    auto x_inp = runInp(example.align_id, example.x, program);
    std::unordered_map<std::string, Data> x_oup;
    auto x_flag = runOup(example.align_id, example.x, program, x_inp, x_oup);
    if (!x_flag) return false;
    if (example.x == example.y) return true;
    auto y_inp = runInp(example.align_id, example.y, program);
    std::unordered_map<std::string, Data> y_oup;
    auto y_flag = runOup(example.align_id, example.y, program, y_inp, y_oup);
    if (!y_flag) return false;
    /*LOG(INFO) << "is valid check info";
    LOG(INFO) << x_inp.structure_feature << " " << data::dataList2String(x_inp.scalar_input);
    for (auto& [path, v]: x_oup) LOG(INFO) << "  " << path << " " << v.toString();
    LOG(INFO) << y_inp.structure_feature << " " << data::dataList2String(y_inp.scalar_input);
    for (auto& [path, v]: y_oup) LOG(INFO) << "  " << path << " " << v.toString();*/
    if (x_inp.structure_feature != y_inp.structure_feature || data::dataList2String(x_inp.scalar_input) != data::dataList2String(y_inp.scalar_input)) {
        return true;
    }
    for (auto& [path, v]: x_oup) {
        auto it = y_oup.find(path);
        if (it != y_oup.end() && !(it->second == v)) return false;
    }
    return true;
}

namespace {
    bool _isBasicScalar(const Data& data) {
        return dynamic_cast<VInt*>(data.get()) || dynamic_cast<VBool*>(data.get()) || dynamic_cast<VUnit*>(data.get());
    }
}

Data autolifter::executeCompress(const Data &data, int compress_id, const PProgram &program, Env *env) {
    if (_isBasicScalar(data)) return data;
    auto* vc = dynamic_cast<VLabeledCompress*>(data.get());
    if (vc) {
        if (vc->id == compress_id) return env->run(program.get(), {vc->content});
        return Data(std::make_shared<VUnit>());
    }
    auto* vt = dynamic_cast<VTuple*>(data.get());
    if (vt) {
        DataList result;
        for (auto& element: vt->elements) result.push_back(executeCompress(element, compress_id, program, env));
        return BuildData(Product, result);
    }
    auto* vi = dynamic_cast<VInductive*>(data.get());
    if (vi) {
        auto content = executeCompress(vi->content, compress_id, program, env);
        return Data(std::make_shared<VInductive>(vi->name, content));
    }
    LOG(FATAL) << "Unknown data " << data.toString();
}

Data autolifter::executeCompress(const Data &data, const TypedProgramList &program_list, Env *env) {
    if (_isBasicScalar(data)) return data;
    auto* vc = dynamic_cast<VLabeledCompress*>(data.get());
    if (vc) {
        assert(program_list.size() > vc->id);
        return env->run(program_list[vc->id].second.get(), {vc->content});
    }
    auto* vt = dynamic_cast<VTuple*>(data.get());
    if (vt) {
        DataList result;
        for (auto& element: vt->elements) result.push_back(executeCompress(element, program_list, env));
        return BuildData(Product, result);
    }
    auto* vi = dynamic_cast<VInductive*>(data.get());
    if (vi) {
        auto content = executeCompress(vi->content, program_list, env);
        return Data(std::make_shared<VInductive>(vi->name, content));
    }
    LOG(FATAL) << "Unknown data " << data.toString();
}

void autolifter::mergeInputInfo(InputUnfoldInfo &base, const InputUnfoldInfo &extra) {
    for (auto& scalar_value: extra.scalar_input) base.scalar_input.push_back(scalar_value);
    base.structure_feature += "@" + extra.structure_feature;
    for (auto& ds_value: extra.ds_input) base.ds_input.insert(ds_value);
}

InputUnfoldInfo BasicNonScalarExecutionTool::runInp(int align_id, int example_id, const TypedProgramList &program) {
    auto& example = pool->example_pool[align_id][example_id];
    InputUnfoldInfo result;
    for (auto& local_var: local_inp_names[align_id]) {
        auto value = example->local_inputs[local_var];
        value = executeCompress(value, program, env);
        autolifter::mergeInputInfo(result, unfoldInput(value, KUnfoldDepth));
    }
    // TODO: consider global inputs
    return result;
}

bool BasicNonScalarExecutionTool::runOup(int align_id, int example_id, const TypedProgramList &program,
                                         const InputUnfoldInfo& inp_info, std::unordered_map<std::string, Data> &result) {
    auto& example = pool->example_pool[align_id][example_id];
    auto value = executeCompress(example->oup, program, env);
    return unfoldOutput(value, inp_info, KUnfoldDepth, result);
}

namespace {
    const int KDefaultVerifyBase = 1000;
    const int KDefaultExampleTimeOut = 10;
}

NonScalarAlignSolver::NonScalarAlignSolver(IncreInfo *_info): info(_info),
    compress_num(incre::getCompressTypeList(_info).size()), KVerifyNumList(_info->align_infos.size(), 0), verify_pos(0, 0) {
    env = info->example_pool->generator->env;
    auto d = env->getConstRef(solver::autolifter::KOccamExampleNumName, BuildData(Int, KDefaultVerifyBase));
    KVerifyBase = theory::clia::getIntValue(*d);
    KExampleTimeOut = KDefaultExampleTimeOut;
}

BasicNonScalarSolver::BasicNonScalarSolver(IncreInfo *_info,  NonScalarExecutionTool *_runner): NonScalarAlignSolver(_info), runner(_runner) {
    auto type_list = incre::getCompressTypeList(info);
    for (int i = 0; i < type_list.size(); ++i) {
        TypeList inp_list(1, incre::typeFromIncre(type_list[i]));
        PType oup_type = std::make_shared<TBot>();
        std::string name = std::to_string(i);
        auto* grammar = info->component_pool.buildAlignGrammar(inp_list, false);
        auto var_info = std::make_shared<SynthInfo>(name, inp_list, oup_type, grammar);
        cinfo_list.push_back(var_info);
    }
}

void BasicNonScalarSolver::addCounterExample(const FullPairExample &example) {
    example_list.push_back(example);
}


void NonScalarAlignSolver::acquireExamples(int target_num) {
    for (int i = 0; i < info->align_infos.size(); ++i) {
        auto* guard = new TimeGuard(KExampleTimeOut);
        LOG(INFO) << "target num " << target_num << " pre num " << info->example_pool->example_pool[i].size();
        info->example_pool->generateBatchedExample(i, target_num, guard);
        delete guard;
        LOG(INFO) << "found " << info->example_pool->example_pool[i].size();
        KVerifyNumList[i] = std::max(KVerifyNumList[i], std::min(int(info->example_pool->example_pool[i].size()), target_num));
    }
}

FullPairExample BasicNonScalarSolver::verify(const TypedProgramList &res) {
    std::unordered_map<std::string, std::pair<int, std::string>> scalar_map;

    auto verify = [&](int align_id, int example_id) {
        auto inp_info = runner->runInp(align_id, example_id, res);
        std::unordered_map<std::string, Data> oup_info;
        if (!runner->runOup(align_id, example_id, res, inp_info, oup_info)) return example_id;
        auto inp_feature = inp_info.structure_feature + "@" + data::dataList2String(inp_info.scalar_input);
        for (auto& [path, value]: oup_info) {
            std::string full_feature = inp_feature + "@" + path;
            auto it = scalar_map.find(full_feature);
            if (it == scalar_map.end()) {
                scalar_map[full_feature] = std::make_pair(example_id, value.toString());
            } else {
                if (!(it->second.second == value.toString())) return it->second.first;
            }
        }
        return -1;
    };

    int verify_num = 0;
    for (auto& prog: res) verify_num += prog.second->size();
    verify_num *= KVerifyBase; acquireExamples(verify_num);

    for (int _ = 0; _ < info->align_infos.size(); ++_) {
        int n = KVerifyNumList[verify_pos.first];
        scalar_map.clear();
        for (int __ = 0; __ < n; ++__) {
            auto verify_res = verify(verify_pos.first, verify_pos.second);
            if (verify_res != -1) return {verify_pos.first, verify_res, verify_pos.second};
            verify_pos.second++;
            if (verify_pos.second == n) verify_pos.second = 0;
        }
        verify_pos = {verify_pos.first + 1, 0};
        if (verify_pos.first == info->align_infos.size()) verify_pos = {0, 0};
    }
    return {-1, -1, -1};
}

namespace {
    TypedProgramList _functionContext2TypedProgram(const FunctionContext& ctx) {
        TypedProgramList program_list;
        for (int i = 0; i < ctx.size(); ++i) {
            auto name = std::to_string(i);
            auto it = ctx.find(name); assert(it != ctx.end());
            auto program = it->second;
            auto* sem = dynamic_cast<incre::grammar::TypeLabeledDirectSemantics*>(program->semantics.get());
            assert(sem);
            program_list.emplace_back(sem->type, program->sub_list[0]);
        }
        return program_list;
    }

    class _BasicFullAutoLifterVerifier: public Verifier {
    public:
        NonScalarExecutionTool* tool;
        std::vector<FullPairExample> example_list;
        _BasicFullAutoLifterVerifier(NonScalarExecutionTool* _tool, const std::vector<FullPairExample>& _example_list):
            tool(_tool), example_list(_example_list) {
        }
        virtual bool verify(const FunctionContext& info, Example* counter_example) {
            assert(!counter_example);
            auto program_list = _functionContext2TypedProgram(info);
            for (auto& example: example_list) {
                if (!tool->isValid(example, program_list)) return false;
            }
            return true;
        }
        ~_BasicFullAutoLifterVerifier() = default;
    };
}

TypedProgramList BasicNonScalarSolver::synthesisFromExample() {
    auto* verifier = new _BasicFullAutoLifterVerifier(runner, example_list);
    EnumConfig conf(verifier, new TrivialOptimizer());
    auto enum_res = solver::enumerate(cinfo_list, conf);
    if (enum_res.empty()) {
        LOG(FATAL) << "synthesis failed";
    }
    delete verifier;
    return _functionContext2TypedProgram(enum_res);
}

TypedProgramList NonScalarAlignSolver::solve() {
    PType empty_type = std::make_shared<TBot>();
    PProgram empty_program = program::buildConst(Data(std::make_shared<VUnit>()));
    TypedProgramList result(compress_num, {empty_type, empty_program});
    auto counter_example = verify(result);
    if (counter_example.align_id == -1) return result;
    addCounterExample(counter_example);

    while (1) {
        result = synthesisFromExample();
        LOG(INFO) << "Current result";
        for (auto& program: result) LOG(INFO) << "  " << program.second->toString();
        counter_example = verify(result);
        if (counter_example.align_id != -1) {
            addCounterExample(counter_example);
            //assert(!runner->isValid(counter_example, result));
            LOG(INFO) << "Example: " << counter_example.align_id << " " << counter_example.x << " " << counter_example.y;
            LOG(INFO) << info->example_pool->example_pool[counter_example.align_id][counter_example.x]->toString();
            if (counter_example.x != counter_example.y) {
                LOG(INFO) << info->example_pool->example_pool[counter_example.align_id][counter_example.y]->toString();
            }
        } else return result;
    }
}

namespace {
    const int KDefaultUnfoldDepth = 1;
}

IncreNonScalarSolver::IncreNonScalarSolver(IncreInfo *_info, const PEnv& _env, NonScalarAlignSolver* _aux_solver):
    IncreSolver(_info), env(_env), aux_solver(_aux_solver) {
    auto* d = env->getConstRef(autolifter::KUnfoldDepthName, BuildData(Int, KDefaultUnfoldDepth));
    KUnfoldDepth = theory::clia::getIntValue(*d);
    KExampleTimeOut = KDefaultExampleTimeOut;
    runner = new BasicNonScalarExecutionTool(info, env.get(), KUnfoldDepth);
}

const std::string autolifter::KUnfoldDepthName = "NonScalarIncre@UnfoldDepth";

IncreSolution IncreNonScalarSolver::solve() {

    global::printStageResult("Stage 1/2: synthesizing the representation function.");
    align_list = aux_solver->solve();
    LOG(INFO) << "align result";
    for (auto& align: align_list) LOG(INFO) << "  " << align.second->toString();

    global::printStageResult("Stage 2/2: synthesizing the combinator.");
    TermList comb_list;
    for (int i = 0; i < info->align_infos.size(); ++i) {
        global::printStageResult("  Synthesizing sketch hole " + std::to_string(i + 1) + "/" + std::to_string(info->align_infos.size()));
        comb_list.push_back(synthesisComb(i));
    }
    TyList compress_type_list;
    for (auto& [ty, prog]: align_list) compress_type_list.push_back(incre::typeToIncre(ty.get()));
    return {compress_type_list, comb_list, {}};
}