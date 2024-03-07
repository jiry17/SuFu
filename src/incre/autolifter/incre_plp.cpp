//
// Created by pro on 2022/9/26.
//

#include "istool/incre/autolifter/incre_plp.h"
#include "glog/logging.h"
#include "istool/solver/enum/enum_util.h"
#include "istool/incre/trans/incre_trans.h"
#include <cassert>

using namespace incre::autolifter;
using namespace incre::grammar;


std::string FExampleSpace::example2String(const IOExample &example) {
    std::string res = "{";
    for (int i = 0; i < value_list.size(); ++i) {
        if (i) res += ", ";
        res += value_list[i].first + ": " + example.first[i].toString();
    }
    res += "} -> " + example.second.toString();
    return res;
}
std::string FExampleSpace::example2String(int id) {
    return example_list[id].toString();
}
IncreIOExample::IncreIOExample(const DataList &_local, const DataList &_global, const Data &_oup):
    local_input(_local), global_input(_global), oup(_oup) {
    full_input = data::concatDataList(local_input, global_input);
}
DataList IncreIOExample::getAuxInput(const Data &compress) {
    DataList res(global_input.size() + 1); res[0] = compress;
    for (int i = 0; i < global_input.size(); ++i) {
        res[i + 1] = global_input[i];
    }
    return res;
}
std::string IncreIOExample::toString() const {
    return data::dataList2String(local_input) + "@" + data::dataList2String(global_input) + " -> " + oup.toString();
}
void FExampleSpace::addExample() {
    auto example = pool->example_pool[tau_id][example_list.size()];

    DataList local_input, global_input;
    for (auto& [var_name, _]: value_list) {
        local_input.push_back(example->local_inputs[var_name]);
    }
    for (auto& [var_name, _]: pool->input_list) {
        global_input.push_back(example->global_inputs[var_name]);
    }
    example_list.emplace_back(local_input, global_input, example->oup);
}
int FExampleSpace::acquireExample(int target_num, TimeGuard *guard) {
    pool->generateBatchedExample(tau_id, target_num, guard);
    target_num = std::min(target_num, int(pool->example_pool[tau_id].size()));
    while (example_list.size() < target_num) {
        addExample();
    }
    return target_num;
}
FExampleSpace::FExampleSpace(IncreExamplePool *_pool, int _tau_id, const PEnv& _env, AlignTypeInfoData* info):
        pool(_pool), tau_id(_tau_id), env(_env.get()), current_example_id(-1) {

    for (auto& [var_name, var_type]: info->inp_types) {
        value_list.emplace_back(var_name, incre::typeFromIncre(var_type));
    }
    for (auto& [var_name, var_type]: pool->input_list) {
        global_input_list.emplace_back(var_name, incre::typeFromIncre(var_type));
    }
}

void FExampleSpace::extendAuxCache(const AuxProgram &program, DataList *cache_item, int length) {
    assert(length <= example_list.size());
    for (int i = cache_item->size(); i < length; ++i) {
        cache_item->push_back(runAux(i, program));
    }
}
void FExampleSpace::extendOupCache(const PProgram &program, const std::vector<int> &path, DataList *cache_item,
                                   int length) {
    assert(length <= example_list.size());
    LOG(INFO) << "Extend from " << cache_item->size() << " to " << length;
    for (int i = cache_item->size(); i < length; ++i) {
        cache_item->push_back(runOup(i, program.get(), path));
    }
}

DataList *FExampleSpace::getAuxCache(const AuxProgram &program, int length) {
    auto feature = aux2String(program);
    if (aux_cache.find(feature) == aux_cache.end()) return nullptr;
    auto* cache_item = aux_cache[feature];
    extendAuxCache(program, cache_item, length);
    return cache_item;
}

namespace {
    std::string _path2String(const std::vector<int>& path) {
        std::string res = "[";
        for (int i = 0; i < path.size(); ++i) {
            if (i) res += ","; res += std::to_string(path[i]);
        }
        return res + "]";
    }

    std::string _getOupFeature(const PProgram& program, const std::vector<int>& path) {
        if (program) return program->toString() + "@" + _path2String(path);
        return _path2String(path);
    }
}

DataList* FExampleSpace::getOupCache(const PProgram &program, const std::vector<int> &path, int length) {
    auto feature = _getOupFeature(program, path);
    if (oup_cache.find(feature) == oup_cache.end()) return nullptr;
    auto* cache_item = oup_cache[feature];
    extendOupCache(program, path, cache_item, length);
    return cache_item;
}

void FExampleSpace::registerAuxCache(const AuxProgram &program, const DataList &oup_list) {
    auto feature = aux2String(program);
    assert(aux_cache.find(feature) == aux_cache.end());
    auto* cache_item = new DataList(oup_list);
    aux_cache[feature] = cache_item;
}
void FExampleSpace::registerOupCache(const PProgram &program, const std::vector<int> &path, const DataList& oup_list) {
    auto feature = _getOupFeature(program, path);
    assert(oup_cache.find(feature) == oup_cache.end());
    auto* cache_item = new DataList(oup_list);
    oup_cache[feature] = cache_item;
}

void FExampleSpace::switchTo(int example_id) {
    if (current_example_id == example_id) return;
    current_example_id = example_id;
    std::unordered_map<std::string, Data> global_map;
    for (int i = 0; i < global_input_list.size(); ++i) {
        global_map[global_input_list[i].first] = example_list[example_id].global_input[i];
    }
    pool->ctx->initGlobal(global_map);
}

namespace {
    Data _extract(const Data& d, const std::vector<int>& path) {
        Data res(d);
        for (auto pos: path) {
            auto* v = dynamic_cast<incre::VTuple*>(res.get());
            assert(v && v->elements.size() > pos);
            res = v->elements[pos];
        }
        auto* cv = dynamic_cast<incre::VCompress*>(res.get());
        if (cv) return cv->content;
        return res;
    }
}

#include "istool/basic/config.h"

Data FExampleSpace::runCompress(int example_id, Program *prog) {
    switchTo(example_id);
    global::recorder.start("execute");
    int pre_size = pool->ctx->holder->address_list.size();
    auto res = env->run(prog, example_list[example_id].full_input);
    pool->ctx->holder->recover(pre_size);
    global::recorder.end("execute");
    return res;
}
Data FExampleSpace::runAux(int example_id, const Data& content, Program *prog) {
    switchTo(example_id);
    global::recorder.start("execute");
    int pre_size = pool->ctx->holder->address_list.size();
    auto res = env->run(prog, example_list[example_id].getAuxInput(content));
    pool->ctx->holder->recover(pre_size);
    global::recorder.end("execute");
    return res;
}

Data FExampleSpace::runAux(int example_id, const AuxProgram &aux) {
    auto compress = runCompress(example_id, aux.first.second.get());
    Data res;
    if (aux.second.second) {
        auto* tv = dynamic_cast<VLabeledCompress*>(compress.get());
#ifdef DEBUG
        auto* mid_type = dynamic_cast<TLabeledCompress*>(aux.first.first.get());
        // LOG(INFO) << compress.toString() <<"  " << tv;
        // LOG(INFO) << aux2String(aux) << " " << compress.toString();
        assert(tv && mid_type && tv->id == mid_type->id);
#endif
        return runAux(example_id, tv->content, aux.second.second.get());
    }
    return compress;
}
Data FExampleSpace::runOup(int example_id, Program *program, const std::vector<int>& path) {
    switchTo(example_id);
    auto oup = _extract(example_list[example_id].oup, path);
    if (program) {
        return runAux(example_id, oup, program);
    }
    return oup;
}

PLPTask::PLPTask(FExampleSpace *_example_space, const std::vector<GrammarEnumerateTool *> &_aux_grammar_list,
                 const std::vector<TypedProgramList> &_pre_res, GrammarEnumerateTool *_compress_grammar, const TypedProgram &_target,
                 const std::vector<int> &_path, int _oup_compress_id): example_space(_example_space), aux_grammar_list(_aux_grammar_list),
                 pre_res_list(_pre_res), compress_grammar(_compress_grammar), target(_target), path(_path), oup_compress_id(_oup_compress_id) {
    oup_cache = example_space->getOupCache(target.second, path, 0);
    if (!oup_cache) {
        example_space->registerOupCache(target.second, path, {});
        oup_cache = example_space->getOupCache(target.second, path, 0);
    }
}

void PLPTask::extendOupCache(int length) {
    example_space->extendOupCache(target.second, path, oup_cache, length);
}

// TODO: add cache
Data PLPTask::runOup(int example_id) {
    if (oup_cache->size() <= example_id) extendOupCache(example_id + 1);
    return oup_cache->at(example_id);
    //return example_space->runOup(example_id, target.second.get(), path);
}
Data PLPTask::runInp(int example_id, const AuxProgram& program) {
    return example_space->runAux(example_id, program);
}

IOExample PLPTask::getIO(int example_id, const std::vector<AuxProgram> &aux_list) {
    DataList inp;
    for (auto& aux_program: aux_list) {
        inp.push_back(runInp(example_id, aux_program));
    }
    return {inp, runOup(example_id)};
}

int PLPTask::acquireExample(int target_num, int timeout) {
    auto* guard = new TimeGuard(timeout);
    auto res = example_space->acquireExample(target_num, guard);
    delete guard;
    return res;
}

#define ValueHead(name) auto* v = dynamic_cast<V ## name*>(data.get()); if (v)

Data incre::autolifter::eliminateCompress(const Data &data) {
    {ValueHead(Int) return data;}
    {ValueHead(Bool) return data;}
    {ValueHead(Compress) return v->content;}
    {
        ValueHead(Tuple) {
            DataList elements;
            for (int i = 0; i < v->elements.size(); ++i) {
                elements.push_back(eliminateCompress(v->elements[i]));
            }
            return Data(std::make_shared<VTuple>(elements));
        }
    }
    {
        ValueHead(Inductive) {
            return Data(std::make_shared<VInductive>(v->name, eliminateCompress(v->content)));
        }
    }
    LOG(FATAL) << "Unknown data " << data.toString();
}

Data incre::autolifter::openLabeledCompress(const Data &data, int label) {
    auto* v = dynamic_cast<VLabeledCompress*>(data.get());
    if (!v || v->id != label) LOG(FATAL) << "Unmatched compress id: get " << data.toString() << " but except " << label;
    return eliminateCompress(data);
}

std::string incre::autolifter::aux2String(const AuxProgram &program) {
    if (!program.second.first) {
        return program.first.first->getName() + "@" + program.first.second->toString();
    } else {
        return /*program.first.first->getName() + "@" +*/ program.first.second->toString() + " -> " + program.second.second->toString();
    }
}
namespace {
    TypedProgram _extractTypedProgram(const PProgram& program) {
        auto* ts = dynamic_cast<TypeLabeledDirectSemantics*>(program->semantics.get());
        assert(ts && ts->type);
        return {ts->type, program->sub_list[0]};
    }

    class _RuleBasedOptimizer: public Optimizer {
    public:
        static const std::unordered_set<std::string> KComOpSet, KAssocOpSet;
        virtual void clear() {
        }
        virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p) {
        }

    };
}

void GrammarEnumerateTool::extend() {
    int target_size = program_pool.size();
    auto dummy_info = std::make_shared<SynthInfo>("", TypeList(), PType(), grammar);
    std::vector<FunctionContext> collect_list;
    auto* op = new RuleBasedOptimizer();
    EnumConfig c(nullptr, op, nullptr);
    solver::collectAccordingSize({dummy_info}, target_size + 1, collect_list, c);
    delete op;
    TypedProgramList res_list;
    for (auto& res: collect_list) {
        auto p = _extractTypedProgram(res[""]);
        if (p.second->size() == target_size) {
            res_list.push_back(p);
        }
    }
    program_pool.emplace_back(res_list);
}
TypedProgramList* GrammarEnumerateTool::acquirePrograms(int target_size) {
    if (target_size > size_limit) return nullptr;
    while (target_size >= program_pool.size()) extend();
    return &program_pool[target_size];
}
GrammarEnumerateTool::~GrammarEnumerateTool() {
    delete grammar;
}