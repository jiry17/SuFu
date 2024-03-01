//
// Created by pro on 2023/7/26.
//

#include "istool/incre/autolifter/incre_nonscalar_oc.h"
#include "glog/logging.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include <iostream>

using namespace incre;
using namespace incre::autolifter;

namespace {
    std::unordered_set<int> _union(const std::unordered_set<int>& x, const std::unordered_set<int>& y) {
        std::unordered_set<int> res(x);
        for (auto& v: y) res.insert(v);
        return res;
    }

    IndexStorage _unionAll(const IndexStorage& x, const IndexStorage& y) {
        IndexStorage res(x.size());
        for (int i = 0; i < x.size(); ++i) res[i] = _union(x[i], y[i]);
        return res;
    }

    void _printIndexStorage(const IndexStorage& storage) {
        for (auto& index_list: storage) {
            std::cout << "["; bool flag = false;
            for (auto v: index_list) {
                if (flag) std::cout << ","; std::cout << v; flag = true;
            }
            std::cout << "]";
        }
        std::cout << std::endl;
    }

    void _printComponent(const ComponentInfo& info) {
        std::cout << "Component for #" << info.compress_id << ": " << (info.program ? info.program->toString() : "NULL") << std::endl;
        std::cout << "full: " << info.info->full.toString() << " covered: " << info.info->covered.toString() << std::endl;
        std::cout << "offer list: "; _printIndexStorage(info.info->offer_list);
        std::cout << "require list: "; _printIndexStorage(info.info->require_list);
    }

    void _printClosedInfo(const ClosedEnumerateInfo& info, const TypedProgramList& program_list) {
        std::cout << "full:" << info->full.toString() << " covered:" << info->covered.toString() << " is_start: " << info->is_include_start << std::endl;
        for (auto &[type, program]: program_list) {
            std::cout << program->toString() << std::endl;
        }
    }

    void _printDSInfo(const DSEnumerateInfo& info, const TypedProgramList& program_list) {
        std::cout << "full:" << info->full.toString() << " covered:" << info->covered.toString() << " " << "is_start: " << (info->index_list[0] == 0) << std::endl;
        for (auto &[type, program]: program_list) {
            std::cout << program->toString() << std::endl;
        }
    }
}

ClosedEnumerateInfo autolifter::mergeInfo(const ClosedEnumerateInfo &x, const ClosedEnumerateInfo &y) {
    std::vector<int> index_list;
    auto itx = x->index_list.begin(), ity = y->index_list.begin();
    while (itx < x->index_list.end() && ity < y->index_list.end()) {
        if (*itx == *ity) return nullptr;
        if (*itx < *ity) {
            index_list.push_back(*itx); ++itx;
        } else {
            index_list.push_back(*ity); ++ity;
        }
    }
    while (itx < x->index_list.end()) {
        index_list.push_back(*itx); ++itx;
    }
    while (ity < y->index_list.end()) {
        index_list.push_back(*ity); ++ity;
    }
    return std::make_shared<ClosedEnumerateInfoData>(x->full | y->full, x->covered | y->covered, x->is_include_start | y->is_include_start,
                                                     index_list, std::min(x->flag, y->flag));
}

DSEnumerateInfo autolifter::mergeDSInfo(const DSEnumerateInfo &x, const DSEnumerateInfo &y) {
    std::vector<int> index_list = x->index_list;
    for (auto index: y->index_list) index_list.push_back(index);
    std::sort(index_list.begin(), index_list.end());
    index_list.resize(std::unique(index_list.begin(), index_list.end()) - index_list.begin());

    int n = x->require_list.size();
    IndexStorage offer_list(n), require_list(n);
    for (int i = 0; i < n; ++i) {
        offer_list[i] = x->offer_list[i];
        for (auto v: y->offer_list[i]) offer_list[i].insert(v);
        for (auto v: x->require_list[i]) {
            if (offer_list[i].find(v) == offer_list[i].end()) require_list[i].insert(v);
        }
        for (auto v: y->require_list[i]) {
            if (offer_list[i].find(v) == offer_list[i].end()) require_list[i].insert(v);
        }
    }
    return std::make_shared<DSEnumerateInfoData>(x->full | y->full, x->covered | y->covered, index_list, offer_list, require_list);
}

namespace {
    const int KDefaultComposeNum = 3;
}

OCRunner::OCRunner(IncreInfo *_info, Env *_env, int _KInputUnfoldDepth, int _KOutputUnfoldDepth):
    info(_info), env(_env), KInputUnfoldDepth(_KInputUnfoldDepth), KOutputUnfoldDepth(_KOutputUnfoldDepth) {
    pool = info->example_pool;
    local_names.resize(info->align_infos.size());
    for (int i = 0; i < local_names.size(); ++i) {
        auto& align_info = info->align_infos[i];
        for (auto& var: align_info->inp_types) local_names[i].push_back(var.first);
    }
}

InputUnfoldInfo OCRunner::runInp(int align_id, int example_id, const TypedProgramList &program_list) {
    InputUnfoldInfo result;
    auto& example = pool->example_pool[align_id][example_id];
    for (auto& inp_name: local_names[align_id]) {
        auto full_inp = autolifter::executeCompress(example->local_inputs[inp_name], program_list, env);
        autolifter::mergeInputInfo(result, autolifter::unfoldInput(full_inp, KInputUnfoldDepth));
    }
    return result;
}

namespace {
    bool _isScalar(const Data& data) {
        return dynamic_cast<VInt*>(data.get()) || dynamic_cast<VBool*>(data.get()) || dynamic_cast<VUnit*>(data.get());
    }

    bool _isInvolveCompress(const Data& data, int cared_id=-1) {
        if (_isScalar(data)) return false;
        auto* vc = dynamic_cast<VLabeledCompress*>(data.get());
        if (vc) return cared_id == -1 || vc->id == cared_id;
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto& element: vt->elements) if (_isInvolveCompress(element)) return true;
            return false;
        }
        auto* vi = dynamic_cast<VInductive*>(data.get());
        assert(vi);
        return _isInvolveCompress(vi->content);
    }

    void _unfoldOriginalInp(const Data& data, int depth, InputUnfoldInfo& result) {
        if (_isScalar(data)) {
            result.scalar_input.push_back(data); return;
        }
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto& element: vt->elements) _unfoldOriginalInp(element, depth, result);
            return;
        }
        if (dynamic_cast<VLabeledCompress*>(data.get())) return;
        auto* vi = dynamic_cast<VInductive*>(data.get()); assert(vi);
        if (!_isInvolveCompress(data)) result.ds_input.insert(data.toString());
        if (depth == 0) return;
        result.structure_feature += "@" + vi->name;
        _unfoldOriginalInp(vi->content, depth - 1, result); return;
    }
}

InputUnfoldInfo OCRunner::runOriginalInp(int align_id, int example_id) {
    InputUnfoldInfo result;
    auto& example = pool->example_pool[align_id][example_id];
    for (auto& inp_name: local_names[align_id]) {
        _unfoldOriginalInp(example->local_inputs[inp_name], KInputUnfoldDepth, result);
    }
    return result;
}

namespace {
    void _unfoldNewResult(const Data& data, int depth, int compress_id, const PProgram& program, Env* env, InputUnfoldInfo& result, bool is_new) {
        if (_isScalar(data)) {
            if (is_new) result.scalar_input.push_back(data);
            return;
        }
        auto *vc = dynamic_cast<VLabeledCompress *>(data.get());
        if (vc) {
            if (vc->id != compress_id) return;
            auto now = autolifter::executeCompress(data, compress_id, program, env);
            _unfoldNewResult(now, depth, compress_id, program, env, result, true);
            return;
        }
        auto *vt = dynamic_cast<VTuple *>(data.get());
        if (vt) {
            for (auto &element: vt->elements) {
                _unfoldNewResult(element, depth, compress_id, program, env, result, is_new);
            }
            return;
        }
        auto *vi = dynamic_cast<VInductive *>(data.get());
        assert(vi);
        if (is_new) result.ds_input.insert(data.toString());
        else if (_isInvolveCompress(data, compress_id)) {
            auto now = autolifter::executeCompress(data, compress_id, program, env);
            result.ds_input.insert(now.toString());
        }
        if (depth == 0) return;
        result.structure_feature += "@" + vi->name;
        _unfoldNewResult(vi->content, depth - 1, compress_id, program, env, result, is_new);
    }
}

InputUnfoldInfo OCRunner::runNewInp(int align_id, int example_id, int compress_id, const PProgram &program) {
    InputUnfoldInfo result;
    auto& example = pool->example_pool[align_id][example_id];
    for (auto& inp_name: local_names[align_id]) {
        _unfoldNewResult(example->local_inputs[inp_name], KInputUnfoldDepth, compress_id, program, env, result, false);
    }
    return result;
}

namespace {
    void _unfoldOutput(const Data& data, int depth, OutputUnfoldInfo& result) {
        if (_isScalar(data)) {
            result.first += "@" + data.toString(); return;
        }
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto& element: vt->elements) {
                _unfoldOutput(element, depth, result);
            }
            return;
        }
        auto* vi = dynamic_cast<VInductive*>(data.get());
        if (vi) {
            if (depth == 0) {
                result.second.insert(data.toString());
                return;
            }
            result.first += "@" + vi->name;
            _unfoldOutput(vi->content, depth - 1, result);
            return;
        }
        LOG(FATAL) << "Unknown data " << data.toString();
    }
}

OutputUnfoldInfo autolifter::unfoldOutput(const Data &data, int depth) {
    OutputUnfoldInfo result;
    _unfoldOutput(data, depth, result);
    return result;
}

OutputUnfoldInfo OCRunner::runOup(int align_id, int example_id, const TypedProgramList &program_list) {
    auto& example = pool->example_pool[align_id][example_id];
    auto full_oup = autolifter::executeCompress(example->oup, program_list, env);
    return autolifter::unfoldOutput(full_oup, KOutputUnfoldDepth);
}

namespace {
    void _unfoldOriginalOup(const Data& data, int depth, OutputUnfoldInfo& result) {
        if (_isScalar(data)) {
            result.first += "@" + data.toString(); return;
        }
        if (dynamic_cast<VCompress*>(data.get())) return;
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto &element: vt->elements) {
                _unfoldOriginalOup(element, depth, result);
            }
            return;
        }
        auto* vi = dynamic_cast<VInductive*>(data.get());
        assert(vi);
        if (depth == 0) {
            if (!_isInvolveCompress(data)) result.second.insert(data.toString());
            return;
        }
        result.first += "@" + vi->name;
        _unfoldOriginalOup(vi->content, depth - 1, result);
    }
}

OutputUnfoldInfo OCRunner::runOriginalOup(int align_id, int example_id) {
    auto& example = pool->example_pool[align_id][example_id];
    OutputUnfoldInfo result;
    _unfoldOriginalOup(example->oup, KOutputUnfoldDepth, result);
    return result;
}

namespace {
    void _unfoldNewOup(const Data& data, int depth, int compress_id, const PProgram& program, Env* env, OutputUnfoldInfo& result, bool is_new) {
        if (_isScalar(data)) {
            if (is_new) result.first += "@" + data.toString(); return;
        }
        auto* tc = dynamic_cast<VLabeledCompress*>(data.get());
        if (tc) {
            if (tc->id == compress_id) {
                auto now = autolifter::executeCompress(data, compress_id, program, env);
                _unfoldNewOup(now, depth, compress_id, program, env, result, true);
            }
            return;
        }
        auto* tt = dynamic_cast<VTuple*>(data.get());
        if (tt) {
            for (auto& element: tt->elements) {
                _unfoldNewOup(element, depth, compress_id, program, env, result, is_new);
            }
            return;
        }
        auto* ti = dynamic_cast<VInductive*>(data.get());
        assert(ti);
        if (depth == 0) {
            if (is_new) result.second.insert(data.toString());
            else if (_isInvolveCompress(data, compress_id)) {
                auto now = autolifter::executeCompress(data, compress_id, program, env);
                result.second.insert(now.toString());
            }
            return;
        }
        result.first += "@" + ti->name;
        _unfoldNewOup(ti->content, depth - 1, compress_id, program, env, result, is_new);
    }
}

OutputUnfoldInfo OCRunner::runNewOup(int align_id, int example_id, int compress_id, const PProgram &program) {
    auto& example = pool->example_pool[align_id][example_id];
    OutputUnfoldInfo result;
    _unfoldNewOup(example->oup, KOutputUnfoldDepth, compress_id, program, env, result, false);
    return result;
}

OutputUnfoldInfo OCRunner::runOup(int align_id, int example_id, int compress_id, const PProgram &program) {
    auto& example = pool->example_pool[align_id][example_id];
    auto full_oup = autolifter::executeCompress(example->oup, compress_id, program, env);
    return autolifter::unfoldOutput(full_oup, KOutputUnfoldDepth);
}

ComponentInfo::ComponentInfo(int _compress_id, const PType& _type, const PProgram &_program, const DSEnumerateInfo &_info):
    compress_id(_compress_id), type(_type), program(_program), info(_info) {
}

ClosedEnumerateInfoData::ClosedEnumerateInfoData(const Bitset &_full, const Bitset &_covered, bool _is_include_start,
                                                 const std::vector<int> &_index_list, int _flag):
                                                 full(_full), covered(_covered), is_include_start(_is_include_start),
                                                 index_list(_index_list), flag(_flag) {
}
DSEnumerateInfoData::DSEnumerateInfoData(const Bitset &_full, const Bitset &_covered,
                                         const std::vector<int> &_index_list, const IndexStorage &_offer_list,
                                         const IndexStorage &_require_list):
                                         full(_full), covered(_covered), index_list(_index_list), offer_list(_offer_list),
                                         require_list(_require_list) {
}

int ClosedEnumerateInfoGenerator::getIndex(int example_id, const std::string &name) {
    while (index_manager.size() <= example_id) index_manager.emplace_back();
    auto it = index_manager[example_id].find(name);
    if (it == index_manager[example_id].end()) {
        int index = index_manager[example_id].size();
        return index_manager[example_id][name] = index;
    }
    return it->second;
}

void ClosedEnumerateInfoGenerator::updateComponentWithScalarExample(const ComponentInfo &info, int example_id) {
    auto& example = scalar_example_list[example_id];
    InputUnfoldInfo inp_x, inp_y; OutputUnfoldInfo oup_x, oup_y;
    if (!info.program) {
        inp_x = runner->runOriginalInp(example.align_id, example.x);
        inp_y = runner->runOriginalInp(example.align_id, example.y);
        oup_x = runner->runOriginalOup(example.align_id, example.x);
        oup_y = runner->runOriginalOup(example.align_id, example.y);
    } else {
        inp_x = runner->runNewInp(example.align_id, example.x, info.compress_id, info.program);
        oup_x = runner->runNewOup(example.align_id, example.x, info.compress_id, info.program);
        inp_y = runner->runNewInp(example.align_id, example.y, info.compress_id, info.program);
        oup_y = runner->runNewOup(example.align_id, example.y, info.compress_id, info.program);
    }
    bool is_full = oup_x.first != oup_y.first, is_covered = !(inp_x.scalar_input == inp_y.scalar_input) || inp_x.structure_feature != inp_y.structure_feature;
    info.info->full.append(is_full); info.info->covered.append(is_covered);
    if (info.program && info.program->toString() == "al_scanr(al_max(),Param0)") {
        LOG(INFO) << "cared component";
        _printComponent(info);
    }
}

void ClosedEnumerateInfoGenerator::updateComponentWithDSExample(const ComponentInfo &info, int example_id) {
    auto& example = ds_example_list[example_id];
    auto original_inp = runner->runOriginalInp(example.align_id, example.x);
    auto original_oup = runner->runOriginalOup(example.align_id, example.x);
    std::unordered_set<int> original_offer;
    for (auto& feature: original_inp.ds_input) original_offer.insert(getIndex(example_id, feature));
    std::unordered_set<int> original_require;
    for (auto& feature: original_oup.second) original_require.insert(getIndex(example_id, feature));
    if (!info.program) {
        info.info->require_list.push_back(original_require);
        info.info->offer_list.push_back(original_offer);
    } else {
        auto new_inp = runner->runNewInp(example.align_id, example.x, info.compress_id, info.program);
        auto new_oup = runner->runNewOup(example.align_id, example.x, info.compress_id, info.program);
        std::unordered_set<int> new_offer, new_require;
        for (auto& feature: new_inp.ds_input) {
            auto index = getIndex(example_id, feature);
            if (original_offer.find(index) == original_offer.end()) new_offer.insert(index);
        }
        for (auto& feature: new_oup.second) {
            auto index = getIndex(example_id, feature);
            if (original_offer.find(index) == original_offer.end() && original_require.find(index) == original_require.end() &&
                new_offer.find(index) == new_offer.end()) {
                new_require.insert(index);
            }
        }
        info.info->require_list.push_back(new_require);
        info.info->offer_list.push_back(new_offer);
    }
    if (info.program && info.program->toString() == "al_scanr(al_max(),Param0)") {
        LOG(INFO) << "cared component";
        _printComponent(info);
    }
}

void ClosedEnumerateInfoGenerator::insertComponent(int compress_id, const TypedProgram &program) {
    int index = component_info_list.size();
    auto enumerate_info = std::make_shared<DSEnumerateInfoData>(Bitset(), Bitset(), (std::vector<int>){index}, IndexStorage(), IndexStorage());
    ComponentInfo info(compress_id, program.first, program.second, enumerate_info);
    for (int i = 0; i < ds_example_list.size(); ++i) updateComponentWithDSExample(info, i);
    for (int i = 0; i < scalar_example_list.size(); ++i) updateComponentWithScalarExample(info, i);
    component_info_list.push_back(info);
}

void ClosedEnumerateInfoGenerator::addCounterExample(const FullPairExample &example) {
    closed_index = 0;
    if (example.x == example.y) {
        closed_list.clear(); component_index = 0; state_require_map.clear();
        component_offer_map.clear(); result_cache.clear();
        for (auto& info_list: close_merge_storage) info_list.clear();
        int example_id = ds_example_list.size(); ds_example_list.push_back(example);
        for (auto& info: component_info_list) updateComponentWithDSExample(info, example_id);
    } else {
        int example_id = scalar_example_list.size(); scalar_example_list.push_back(example);
        for (auto& info: component_info_list) updateComponentWithScalarExample(info, example_id);

        auto update = [&](const DSEnumerateInfo& info) {
            if (info->full.size() > example_id) return;
            bool is_full = false, is_cover = false;
            for (auto index: info->index_list) {
                auto& component_info = component_info_list[index].info;
                is_full |= component_info->full[example_id];
                is_cover |= component_info->covered[example_id];
            }
            info->full.append(is_full);
            info->covered.append(is_cover);
        };

        for (auto& [_, info_list]: state_require_map) {
            for (auto& info: info_list) update(info);
        }
        for (auto& info: closed_list) {
            update(info);
            auto current = index2ProgramList(info->index_list);
            /*if (current[0].second->toString() == "al_rev(Param0)") {
                LOG(INFO) << "update rev " << info->full.toString() << " " << info->covered.toString();
            }*/
        }
    }
}

namespace {
    std::vector<std::pair<int, TypedProgram>> _merge(const std::vector<std::pair<int, TypedProgram>>& x_list,
                                                     const TypedProgramList& y_list, int compress_id, Env* env) {
        std::vector<int> id(x_list.size() + y_list.size(), 0);
        for (int i = 0; i < y_list.size(); ++i) id[i] = 1;
        std::shuffle(id.begin(), id.end(), env->random_engine);
        int x_id = 0, y_id = 0;
        std::vector<std::pair<int, TypedProgram>> result(id.size());
        for (int i = 0; i < result.size(); ++i) {
            if (id[i]) result[i] = {compress_id, y_list[y_id++]};
            else result[i] = x_list[x_id++];
        }
        return result;
    }
}

void ClosedEnumerateInfoGenerator::acquireMoreComponent() {
    enumerate_size++;
    std::vector<std::pair<int, TypedProgram>> program_list;
    for (int i = 0; i < enumerate_tool_list.size(); ++i) {
        auto* now = enumerate_tool_list[i]->acquirePrograms(enumerate_size);
        program_list = _merge(program_list, *now, i, runner->env);
    }
    for (auto& [id, program]: program_list) insertComponent(id, program);
}

#include <queue>

bool ClosedEnumerateInfoGenerator::isValidInsert(const DSEnumerateInfo &base, const DSEnumerateInfo &extra) {
    int rem_size = KComposeNum - base->index_list.size(); assert(rem_size);
    int remain_num = 0, covered_num = 0;
    for (int i = 0; i < base->require_list.size(); ++i) {
        for (auto index: base->require_list[i]) {
            remain_num++;
            if (extra->offer_list[i].find(index) != extra->offer_list[i].end()) covered_num++;
        }
    }
    return covered_num * rem_size >= remain_num;
}

bool ClosedEnumerateInfoGenerator::isPrunedOff(const DSEnumerateInfo &info) {
    int n = info->index_list.size(); int covered_state = 0;
    for (int S = 1; S < (1 << n); ++S) {
        if ((S & (covered_state)) == S) continue;
        std::string feature;
        for (int i = 0; i < n; ++i) if (S & (1 << i)) {
            feature += "@" + std::to_string(info->index_list[i]);
        }
        if (result_cache.find(feature) != result_cache.end()) covered_state |= S;
    }
    return covered_state == (1 << n) - 1;
}

bool ClosedEnumerateInfoGenerator::isValidGroup(const DSEnumerateInfo &info) {
    int n = info->index_list.size(); int covered_state = 0;
    for (int S = 1; S < (1 << n); ++S) {
        if (S & 1) {
            std::string feature_x, feature_y;
            for (int i = 0; i < n; i++) {
                if (S & (1 << i)) {
                    feature_x += "@" + std::to_string(info->index_list[i]);
                } else {
                    feature_y += "@" + std::to_string(info->index_list[i]);
                }
            }
            if (result_cache.find(feature_x) != result_cache.end() && result_cache.find(feature_y) != result_cache.end()) {
                return false;
            }
        }
    }
    return true;
}

namespace {
    std::string _indexList2String(const std::vector<int> &index_list) {
        std::string feature;
        for (auto v: index_list) feature += "@" + std::to_string(v);
        return feature;
    }
}

std::vector<DSEnumerateInfo> ClosedEnumerateInfoGenerator::getOffer(const DSEnumerateInfo &info) {
    // TODO: optimize, record count in each enumerate info, and check isValidInsert after full enumeration
    std::vector<DSEnumerateInfo> result;
    std::unordered_set<int> cache;
    for (int example_id = 0; example_id < info->require_list.size(); ++example_id) {
        for (auto v: info->require_list[example_id]) {
            auto feature = std::to_string(example_id) + "@" + std::to_string(v);
            for (auto& component_info: component_offer_map[feature]) {
                if (cache.find(component_info->index_list[0]) != cache.end()) continue;
                cache.insert(component_info->index_list[0]);
                if (isValidInsert(info, component_info)) {
                    result.push_back(mergeDSInfo(info, component_info));
                }
            }
        }
    }
    return result;
}
std::vector<DSEnumerateInfo> ClosedEnumerateInfoGenerator::getRequire(const DSEnumerateInfo &info) {
    std::vector<DSEnumerateInfo> result;
    std::unordered_set<std::string> cache;
    for (int example_id = 0; example_id < info->offer_list.size(); ++example_id) {
        for (auto v: info->offer_list[example_id]) {
            auto feature = std::to_string(example_id) + "@" + std::to_string(v);
            for (auto& state_info: state_require_map[feature]) {
                auto state_feature = _indexList2String(state_info->index_list);
                if (cache.find(state_feature) != cache.end()) continue;
                cache.insert(state_feature);
                if (isValidInsert(state_info, info)) {
                    result.push_back(mergeDSInfo(state_info, info));
                }
            }
        }
    }
    return result;
}

namespace {
    bool _isFinishState(const DSEnumerateInfo& info) {
        for (auto& require: info->require_list) if (!require.empty()) return false;
        return true;
    }
    bool _isScalarType(const PType& type) {
        return dynamic_cast<TInt*>(type.get()) || dynamic_cast<TBool*>(type.get()) || dynamic_cast<TBot*>(type.get());
    }
    ClosedEnumerateInfo _dsInfo2ClosedInfo(const DSEnumerateInfo& info) {
        if (info->index_list[0] == 0) {
            std::vector<int> final_index_list;
            for (int i = 1; i < info->index_list.size(); ++i) {
                final_index_list.push_back(info->index_list[i]);
            }
            return std::make_shared<ClosedEnumerateInfoData>(info->full, info->covered, true, final_index_list);
        }
        return std::make_shared<ClosedEnumerateInfoData>(info->full, info->covered, false, info->index_list);
    }
}

void ClosedEnumerateInfoGenerator::insertClosedInfo(const DSEnumerateInfo &info) {
    while (close_merge_storage.size() < component_info_list.size()) close_merge_storage.emplace_back();
    /*LOG(INFO) << "Try extend info" << std::endl;
    auto current_result = index2ProgramList(info->index_list);
    _printDSInfo(info, current_result);
    if (current_result[0].second->toString() == "(Param0,al_rev(al_rev(Param0)))") {
        for (auto index: info->index_list) _printComponent(component_info_list[index]);
        int kk; std::cin >> kk;
    }*/
    std::vector<DSEnumerateInfo> new_info_list;
    new_info_list.push_back(info); result_cache.insert(_indexList2String(info->index_list));
    for (auto index: info->index_list) {
        for (auto& pre_info: close_merge_storage[index]) {
            auto merge_info = autolifter::mergeDSInfo(info, pre_info);
            if (merge_info->index_list.size() > KComposeNum) continue;
            auto feature = _indexList2String(merge_info->index_list);
            if (result_cache.find(feature) == result_cache.end()) {
                result_cache.insert(feature); new_info_list.push_back(merge_info);
            }
        }
    }
    for (auto& new_info: new_info_list) {
        if (isValidGroup(new_info)) {
            closed_list.push_back(new_info);
            //LOG(INFO) << "extend result";
            //_printDSInfo(new_info, index2ProgramList(new_info->index_list));
            for (auto index: new_info->index_list) {
                close_merge_storage[index].push_back(new_info);
            }
        }
    }
}

TypedProgramList ClosedEnumerateInfoGenerator::index2ProgramList(const std::vector<int> &index_list) {
    TypedProgramList result;
    for (int compress_id = 0; compress_id < enumerate_tool_list.size(); ++compress_id) {
        TypeList type_list; ProgramList component_list;
        for (auto index: index_list) {
            auto& component_info = component_info_list[index];
            if (component_info.compress_id == compress_id) {
                type_list.push_back(component_info.type);
                component_list.push_back(component_info.program);
            }
        }
        if (type_list.empty()) {
            PType full_type = std::make_shared<TBot>();
            PProgram full_program = program::buildConst(Data(std::make_shared<VUnit>()));
            result.emplace_back(full_type, full_program);
        } else if (type_list.size() == 1) {
            result.emplace_back(type_list[0], component_list[0]);
        } else {
            PType full_type = std::make_shared<TProduct>(type_list);
            PProgram full_program = std::make_shared<Program>(std::make_shared<ProductSemantics>(), component_list);
            result.emplace_back(full_type, full_program);
        }
    }
    return result;
}

void ClosedEnumerateInfoGenerator::addNextComponent() {
    while (component_info_list.size() == component_index) acquireMoreComponent();

    auto& base = component_info_list[component_index++];
    if (_isScalarType(base.type)) {
        result_cache.insert(_indexList2String(base.info->index_list));
        closed_list.push_back(base.info);
        return;
    }

    //LOG(INFO) << "Consider component";
    //_printComponent(base);

    std::vector<std::vector<DSEnumerateInfo>> info_queue(KComposeNum + 1);
    std::unordered_set<std::string> cache;

    auto insert = [&](const DSEnumerateInfo& info) {
        auto feature = _indexList2String(info->index_list);
        if (info->index_list.size() == KComposeNum && !_isFinishState(info)) return;
        if (isPrunedOff(info) || cache.find(feature) != cache.end()) return;
        cache.insert(feature);
        info_queue[info->index_list.size()].push_back(info);
        if (info->index_list.size() == KComposeNum) return;
        for (int example_id = 0; example_id < info->require_list.size(); ++example_id) {
            for (auto v: info->require_list[example_id]) {
                auto feature = std::to_string(example_id) + "@" + std::to_string(v);
                state_require_map[feature].push_back(info);
            }
        }
    };

    insert(base.info);
    for (auto& merge_info: getRequire(base.info)) insert(merge_info);
    for (int component_num = 1; component_num <= KComposeNum; ++component_num) {
        for (int i = 0; i < info_queue[component_num].size(); ++i) {
            auto current_info = info_queue[component_num][i];
            //LOG(INFO) << "consider info ";
            //_printDSInfo(current_info, index2ProgramList(current_info->index_list));
            if (_isFinishState(current_info)) {
                insertClosedInfo(current_info); continue;
            }
            for (auto& next_info: getOffer(current_info)) insert(next_info);
        }
    }

    for (int example_id = 0; example_id < base.info->offer_list.size(); ++example_id) {
        for (auto v: base.info->offer_list[example_id]) {
            auto feature = std::to_string(example_id) + "@" + std::to_string(v);
            component_offer_map[feature].push_back(base.info);
        }
    }
}

ClosedEnumerateInfo ClosedEnumerateInfoGenerator::getNextEnumerateInfo() {
    while (closed_list.size() == closed_index) addNextComponent();
    return _dsInfo2ClosedInfo(closed_list[closed_index++]);
}

#include "istool/incre/trans/incre_trans.h"

ClosedEnumerateInfoGenerator::ClosedEnumerateInfoGenerator(IncreInfo *info, OCRunner *_runner): runner(_runner) {
    auto type_list = incre::getCompressTypeList(info);
    for (int i = 0; i < type_list.size(); ++i) {
        TypeList inp_list(1, incre::typeFromIncre(type_list[i]));
        auto* grammar = info->component_pool.buildAlignGrammar(inp_list, false);
        enumerate_tool_list.push_back(new GrammarEnumerateTool(grammar));
    }

    auto* d = runner->env->getConstRef(solver::autolifter::KComposedNumName, BuildData(Int, KDefaultComposeNum));
    KComposeNum = theory::clia::getIntValue(*d);

    insertComponent(-1, {});
}

FullPairExample OCNonScalarAlignSolver::verify(const TypedProgramList &res) {
    std::unordered_map<std::string, std::pair<int, std::string>> scalar_map;

    auto verify = [&](int align_id, int example_id) {
        auto inp_info = runner->runInp(align_id, example_id, res);
        auto oup_info = runner->runOup(align_id, example_id, res);
        for (auto& v: oup_info.second) {
            if (inp_info.ds_input.find(v) == inp_info.ds_input.end()) return example_id;
        }
        auto inp_feature = inp_info.structure_feature + "@" + data::dataList2String(inp_info.scalar_input);
        auto it = scalar_map.find(inp_feature);
        if (it == scalar_map.end()) {
            scalar_map[inp_feature] = std::make_pair(example_id, oup_info.first);
        } else if (it->second.second != oup_info.first) return it->second.first;
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


bool autolifter::isCovered(const ClosedEnumerateInfo &x, const ClosedEnumerateInfo &y) {
    if (!x->is_include_start && y->is_include_start) return false;
    return y->full.checkCover(x->full) && x->covered.checkCover(y->covered);
}

bool autolifter::isFull(const ClosedEnumerateInfo &x, const ClosedEnumerateInfo &y) {
    if (!x->is_include_start && !y->is_include_start) return false;
    int n = x->full.getASize();
    for (int i = 0; i < n; ++i) {
        auto full = x->full.accessA(i) | y->full.accessA(i);
        auto covered = x->covered.accessA(i) | y->covered.accessA(i);
        if ((full & covered) != full) return false;
    }
    return true;
}

bool EnumerateInfoMaximumList::isCovered(const ClosedEnumerateInfo &info) const {
    for (auto& existing_info: info_list) {
        if (autolifter::isCovered(existing_info, info)) return true;
    }
    return false;
}

ClosedEnumerateInfo EnumerateInfoMaximumList::merge(const ClosedEnumerateInfo &info) const {
    for (auto& existing_info: info_list) {
        if (autolifter::isFull(existing_info, info)) return existing_info;
    }
    return info;
}

void EnumerateInfoMaximumList::insert(const ClosedEnumerateInfo &info) {
    int now = 0;
    for (auto& existing_info: info_list) {
        if (!autolifter::isCovered(info, existing_info)) {
            info_list[now++] = existing_info;
        }
    }
    info_list.resize(now); info_list.push_back(info);
}

OCNonScalarAlignSolver::OCNonScalarAlignSolver(IncreInfo *_info, OCRunner *_runner):
        NonScalarAlignSolver(_info), runner(_runner), generator(_info, _runner) {
    KComposeNum = generator.KComposeNum; working_storage.resize(KComposeNum + 1);
    size_grouped_list.resize(KComposeNum + 1); progress_list.resize(KComposeNum + 1);
    for (int i = 0; i <= KComposeNum; ++i) progress_list[i] = {0, 0};
}

void OCNonScalarAlignSolver::addCounterExample(const FullPairExample &example) {
    for (auto& list: size_grouped_list) list.info_list.clear();
    generator.addCounterExample(example);
    for (int i = 0; i <= KComposeNum; ++i)  working_storage[i].clear(), progress_list[i] = {0, 0};
    considered_set.clear(); group_list.clear();
}

ClosedEnumerateInfo OCNonScalarAlignSolver::tryMerge(const ClosedEnumerateInfo &current, int remain_size) {
    if (!size_grouped_list[remain_size].merge(current)) return nullptr;
    for (auto& remain_info: working_storage[remain_size]) {
        if (isFull(remain_info, current)) {
            //LOG(INFO) << "valid merge";
            //_printClosedInfo(current, generator.index2ProgramList(current->index_list));
            //_printClosedInfo(remain_info, generator.index2ProgramList(remain_info->index_list));
            return autolifter::mergeInfo(current, remain_info);
        }
    }
    return nullptr;
}

TypedProgramList OCNonScalarAlignSolver::synthesisFromExample() {
    /*LOG(INFO) << "Start solve ";
    for (int i = 0; i < generator.scalar_example_list.size(); ++i) {
        auto& example = generator.scalar_example_list[i];
        LOG(INFO) << "  Scalar example #" << i;
        LOG(INFO) << "  " << info->example_pool->example_pool[example.align_id][example.x]->toString();
        LOG(INFO) << "  " << info->example_pool->example_pool[example.align_id][example.y]->toString();
    }
    for (int i = 0; i < generator.ds_example_list.size(); ++i) {
        auto& example = generator.ds_example_list[i];
        LOG(INFO) << "  DS example #" << i;
        LOG(INFO) << "  " << info->example_pool->example_pool[example.align_id][example.x]->toString();
    }*/
    for (int term_id = 0;; ++term_id) {
        int k = term_id % KComposeNum;
        ClosedEnumerateInfo current;
        if (k == 0) {
            current = generator.getNextEnumerateInfo();
            current->flag = group_list.size();
            group_list.push_back(current);
            /*LOG(INFO) << "new group";
            _printClosedInfo(current, generator.index2ProgramList(current->index_list));
            if (rand() % 10 == 0) {
                int kk; std::cin >> kk;
            }*/
        } else {
            auto& pos = progress_list[k];
            while (pos.first < working_storage[k].size() && pos.second == working_storage[k][pos.first]->flag) {
                pos.first++; pos.second = 0;
            }
            if (pos.first == working_storage[k].size()) continue;
            current = autolifter::mergeInfo(working_storage[k][pos.first], group_list[pos.second]);
            pos.second++;
        }
        if (!current || current->index_list.size() > KComposeNum) continue;
        auto feature = _indexList2String(current->index_list) + "@" + std::to_string(current->is_include_start);
        //LOG(INFO) << "current";
        //_printClosedInfo(current, generator.index2ProgramList(current->index_list));
        if (considered_set.find(feature) != considered_set.end()) continue;
        considered_set.insert(feature);
        if ((current->full & current->covered) == current->full && current->is_include_start) {
            return generator.index2ProgramList(current->index_list);
        }

        if (size_grouped_list[KComposeNum].merge(current)) {
            for (int i = 0; i <= KComposeNum - current->index_list.size(); ++i) {
                auto result = tryMerge(current, i);
                if (result) {
                    /*LOG(INFO) << "merge fin";
                    _printComponent(generator.component_info_list[0]);
                    for (auto index: result->index_list) {
                        _printComponent(generator.component_info_list[index]);
                    }
                    LOG(INFO) << "final result";
                    _printClosedInfo(result, generator.index2ProgramList(result->index_list));
                    int kk; std::cin >> kk;*/
                    return generator.index2ProgramList(result->index_list);
                }
            }
        }
        int n = current->index_list.size();
        if (n == KComposeNum) continue;
        if (!size_grouped_list[n].isCovered(current)) {
            working_storage[n].push_back(current);
            for (int i = n; i <= KComposeNum; ++i) size_grouped_list[i].insert(current);
        }
    }
}