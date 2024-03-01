//
// Created by pro on 2022/1/21.
//

#include "istool/solver/autolifter/sf_verifier.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "istool/ext/deepcoder/data_util.h"
#include "glog/logging.h"

namespace {
    int KDefaultExampleNum = 1000;
}

SFVerifier::SFVerifier(PartialLiftingTask *_task): task(_task), size_limit(5) {
    auto* env = task->info->env;
    example_num = env->getConstRef(solver::autolifter::KOccamExampleNumName, BuildData(Int, KDefaultExampleNum));
    p_list = ext::ho::splitProduct(task->p);
    for (const auto& p: p_list) {
        auto *cache = task->info->getModCache(p.get());
        if (!cache) {
            cache = task->info->registerModCache(p.get(), {});
        }
        p_cache_list.push_back(cache);
    }

    h_list = ext::ho::splitProduct(task->h);
    for (const auto& h: h_list) {
        auto* cache = task->info->getFMapCache(h.get());
        if (!cache) {
            cache = task->info->registerFMapCache(h.get(), {});
        }
        h_cache_list.push_back(cache);
    }

    example_pos = 0;
}

namespace {
    DataList _shift(const DataList& x, int s) {
        DataList res(x.size());
        for (int i = 0; i < x.size(); ++i) {
            res[s] = x[i];
            s = (s + 1) % x.size();
        }
        return res;
    }
}

std::pair<int, int> SFVerifier::verify(const PProgram& f) {
    int num = theory::clia::getIntValue(*example_num);
    int f_size = f->size();
    if (f_size > size_limit) size_limit = std::max(size_limit * 2, f_size);
    int target = num * size_limit;
    target = task->info->example_space->extendExampleList(target, nullptr);

    ProgramList f_list = ext::ho::splitProduct(f);
    DataStorage tmp_cache(f_list.size());
    std::vector<DataList*> f_cache_list(f_list.size(), nullptr);
    for (int i = 0; i < f_cache_list.size(); ++i) {
        f_cache_list[i] = task->info->getFMapCache(f_list[i].get());
    }

    for (int i = 0; i < p_list.size(); ++i) {
        task->info->extendModCache(p_list[i].get(), p_cache_list[i], target);
    }
    for (int i = 0; i < f_list.size(); ++i) {
        if (f_cache_list[i]) {
            task->info->extendFMapCache(f_list[i].get(), f_cache_list[i], target);
        }
    }

    auto get_feature = [&](int id) -> std::pair<std::string, std::string> {
        DataList eq_part, test_part;
        for (auto* cache: p_cache_list) eq_part.push_back(cache->at(id));
        for (int i = 0; i < f_cache_list.size(); ++i) {
            if (f_cache_list[i]) test_part.push_back(f_cache_list[i]->at(id));
            else {
                auto res = task->info->getFMapResult(f_list[i].get(), id);
                test_part.push_back(res);
                tmp_cache[i].push_back(res);
            }
        }
        return {data::dataList2String(eq_part), data::dataList2String(test_part)};
    };

    std::unordered_map<std::string, std::pair<int, std::string>> feature_map;
    for (int i = 0; i < target; ++i) {
        auto feature = get_feature(example_pos);
        auto it = feature_map.find(feature.second);
        if (it == feature_map.end()) {
            feature_map[feature.second] = {example_pos, feature.first};
        } else if (it->second.second != feature.first) {
            return {example_pos, it->second.first};
        }
        example_pos = (example_pos + 1) % target;
    }

    for (int i = 0; i < f_list.size(); ++i) {
        if (!f_cache_list[i]) {
            auto res = _shift(tmp_cache[i], example_pos);
            task->info->registerFMapCache(f_list[i].get(), _shift(tmp_cache[i], example_pos));
        }
    }
    return {-1, -1};
}

bool SFVerifier::verify(const FunctionContext &info, Example *counter_example) {
    if (info.size() > 1) LOG(FATAL) << "There is only a single target program in a PLP task";
    auto res = verify(info.begin()->second);
    if (res.first == -1) return true;
    auto x = task->info->example_space->getExample(res.first)[0];
    auto y = task->info->example_space->getExample(res.second)[0];
    (*counter_example) = {x, y};
    return false;
}