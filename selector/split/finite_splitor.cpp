//
// Created by pro on 2022/1/14.
//

#include "istool/selector/split/finite_splitor.h"
#include "glog/logging.h"

FiniteSplitor::FiniteSplitor(ExampleSpace *_example_space): Splitor(_example_space), env(example_space->env) {
    io_space = dynamic_cast<FiniteIOExampleSpace*>(example_space);
    if (!io_space) {
        LOG(FATAL) << "FiniteSplitSelector supports only FiniteIOExampleSpace";
    }
    for (auto& example: io_space->example_space) {
        example_list.push_back(io_space->getIOExample(example));
    }
}

namespace {
    std::string _getFeature(const std::string& x, const std::string& y) {
        if (x > y) return y + "@" + x;
        return x + "@" + y;
    }
}

bool FiniteSplitor::getExample(const std::function<bool (const IOExample &)> &filter, const ProgramList& seed_list, Example *counter_example, TimeGuard *guard) {
    int best_id = -1, best_cost = 1e9;
    for (int i = 0; i < example_list.size(); ++i) {
        auto& example = example_list[i];
        if (!filter(example)) continue;
        int current_cost = getCost(example.first, seed_list);
        if (current_cost < best_cost) {
            best_cost = current_cost;
            best_id = i;
        }
    }
    if (best_id == -1) return true;
    if (counter_example) *counter_example = io_space->example_space[best_id];
    return false;
}

bool FiniteSplitor::getCounterExample(Program *p, const ProgramList &seed_list, Example *counter_example, TimeGuard *guard) {
    auto filter = [this, p](const IOExample& example) {
        return !(this->env->run(p, example.first) == example.second);
    };
    return getExample(filter, seed_list, counter_example, guard);
}

bool FiniteSplitor::getDistinguishExample(Program *x, Program *y, const ProgramList &seed_list, Example *counter_example, TimeGuard *guard) {
    auto filter = [this, x, y](const IOExample& example) {
        return !(this->env->run(x, example.first) == this->env->run(y, example.first));
    };
    return getExample(filter, seed_list, counter_example, guard);
}

int FiniteSplitor::getCost(const DataList &inp, const ProgramList &seed_list) {
    DataList oup_list;

    std::unordered_map<std::string, int> feature_map;
    int cost = 0;
    int num = std::min(int(seed_list.size()), 10000);
    for (int i = 0; i < num; ++i) {
        auto now = env->run(seed_list[i].get(), inp).toString();
        cost = std::max(cost, ++feature_map[now]);
    }
    return cost;
/*
    std::vector<std::string> feature_list;
    for (auto& seed: seed_list) {
        oup_list.push_back(env->run(seed.get(), inp));
        feature_list.push_back(seed->toString());
    }
    int cost = 0;
    int num = std::min(500, int(seed_list.size()));
    for (int i = 0; i < num; ++i) {
        for (int j = i + 1; j < num; ++j) {
            if (!(oup_list[i] == oup_list[j])) continue;
            auto feature = _getFeature(feature_list[i], feature_list[j]);
            if (feature_cache.find(feature) != feature_cache.end()) continue;
            ++cost;
        }
    }
    return cost;*/
}