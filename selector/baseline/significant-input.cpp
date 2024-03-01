//
// Created by pro on 2022/7/1.
//

#include "istool/selector/baseline/significant_input.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

SignificantInputSelector::SignificantInputSelector(FiniteExampleSpace *_example_space, const std::string &_benchmark_name, const std::string &_result_cache):
    example_space(_example_space), benchmark_name(_benchmark_name), result_cache(_result_cache) {
    if (result_cache.empty()) result_cache = config::KSourcePath + "/selector/baseline/significant-input";
}

void SignificantInputSelector::loadPartition() {
    std::string file_name = result_cache + "/" + benchmark_name + ".s";
    auto* f = fopen(file_name.c_str(), "r");
    int n; fscanf(f, "%d", &n); int tot = 0;
    for (;n;n--) {
        int size; fscanf(f, "%d", &size); tot += size;
        std::vector<int> x;
        for (;size;size--) {
            int k; fscanf(f, "%d", &k); x.push_back(k - 1);
        }
        partition.push_back(x);
    }
    assert(tot == example_space->example_space.size());
    char ch[100];
    for (int i = 0; i < 3; ++i) fscanf(f, "%s", ch + 1);
    assert(ch[1] == '='); fscanf(f, "%s", ch + 1);
    std::string s(ch + 1); s.pop_back(); s.pop_back();
    double cost = std::stod(s);
    // simulate the time cost of calculating the significant input
    LOG(INFO) << "cost of significant input " << cost;
    auto* guard = new TimeGuard(cost / 1000.0);
    while (guard->getRemainTime() > 0);
}

bool SignificantInputSelector::verify(const FunctionContext &info, Example *counter_example) {
    if (partition.empty()) loadPartition();
    for (int _ = 0; _ < partition.size(); ++_) {
        pos = (pos + 1) % partition.size();
        std::shuffle(partition[pos].begin(), partition[pos].end(), example_space->env->random_engine);
        for (auto id: partition[pos]) {
            if (!example_space->satisfyExample(info, example_space->example_space[id])) {
                if (counter_example) * counter_example = example_space->example_space[id];
                return false;
            }
        }
    }
    return true;
}