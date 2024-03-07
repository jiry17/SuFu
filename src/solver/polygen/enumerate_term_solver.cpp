//
// Created by pro on 2023/7/22.
//

#include "istool/solver/polygen/polygen_term_solver.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/solver/enum/enum_util.h"
#include <unordered_set>
#include "glog/logging.h"

using namespace polygen;

namespace {
    const int KDefaultMaxTermNum = 4;

    int _getMaxTermNum(const PSynthInfo& info) {
        return KDefaultMaxTermNum;
    }
}

EnumeratePolyGenTermSolver::EnumeratePolyGenTermSolver(Specification *spec, const PSynthInfo &info): TermSolver(spec, info) {
    example_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!example_space) {
        LOG(FATAL) << "EnumeratePolyGenTermSolver only supports IOExampleSpace";
    }
    auto* val = spec->env->getConstRef(solver::polygen::KMaxTermNumName);
    if (!val->isNull()) KMaxTermNum = theory::clia::getIntValue(*val);
    else KMaxTermNum = _getMaxTermNum(spec->info_list[0]);

    KRelaxTimeOut = 0.1;
    KRelaxFactor = 1.5;
}

void EnumeratePolyGenTermSolver::updateInfo() {
    for (auto& term_info: info_pool) {
        auto& program = term_info.term.begin()->second;
        for (int i = term_info.P.size(); i < previous_example_list.size(); ++i) {
            auto& example = previous_example_list[i];
            try {
                if (spec->env->run(program.get(), example.first) == example.second) {
                    term_info.P.append(1);
                } else term_info.P.append(0);
            } catch (const SemanticsError &error) {
                term_info.P.append(0);
            }
        }
    }
}

void EnumeratePolyGenTermSolver::updateExamples(const ExampleList &example_list) {
    for (int i = 0; i < previous_example_list.size(); ++i) {
        auto current_example = example_space->getIOExample(example_list[i]);
        assert(current_example == previous_example_list[i]);
    }
    for (int i = previous_example_list.size(); i < example_list.size(); ++i) {
        auto current_example = example_space->getIOExample(example_list[i]);
        previous_example_list.push_back(current_example);
    }
    updateInfo();
}

namespace {
    struct _TmpInfo {
    public:
        Bitset x;
        int index, num;
        _TmpInfo(const Bitset& _x, int _index, int _num): x(_x), index(_index), num(_num) {
        }
    };
}

std::vector<int> EnumeratePolyGenTermSolver::getUsefulTermIndex(const std::vector<int> &current, const Bitset &full) {
    std::unordered_set<std::string> feature_set;
    std::vector<_TmpInfo> local_info_list;
    for (auto index: current) {
        auto now = info_pool[index].P & full;
        int num = now.count();
        auto feature = now.toXString();
        if (feature_set.find(feature) != feature_set.end()) continue;
        feature_set.insert(feature);
        local_info_list.emplace_back(now, index, num);
    }
    std::vector<int> x(local_info_list.size());
    for (int i = 0; i < local_info_list.size(); ++i) x[i] = i;
    std::sort(x.begin(), x.end(), [&](int a, int b) {return local_info_list[a].num > local_info_list[b].num;});
    //LOG(INFO) << "start reduce";
    //for (auto w: x) std::cout << " " << local_info_list[w].index; std::cout << std::endl;
    int now = 0;
    for (int i = 0; i < x.size(); ++i) {
        bool is_covered = false;
        auto& current_info = local_info_list[x[i]];
        for (int j = 0; j < now; ++j) {
            if (local_info_list[x[j]].x.checkCover(current_info.x)) {
                is_covered = true; break;
            }
        }
        if (!is_covered) x[now++] = x[i];
    }
    x.resize(now); //std::sort(x.begin(), x.end());
    for (int i = 0; i < x.size(); ++i) x[i] = local_info_list[x[i]].index;
    //for (auto w: x) std::cout << " " << w; std::cout << std::endl;
    return x;
}

std::vector<int> EnumeratePolyGenTermSolver::search(int branch_num, const std::vector<int> &index_list, const Bitset &full,
                                                    int remain_num, std::unordered_map<std::string, std::vector<int>> &cache, TimeGuard* guard) {
    assert(remain_num && branch_num); TimeCheck(guard);
    auto feature = full.toXString() + "@" + std::to_string(branch_num);
    auto it = cache.find(feature);
    if (it != cache.end()) return it->second;
    int num_lim = remain_num - ((remain_num - 1) / branch_num + 1);
    auto useful_index_list = getUsefulTermIndex(index_list, full);
    for (auto index: useful_index_list) {
        auto& term_info = info_pool[index]; auto rem = full.exclude(term_info.P);
        int current_remain_num = rem.count();
        if (current_remain_num > num_lim) break;
        if (!current_remain_num) return cache[feature] = {index};
        auto res = search(branch_num - 1, useful_index_list, rem, current_remain_num, cache, guard);
        if (!res.empty()) {
            res.push_back(index); return cache[feature] = res;
        }
    }
    return cache[feature] = {};
}

namespace {
    ProgramList _getConsideredTerms(const PSynthInfo& term_info, int target_num, double time_out) {
        std::vector<FunctionContext> result;
        EnumConfig config(nullptr, new RuleBasedOptimizer(), new TimeGuard(time_out));
        solver::collectAccordingNum({term_info}, target_num, result, config);
        ProgramList program_result;
        for (auto& ctx: result) program_result.push_back(ctx.begin()->second);
        return program_result;
    }
}

void EnumeratePolyGenTermSolver::relax(TimeGuard* guard) {
    int next_num = std::max(int(info_pool.size() * KRelaxFactor) + 1, 10);
    double time_out = KRelaxTimeOut;
    if (guard) time_out = std::min(time_out, guard->getRemainTime());

    LOG(INFO) << "start relax " << next_num << " " << time_out;
    auto program_list = _getConsideredTerms(term_info, next_num, time_out);
    // LOG(INFO) << "Next program list " << next_program_list.size() << " " << next_program_list[next_program_list.size() - 1]->toString() << " " << program_list.size();
    if (program_list.size() == info_pool.size()) {
        KRelaxTimeOut *= 2; return;
    }

    for (int i = info_pool.size(); i < program_list.size(); ++i) {
        info_pool.emplace_back(semantics::buildSingleContext("", program_list[i]));
    }
    term_num_list.push_back(program_list.size());
    updateInfo();
}

ProgramList EnumeratePolyGenTermSolver::synthesisTerms(const ExampleList &example_list, TimeGuard *guard) {
    updateExamples(example_list);
    std::vector<int> current_term_num;
    Bitset full(example_list.size(), true);
    while (1) {
        int lim = current_term_num.size();
        for (int term_batch_id = 0; term_batch_id <= lim; ++term_batch_id) {
            TimeCheck(guard);
            if (term_batch_id == term_num_list.size()) relax(guard);
            if (term_batch_id >= term_num_list.size()) continue;
            if (term_batch_id == current_term_num.size()) current_term_num.push_back(1);
            assert(term_batch_id < current_term_num.size());
            if (current_term_num[term_batch_id] > KMaxTermNum) continue;

            std::vector<int> full_index_list;
            for (int i = 0; i < term_num_list[term_batch_id]; ++i) {
                full_index_list.push_back(i);
            }
            std::unordered_map<std::string, std::vector<int>> cache;
            auto result = search(current_term_num[term_batch_id], full_index_list, full, example_list.size(), cache,
                                 guard);
            if (!result.empty()) {
                ProgramList term_list;
                for (auto index: result) term_list.push_back(info_pool[index].term.begin()->second);
                return term_list;
            }
            current_term_num[term_batch_id]++;
        }
    }
}
