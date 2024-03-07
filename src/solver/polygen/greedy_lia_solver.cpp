//
// Created by pro on 2023/7/21.
//

#include "istool/solver/polygen/lia_solver.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

namespace {
    const int KDefaultTestNum = 20;

    std::pair<int, int> _getCost(const LIAResult& result) {
        int nz = 0, tot = 0;
        if (result.c_val) nz++; tot += std::abs(result.c_val);
        for (int w: result.param_list) {
            if (w) nz++; tot += std::abs(w);
        }
        return {nz, tot};
    }
}

GreedyLIASolver::GreedyLIASolver(Specification *_spec, const ProgramList &_program_list):
    BaseLIASolver(_spec, _program_list) {
    auto* data = spec->env->getConstRef(solver::lia::KRandomTestNumName, BuildData(Int, KDefaultTestNum));
    KRandomTestNum = theory::clia::getIntValue(*data);
}

const std::string solver::lia::KRandomTestNumName = "GreedyLIA@RandomTestNum";

namespace {
    const double KEps = 1e-6;

    int _sign(double w) {
        if (w < -KEps) return -1; else if (w > KEps) return 1; else return 0;
    }
    int _round(double w) {
        return (int)floor(w + 0.5);
    }

    LIAResult _gauss(const IOExampleList& example_list, int n, const std::vector<int>& order, TimeGuard* guard) {
        int m = example_list.size();
        std::vector<std::vector<double>> x(m, std::vector<double>(n + 2, 0.0));
        for (int i = 0; i < m; ++i) {
            TimeCheck(guard);
            for (int j = 0; j < n; j++) {
                //LOG(INFO) << "access " << example_list[i].first.size() << " " << order[j];
                x[i][j] = theory::clia::getIntValue(example_list[i].first[order[j]]);
            }
            x[i][n] = 1;
            x[i][n + 1] = theory::clia::getIntValue(example_list[i].second);
        }
        int now = 0;
        std::vector<int> key;
        /*LOG(INFO) << "init matrix";
        for (auto& r: x) {
            for (auto& w: r) std::cout << w << " ";
            std::cout << std::endl;
        }*/
        for (int c = 0; c <= n && now < m; ++c) {
            int pos = now;
            for (int r = now; r < m; ++r) {
                TimeCheck(guard);
                if (std::fabs(x[r][c]) > std::fabs(x[pos][c])) pos = r;
            }
            if (_sign(x[pos][c]) == 0) continue;
            std::swap(x[now], x[pos]); key.push_back(c);
            for (int r = now + 1; r < m; ++r) {
                TimeCheck(guard);
                if (_sign(x[r][c]) == 0) continue;
                double w = x[r][c] / x[now][c];
                for (int c2 = 0; c2 <= n + 1; ++c2) {
                    x[r][c2] -= x[now][c2] * w;
                }
            }
            ++now;
        }
        /*LOG(INFO) << "result matrix";
        std::cout << "key";
        for (auto w: key) std::cout << " " << w; std::cout << std::endl;
        for (auto& r: x) {
            for (auto& w: r) std::cout << w << " ";
            std::cout << std::endl;
        }*/
        for (int r = now; r < m; ++r) {
            if (_sign(x[r][n + 1])) return {LIAResult::Status::INFEASIBLE};
        }
        std::vector<int> res(n + 1, 0);
        for (int i = now - 1; i >= 0; --i) {
            double rem = x[i][n + 1];
            for (int j = i + 1; j < now; ++j) rem -= x[i][key[j]] * res[key[j]];
            //LOG(INFO) << "current " << key[i] << " " << rem << " " << x[i][key[i]] << " " << _round(rem / x[i][key[i]]);
            res[key[i]] = _round(rem / x[i][key[i]]);
        }
        int c = res[n]; std::vector<int> param_list(n, 0);
        for (int i = 0; i < n; ++i) param_list[order[i]] = res[i];

        /*LOG(INFO) << "solver" << std::endl;
        for (auto& example: example_list) LOG(INFO) << "  " << example::ioExample2String(example) << std::endl;
        std::string solution = std::to_string(c) + "[";
        for (int i = 0; i < param_list.size(); ++i) {
            if (i) solution += ","; solution += std::to_string(param_list[i]);
        }
        LOG(INFO) << "sol " << solution << "]";
        // int kk; std::cin >> kk;*/

        for (auto& [inp, oup]: example_list) {
            int w = c;
            for (int i = 0; i < param_list.size(); ++i) w += param_list[i] * theory::clia::getIntValue(inp[i]);
            if (w != theory::clia::getIntValue(oup)) return {LIAResult::Status::INFEASIBLE};
        }
        return {param_list, c};
    }
}

FunctionContext GreedyLIASolver::synthesis(const std::vector<Example> &example_list, TimeGuard *guard) {
    auto name = spec->info_list[0]->name;
    {
        auto trivial_result = trivialSolve(example_list);
        if (trivial_result) return semantics::buildSingleContext(name, trivial_result);
    }
    auto [wrapped_example_list, considered_programs] = initializeExamples(example_list);
    PProgram best_result;
    std::pair<int, int> best_cost = {1e9, 1e9};
    std::vector<int> order; int n = considered_programs.size();
    for (int i = 0; i < n; ++i) order.push_back(i);
    for (int _ = 0; _ < KRandomTestNum; ++_) {
        TimeCheck(guard);
        std::shuffle(order.begin(), order.end(), spec->env->random_engine);
        auto result = _gauss(wrapped_example_list, n, order, guard);
        if (result.status == LIAResult::Status::SUCCESS) {
            for (int w: result.param_list) if (std::abs(w) > KTermIntMax) continue;
            if (std::abs(result.c_val) > KConstIntMax) continue;
            auto current_cost = _getCost(result);
            if (current_cost < best_cost) {
                best_cost = current_cost;
                best_result = buildProgram(result, considered_programs, spec->env.get());
            }
        }
    }
    if (best_result) {
        return semantics::buildSingleContext(name, best_result);
    }
    return {};
}

BaseLIASolver *GreedyLIASolver::clone(Specification *spec, const ProgramList &program_list) {
    return new GreedyLIASolver(spec, program_list);
}