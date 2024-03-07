//
// Created by pro on 2021/12/27.
//

#include <algorithm>
#include "istool/solver/enum/enum_solver.h"
#include "glog/logging.h"

BasicEnumSolver::BasicEnumSolver(Specification *_spec, Verifier *_v): VerifiedSolver(_spec, _v){
}
FunctionContext BasicEnumSolver::synthesis(TimeGuard* guard) {
    auto* o = new TrivialOptimizer();
    EnumConfig c(v, o, guard);
    return solver::enumerate(spec->info_list, c);
}

namespace {
    void collectAllInvocation(const PProgram& program, std::unordered_map<std::string, ProgramList>& invoke_map) {
        auto* is = dynamic_cast<InvokeSemantics*>(program->semantics.get());
        if (is) {
            invoke_map[is->name].push_back(program);
        }
        for (const auto& sub: program->sub_list) {
            collectAllInvocation(sub, invoke_map);
        }
    }

    bool isGrounded(const PProgram& program) {
        auto* is = dynamic_cast<InvokeSemantics*>(program->semantics.get());
        if (is) return false;
        for (const auto& sub: program->sub_list) {
            if (!isGrounded(sub)) return false;
        }
        return true;
    }
}

OBESolver::OBESolver(Specification *_spec, Verifier* _v, ProgramChecker* _is_runnable): PBESolver(_spec), v(_v), is_runnable(_is_runnable) {
    std::unordered_map<std::string, ProgramList> raw_invoke_map;
    collectAllInvocation(spec->example_space->cons_program, raw_invoke_map);
    for (auto& collect_info: raw_invoke_map) {
        auto name = collect_info.first;
        bool is_grounded = true;
        for (auto& p: collect_info.second) {
            for (auto& sub: p->sub_list) {
                if (!isGrounded(sub) || !is_runnable->isValid(sub.get())) {
                    is_grounded = false;
                    LOG(WARNING) << "Observational equivalence cannot be applied to function " << name <<
                                    " because invocation " << p->toString() << " is not grounded";
                    break;
                }
            }
            if (!is_grounded) break;
        }
        if (is_grounded) {
            invoke_map[name] = collect_info.second;
        }
    }
}

FunctionContext OBESolver::synthesis(const std::vector<Example> &example_list, TimeGuard* guard) {
    std::unordered_map<std::string, ExampleList> example_pool;
    for (auto& invoke_info: invoke_map) {
        std::string name = invoke_info.first;
        std::unordered_set<std::string> feature_set;
        ExampleList res;
        for (const auto& p: invoke_info.second) {
            for (const auto& example: example_list) {
                Example invoke_example;
                for (const auto& sub: p->sub_list) {
                    invoke_example.push_back(spec->env->run(sub.get(), example));
                }
                auto feature = data::dataList2String(invoke_example);
                if (feature_set.find(feature) == feature_set.end()) {
                    res.push_back(invoke_example);
                    feature_set.insert(feature);
                }
            }
        }
        std::shuffle(res.begin(), res.end(), spec->env->random_engine);
        example_pool[name] = res;
    }

    Env* env = spec->env.get();
    auto* obe_optimizer = new OBEOptimizer(is_runnable, example_pool, env);
    auto* finite_example_space = new FiniteExampleSpace(spec->example_space->cons_program, example_list, env);
    auto* finite_verifier = new FiniteExampleVerifier(finite_example_space);

    EnumConfig c(finite_verifier, obe_optimizer, guard);

    auto res = solver::enumerate(spec->info_list, c);

    delete obe_optimizer;
    delete finite_example_space;
    delete finite_verifier;
    return res;
}

OBESolver::~OBESolver() noexcept {
    delete is_runnable;
}