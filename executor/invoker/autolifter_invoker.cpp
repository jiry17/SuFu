//
// Created by pro on 2022/2/21.
//

#include "istool/invoker/lifting_invoker.h"
#include "istool/solver/autolifter/autolifter.h"
#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/solver/polygen/polygen_cegis.h"
#include "istool/solver/polygen/lia_solver.h"
#include "istool/solver/polygen/dnf_learner.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"

namespace {
    int _getComponentNum(Type* type) {
        auto* pt = dynamic_cast<TProduct*>(type);
        if (!pt) return 1;
        int res = 0;
        for (auto& sub_type: pt->sub_types) {
            res += _getComponentNum(sub_type.get());
        }
        return res;
    }

    const SFSolverBuilder KDefaultSFBuilder = [](PartialLiftingTask* task) {return new ComposedSFSolver(task);};

    int _accessIntConfig(Env* env, const std::string& name, int default_value) {
        auto* v = env->getConstRef(name);
        if (v->isNull()) return default_value;
        return theory::clia::getIntValue(*v);
    }

    SolverBuilder _buildDefaultSCBuilder(Env* env) {
        int KMaxTermNum = _accessIntConfig(env, solver::polygen::KMaxTermNumName, 4);
        int KConstIntMax = _accessIntConfig(env, solver::lia::KConstIntMaxName, 1);
        int KTermIntMax = _accessIntConfig(env, solver::lia::KTermIntMaxName, 2);
        int KMaxCost = _accessIntConfig(env, solver::lia::KMaxCostName, 4);
        int KMaxClauseNum = _accessIntConfig(env, solver::polygen::KMaxClauseNumName, 3);

        return [=](Specification *spec, Verifier *v) -> Solver * {
            int total_component_num = 0;
            for (const auto &type: spec->info_list[0]->inp_type_list) {
                total_component_num += _getComponentNum(type.get());
            }
            spec->env->setConst(solver::polygen::KMaxTermNumName, BuildData(Int, std::min(KMaxTermNum, total_component_num)));
            spec->env->setConst(solver::lia::KConstIntMaxName, BuildData(Int, KConstIntMax));
            spec->env->setConst(solver::lia::KTermIntMaxName, BuildData(Int, KTermIntMax));
            spec->env->setConst(solver::lia::KMaxCostName, BuildData(Int, KMaxCost));
            spec->env->setConst(solver::polygen::KMaxClauseNumName, BuildData(Int, KMaxClauseNum));
            spec->env->setConst(solver::polygen::KIsAllowErrorName, BuildData(Bool, true));
            return invoker::builderSolver(spec, v, SolverToken::POLYGEN, {});
        };
    }
}

AutoLifter* invoker::single::buildAutoLifter(LiftingTask *task, const InvokeConfig &config) {
    auto sf_builder = config.access("SfBuilder", KDefaultSFBuilder);
    auto sc_builder = config.access("ScBuilder", _buildDefaultSCBuilder(task->env.get()));
    auto* solver = new AutoLifter(task, sf_builder, sc_builder);
    return solver;
}

LiftingRes invoker::single::invokeAutoLifter(LiftingTask *task, TimeGuard *guard, const InvokeConfig &config) {
    auto* solver = invoker::single::buildAutoLifter(task, config);
    auto res = solver->synthesis(guard);
    delete solver;
    return res;
}