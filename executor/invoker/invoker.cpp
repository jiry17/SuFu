//
// Created by pro on 2022/2/15.
//

#include "istool/invoker/invoker.h"
#include "glog/logging.h"

InvokeConfig::~InvokeConfig() {
    for (auto& item: item_map) delete item.second;
}
InvokeConfig::InvokeConfig(const InvokeConfig &config) {
    for (auto& info: config.item_map) {
        item_map[info.first] = new InvokeConfigItem(*info.second);
    }
}

InvokeConfig::InvokeConfigItem::InvokeConfigItem(void *_data, std::function<void(void *)> _free_operator,
                                                 std::function<void *(void *)> _copy_operator):
                                                 data(_data), free_operator(_free_operator), copy_operator(_copy_operator) {
}
InvokeConfig::InvokeConfigItem::InvokeConfigItem(const InvokeConfigItem &item):
    data(item.copy_operator(item.data)), free_operator(item.free_operator), copy_operator(item.copy_operator) {
}
InvokeConfig::InvokeConfigItem::~InvokeConfigItem() {
    free_operator(data);
}

#define RegisterSolverBuilder(name) return invoker::single::build ## name(spec, v, config)

Solver *invoker::builderSolver(Specification *spec, Verifier *v, SolverToken token, const InvokeConfig &config) {
    switch (token) {
        case SolverToken::OBSERVATIONAL_EQUIVALENCE:
        RegisterSolverBuilder(OBE);
        case SolverToken::POLYGEN:
        RegisterSolverBuilder(PolyGen);
        case SolverToken::POLYGEN_CONDITION:
        RegisterSolverBuilder(CondSolver);
        default:
            LOG(FATAL) << "Unknown solver token";
    }
}

FunctionContext invoker::synthesis(Specification *spec, Verifier *v, SolverToken solver_token, TimeGuard* guard, const InvokeConfig& config) {
    if (solver_token == SolverToken::MULTI_THREAD) return multi::synthesis(spec, v, config, guard);
    else {
        auto* solver = builderSolver(spec, v, solver_token, config);
        auto res = solver->synthesis(guard);
        delete solver;
        return res;
    }
}