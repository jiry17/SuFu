//
// Created by pro on 2021/12/9.
//

#include "istool/solver/solver.h"
#include "glog/logging.h"
#include <mutex>

Solver::Solver(Specification *_spec): spec(_spec) {}
VerifiedSolver::VerifiedSolver(Specification *spec, Verifier *_v): Solver(spec), v(_v) {}
//TODO: delete v;
VerifiedSolver::~VerifiedSolver() {/*delete v;*/}
PBESolver::PBESolver(Specification *_spec): spec(_spec) {}

CEGISSolver::CEGISSolver(PBESolver *_pbe_solver, Verifier *_v):
    pbe_solver(_pbe_solver), VerifiedSolver(_pbe_solver->spec, _v) {
}
CEGISSolver::~CEGISSolver() {
    delete pbe_solver;
}
FunctionContext CEGISSolver::synthesis(TimeGuard* guard) {
    std::vector<Example> example_list;
    while (1) {
        TimeCheck(guard);
        auto res = pbe_solver->synthesis(example_list, guard);
        if (res.empty()) {
            auto* new_solver = solver::relaxSolver(pbe_solver);
            if (new_solver) {
                delete pbe_solver; pbe_solver = new_solver;
                continue;
            } else return res;
        }
        LOG(INFO) << "Candidate result " << res.toString();
#ifdef DEBUG
        for (auto& example: example_list) assert(spec->example_space->satisfyExample(res, example));
#endif
        Example counter_example;
        if (v->verify(res, &counter_example)) {
            return res;
        }
        LOG(INFO) << "Counter example " << data::dataList2String(counter_example);
        LOG(INFO) << "Current examples";
        for (auto& example: example_list) {
            LOG(INFO) << "  " << data::dataList2String(example);
        }
        // auto* io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
        // if (io_space) LOG(INFO) << "ExampleSpace " << example::ioExample2String(io_space->getIOExample(counter_example));
        example_list.push_back(counter_example);
    }
}