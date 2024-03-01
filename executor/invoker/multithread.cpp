//
// Created by pro on 2023/5/16.
//

#include "istool/invoker/multithread_guard.h"
#include "istool/invoker/invoker.h"
#include <thread>

void MultiThreadTimeGuard::check() {
    if (lock.try_lock()) {
        if (is_finished) {
            lock.unlock();
            throw TimeOutError();
        }
        lock.unlock();
    }
    TimeGuard::check();
}

void MultiThreadTimeGuard::finish() {
    lock.lock();
    is_finished = true;
    lock.unlock();
}

MultiThreadTimeGuard::MultiThreadTimeGuard(const TimeGuard& guard): TimeGuard(guard), is_finished(false) {
}

MultiThreadTimeGuard::MultiThreadTimeGuard(double time_limit): TimeGuard(time_limit), is_finished(false) {
}

FunctionContext invoker::multi::synthesis(Specification *spec, Verifier *v, const InvokeConfig &config, TimeGuard* guard) {
    std::vector<SolverBuilder> builder_list;
    builder_list = config.access("solver_list", builder_list);


    MultiThreadTimeGuard* multi_guard;
    if (guard) multi_guard = new MultiThreadTimeGuard(*guard); else multi_guard = new MultiThreadTimeGuard(1e9);
    std::vector<Solver*> solver_list;
    for (auto builder: builder_list) {
        solver_list.push_back(builder(spec, v));
    }

    auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    for (int i = 0; i < 10 && i < fio->example_space.size(); ++i) {
        auto& example = fio->example_space[i];
        LOG(INFO) << example::ioExample2String(fio->getIOExample(example));
    }
    std::mutex res_lock;
    FunctionContext res;

    auto run = [&](Solver* solver, int ind) -> void {
        try {
            auto current_res = solver->synthesis(multi_guard);
            if (!current_res.empty()) {
                res_lock.lock();
                res = current_res;
                res_lock.unlock();
                multi_guard->finish();
            }
        } catch (const TimeOutError& e) {
            LOG(INFO) << "Interrupt " << ind;
        }
    };

    std::vector<std::thread> thread_list;
    for (int i = 0; i < solver_list.size(); ++i) {
        thread_list.emplace_back(run, solver_list[i], i);
    }
    for (int i = 0; i < solver_list.size(); ++i) {
        thread_list[i].join(); delete solver_list[i];
    }
    return res;
}