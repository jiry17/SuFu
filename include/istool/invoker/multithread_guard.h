//
// Created by pro on 2023/5/16.
//

#ifndef ISTOOL_MULTITHREAD_GUARD_H
#define ISTOOL_MULTITHREAD_GUARD_H

#include "istool/basic/time_guard.h"
#include <mutex>

class MultiThreadTimeGuard: public TimeGuard {
public:
    std::mutex lock;
    bool is_finished;
    void finish();
    virtual void check();
    MultiThreadTimeGuard(const TimeGuard& guard);
    MultiThreadTimeGuard(double time_limit);
    ~MultiThreadTimeGuard() = default;
};
#endif //ISTOOL_MULTITHREAD_GUARD_H
