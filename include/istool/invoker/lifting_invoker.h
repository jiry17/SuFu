//
// Created by pro on 2022/2/21.
//

#ifndef ISTOOL_LIFTING_INVOKER_H
#define ISTOOL_LIFTING_INVOKER_H

#include "istool/solver/autolifter/basic/lifting_solver.h"
#include "istool/solver/autolifter/autolifter.h"
#include "istool/invoker/invoker.h"

namespace invoker::single {
     AutoLifter* buildAutoLifter(LiftingTask* task, const InvokeConfig& config);
     LiftingRes invokeAutoLifter(LiftingTask* task, TimeGuard* guard, const InvokeConfig& config);
}

#endif //ISTOOL_LIFTING_INVOKER_H
