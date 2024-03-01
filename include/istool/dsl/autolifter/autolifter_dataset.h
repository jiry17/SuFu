//
// Created by pro on 2022/2/23.
//

#ifndef ISTOOL_AUTOLIFTER_DAD_DATASET_H
#define ISTOOL_AUTOLIFTER_DAD_DATASET_H

#include "istool/solver/autolifter/basic/lifting_problem.h"

namespace dsl::autolifter {
    namespace track {
        LiftingTask* getDaDLiftingTask(const std::string& name);
        LiftingTask* getLazyTagLiftingTask(const std::string& name);
        LiftingTask* getLSPLiftingTask(const std::string& name);
    }
    LiftingTask* getLiftingTask(const std::string& track_name, const std::string& task_name);
}

#endif //ISTOOL_AUTOLIFTER_DAD_DATASET_H
