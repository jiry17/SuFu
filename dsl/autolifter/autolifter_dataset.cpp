//
// Created by pro on 2022/3/7.
//

#include "istool/dsl/autolifter/autolifter_dataset.h"
#include "glog/logging.h"

LiftingTask * dsl::autolifter::getLiftingTask(const std::string& track_name, const std::string &task_name) {
    if (track_name == "dad") return dsl::autolifter::track::getDaDLiftingTask(task_name);
    if (track_name == "lazy-tag") return dsl::autolifter::track::getLazyTagLiftingTask(task_name);
    if (track_name == "lsp") return dsl::autolifter::track::getLSPLiftingTask(task_name);
    LOG(FATAL) << "Unknown track name " << track_name;
}