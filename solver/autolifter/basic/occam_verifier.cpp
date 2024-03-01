//
// Created by pro on 2022/1/20.
//

#include "istool/solver/autolifter/basic/occam_verifier.h"

namespace {
    const int KDefaultExampleNum = 1000;
}

OccamVerifier::OccamVerifier(StreamedExampleSpace *_example_space): example_space(_example_space) {
    example_num = example_space->env->getConstRef(solver::autolifter::KOccamExampleNumName);
    pos = 0;
}

bool OccamVerifier::verify(const FunctionContext &info, Example *counter_example) {
    int total_size = 0;
    for (const auto& item: info) total_size += item.second->size();
    int target = total_size * KDefaultExampleNum;
    target = example_space->extendExampleList(target, nullptr);
    if (pos > target) pos = 0;
    for (int i = 0; i < target; ++i) {
        auto example = example_space->getExample(pos);
        if (!example_space->satisfyExample(info, example)) {
            *counter_example = example;
            return false;
        }
        pos = (pos + 1) % target;
    }
    return true;
}

const std::string solver::autolifter::KOccamExampleNumName = "OccamVerifier@ExampleNum";