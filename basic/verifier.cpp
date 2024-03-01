//
// Created by pro on 2021/12/5.
//

#include "istool/basic/verifier.h"
#include "glog/logging.h"

FiniteExampleVerifier::FiniteExampleVerifier(FiniteExampleSpace *_space): example_space(_space) {
}
bool FiniteExampleVerifier::verify(const FunctionContext &info, Example *counter_example) {
    for (auto& example: example_space->example_space) {
        if (!example_space->satisfyExample(info, example)) {
            if (counter_example) *counter_example = example;
            return false;
        }
    }
    return true;
}