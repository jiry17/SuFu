//
// Created by pro on 2022/1/20.
//

#ifndef ISTOOL_EXAMPLE_SAMPLER_H
#define ISTOOL_EXAMPLE_SAMPLER_H

#include <functional>
#include "example_space.h"
#include "time_guard.h"

class ExampleGenerator {
public:
    virtual ExampleList generateExamples(TimeGuard* guard) = 0;
    virtual ~ExampleGenerator() = default;
};

typedef std::shared_ptr<ExampleGenerator> PExampleGenerator;

#endif //ISTOOL_EXAMPLE_SAMPLER_H
