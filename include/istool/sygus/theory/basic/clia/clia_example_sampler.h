//
// Created by pro on 2022/5/12.
//

#ifndef ISTOOL_CLIA_EXAMPLE_SAMPLER_H
#define ISTOOL_CLIA_EXAMPLE_SAMPLER_H

#include "istool/basic/example_sampler.h"
#include "clia_value.h"

class IntExampleGenerator: public ExampleGenerator {
public:
    Data* int_min, *int_max;
    TypeList type_list;
    Env* env;
    IntExampleGenerator(Env* _env, const TypeList& _type_list);
    virtual ExampleList generateExamples(TimeGuard* guard);
    virtual ~IntExampleGenerator() = default;
};

namespace theory::clia {
    extern const std::string KSampleIntMinName;
    extern const std::string KSampleIntMaxName;

}
#endif //ISTOOL_CLIA_EXAMPLE_SAMPLER_H
