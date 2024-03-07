//
// Created by pro on 2022/5/12.
//

#ifndef ISTOOL_DATA_EXAMPLE_SAMPLER_H
#define ISTOOL_DATA_EXAMPLE_SAMPLER_H

#include "data_value.h"
#include "istool/basic/example_sampler.h"

typedef std::function<bool(const Example&)> ExampleChecker;
class BasicRandomSampler: public ExampleGenerator {
    void initConst();
    Data sampleWithType(Type* type);
    BTreeNode sampleTree(Type* node_type, Type* leaf_type, int size);
public:
    TypeList type_list;
    PProgram checker;
    Env* env;
    Data* int_min, *int_max, *size_max;
    BasicRandomSampler(const TypeList& _type_list, PProgram& _chk, Env* _env);
    BasicRandomSampler(const TypeList& _type_list, const ExampleChecker& _chk, Env* _env);
    virtual ExampleList generateExamples(TimeGuard* guard);
    virtual ~BasicRandomSampler() = default;
};

namespace ext::ho {
    extern const std::string KSampleDSSizeName;
}

#endif //ISTOOL_DATA_EXAMPLE_SAMPLER_H
