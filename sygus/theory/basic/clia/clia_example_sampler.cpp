//
// Created by pro on 2022/5/12.
//

#include "istool/sygus/theory/basic/clia/clia_example_sampler.h"

ExampleList IntExampleGenerator::generateExamples(TimeGuard *guard) {
    Example res;
    int KIntMin = theory::clia::getIntValue(*int_min);
    int KIntMax = theory::clia::getIntValue(*int_max);
    std::uniform_int_distribution<int> d(KIntMin, KIntMax);
    std::bernoulli_distribution b;
    for (auto& type: type_list) {
        if (dynamic_cast<TBool*>(type.get())) {
            res.push_back(BuildData(Bool, b(env->random_engine)));
        } else if (dynamic_cast<TInt*>(type.get())) {
            res.push_back(BuildData(Int, d(env->random_engine)));
        } else assert(0);
    }
    return {res};
}

namespace {
    const int KDefaultIntMin = -5;
    const int KDefaultIntMax = 5;
}

const std::string theory::clia::KSampleIntMaxName = "Sample@IntMax";
const std::string theory::clia::KSampleIntMinName = "Sample@IntMin";

IntExampleGenerator::IntExampleGenerator(Env *_env, const TypeList &_type_list): env(_env), type_list(_type_list) {
    int_min = env->getConstRef(theory::clia::KSampleIntMinName, BuildData(Int, KDefaultIntMin));
    int_max = env->getConstRef(theory::clia::KSampleIntMaxName, BuildData(Int, KDefaultIntMax));
}