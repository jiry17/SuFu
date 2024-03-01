//
// Created by pro on 2022/1/27.
//

#ifndef ISTOOL_SAMPLESY_H
#define ISTOOL_SAMPLESY_H

#include "istool/selector/selector.h"
#include "istool/selector/split/splitor.h"

class SeedGenerator {
public:
    virtual void addExample(const IOExample& example) = 0;
    virtual ProgramList getSeeds(int num, double time_limit) = 0;
    virtual ~SeedGenerator() = default;
};

class SampleSy: public CompleteSelector {
public:
    Splitor* splitor;
    SeedGenerator* gen;
    Data* KSampleTimeOut, *KSampleNum;
    TimeGuard* turn_guard = nullptr;
    ProgramList samples;
    IOExampleList example_list;
    SampleSy(Specification* _spec, Splitor* _splitor, SeedGenerator* _gen, GrammarEquivalenceChecker* checker);
    virtual Example getNextExample(const PProgram& x, const PProgram& y);
    virtual void addExample(const IOExample& example);
    virtual FunctionContext synthesis(TimeGuard *guard);
    virtual ~SampleSy();
};

namespace selector::samplesy {
    extern const std::string KSampleTimeOutLimit;
    extern const std::string KSampleNumLimit;
}

#endif //ISTOOL_SAMPLESY_H
