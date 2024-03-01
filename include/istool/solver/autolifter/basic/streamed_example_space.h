//
// Created by pro on 2022/1/20.
//

#ifndef ISTOOL_STREAMED_EXAMPLE_SPACE_H
#define ISTOOL_STREAMED_EXAMPLE_SPACE_H

#include "istool/basic/example_space.h"
#include "istool/basic/time_guard.h"
#include "istool/basic/example_sampler.h"

class StreamedExampleSpace: public ExampleSpace {
public:
    ExampleList example_list;
    PExampleGenerator g;
    Data* generate_timeout;
    StreamedExampleSpace(const PProgram& cons_program, const PExampleGenerator& g, Env* env);
    bool generateExample(TimeGuard* guard);
    int extendExampleList(int target, TimeGuard* guard);
    Example getExample(int k);
    virtual ~StreamedExampleSpace() = default;
};

class StreamedIOExampleSpace: public StreamedExampleSpace, public IOExampleSpace {
public:
    ProgramList inp_list;
    PProgram oup;
    StreamedIOExampleSpace(const PProgram& cons_program, const std::string& func_name, const ProgramList& _inp_list,
            const PProgram& _oup, const PExampleGenerator& _g, Env* _env);
    virtual bool satisfyExample(const FunctionContext& info, const Example& example);
    virtual IOExample getIOExample(const Example& example);
    virtual ~StreamedIOExampleSpace() = default;
};

typedef std::shared_ptr<StreamedExampleSpace> PStreamedExampleSpace;

namespace example {
    extern const std::string KExampleGenerateTimeOutName;
}

#endif //ISTOOL_STREAMED_EXAMPLE_SPACE_H
