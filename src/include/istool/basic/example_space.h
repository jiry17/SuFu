//
// Created by pro on 2021/12/4.
//

#ifndef ISTOOL_EXAMPLE_SPACE_H
#define ISTOOL_EXAMPLE_SPACE_H

#include "program.h"
#include "env.h"

// TODO: Split example space and oracle

typedef DataList Example;
typedef std::vector<DataList> ExampleList;
typedef std::pair<Example, Data> IOExample;
typedef std::vector<IOExample> IOExampleList;

class ExampleSpace {
public:
    PProgram cons_program;
    Env* env;
    ExampleSpace(const PProgram& _cons_program, Env* _env);
    virtual bool satisfyExample(const FunctionContext& info, const Example& example);
    virtual ~ExampleSpace() = default;
};

typedef std::shared_ptr<ExampleSpace> PExampleSpace;

class IOExampleSpace {
public:
    std::string func_name;
    IOExampleSpace(const std::string& _func_name);
    virtual IOExample getIOExample(const Example& example) = 0;
    virtual DataList getInput(const Example& example);
    virtual ~IOExampleSpace() = default;
};

class FiniteExampleSpace: public ExampleSpace {
public:
    ExampleList example_space;
    void removeDuplicate();
    FiniteExampleSpace(const PProgram& _cons_program, const ExampleList& _example_space, Env* env);
    virtual ~FiniteExampleSpace() = default;
};

class FiniteIOExampleSpace: public FiniteExampleSpace, public IOExampleSpace {
public:
    ProgramList inp_list;
    PProgram oup;
    FiniteIOExampleSpace(const PProgram& _cons_program, const ExampleList& _example_space, const std::string& _name,
            const ProgramList& _inp_list, const PProgram& _oup, Env* env);
    virtual bool satisfyExample(const FunctionContext& info, const Example& example);
    virtual IOExample getIOExample(const Example& example);
    virtual ~FiniteIOExampleSpace() = default;
};

namespace example {
    std::shared_ptr<FiniteIOExampleSpace> buildFiniteIOExampleSpace(const IOExampleList& examples, const std::string& name, Env* env, const TypeList& inp_types = {});
    bool satisfyIOExample(Program* program, const IOExample& example, Env* env);
    std::string ioExample2String(const IOExample& example);
    Example ioExample2Example(const IOExample& example);
}


#endif //ISTOOL_EXAMPLE_SPACE_H
