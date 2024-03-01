//
// Created by pro on 2021/12/22.
//

#ifndef ISTOOL_Z3_EXAMPLE_SPACE_H
#define ISTOOL_Z3_EXAMPLE_SPACE_H

#include "istool/basic/example_space.h"
#include "z3_extension.h"
#include <unordered_map>


class Z3ExampleSpace: public ExampleSpace {
public:
    Z3Extension* ext;
    TypeList type_list;
    std::unordered_map<std::string, Signature> sig_map;
    Z3ExampleSpace(const PProgram& _cons_prog, Env* _env, const TypeList& _type_list, const std::unordered_map<std::string, Signature>& sig_map);
};

class Z3IOExampleSpace: public Z3ExampleSpace, public IOExampleSpace {
    std::unordered_map<std::string, IOExample> cache;
public:
    ProgramList inp_list;
    PProgram oup_cons;
    Z3IOExampleSpace(const PProgram& _cons_prog, Env* _env, const TypeList& _type_list, const Signature& sig,
            const std::string& name, const ProgramList& _inp_list, const PProgram& _oup_cons);
    virtual IOExample getIOExample(const Example& example);
    virtual Example getInput(const Example& example);
    virtual bool satisfyExample(const FunctionContext& info, const Example& example);
};

namespace example {
    PExampleSpace buildZ3ExampleSpace(const PProgram& cons, Env* env, const TypeList& type_list, const std::unordered_map<std::string, Signature>& sig_map);
}

#endif //ISTOOL_Z3_EXAMPLE_SPACE_H
