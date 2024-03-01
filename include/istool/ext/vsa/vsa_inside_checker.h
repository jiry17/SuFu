//
// Created by pro on 2022/5/22.
//

#ifndef ISTOOL_VSA_INSIDE_CHECKER_H
#define ISTOOL_VSA_INSIDE_CHECKER_H

#include "istool/basic/program.h"
#include "istool/ext/vsa/vsa_extension.h"

class ProgramInsideVSAChecker: public ProgramChecker {
    DataList getOutput(Program* program);
    WitnessList getWitness(Semantics* sem, const Data& oup, int id);
public:
    IOExampleList io_example_list;
    std::unordered_map<std::string, DataList> oup_cache;
    std::unordered_map<std::string, WitnessList> wit_cache;
    std::unordered_map<std::string, bool> cache;
    VSAExtension* ext;
    Env* env;
    Grammar* g;
    ProgramInsideVSAChecker(Env* _env, Grammar* g, ExampleSpace* example_space);
    bool isValid(Program* program);
    virtual ~ProgramInsideVSAChecker() = default;
};

#endif //ISTOOL_VSA_INSIDE_CHECKER_H
