//
// Created by pro on 2022/2/7.
//

#ifndef ISTOOL_DIFFERENT_PROGRAM_GENERATOR_H
#define ISTOOL_DIFFERENT_PROGRAM_GENERATOR_H

#include "istool/basic/specification.h"
#include "istool/solver/vsa/vsa_solver.h"
#include <unordered_set>

class DifferentProgramGenerator {
public:
    virtual void addExample(const IOExample& example) = 0;
    virtual ProgramList getDifferentProgram(const IOExample& inp, int num) = 0;
    virtual ~DifferentProgramGenerator() = default;
};


namespace selector::samplesy {
    class CollectRes {
    public:
        PProgram p;
        Data oup;
        CollectRes(const PProgram& _p, const Data& _oup);
    };
}

class VSADifferentProgramGenerator: public DifferentProgramGenerator {
    std::vector<std::vector<selector::samplesy::CollectRes>> res_pool;
    std::vector<VSANode*> node_list;
    VSAExtension* ext;
    void setRoot(VSANode* node);
    bool extendResPool(int size, ExecuteInfo* info);
    selector::samplesy::CollectRes getRes(const VSAEdge& edge, const std::vector<int>& id_list, ExecuteInfo* info);
    bool isValidWitness(const VSAEdge& edge, const Data& oup, const std::vector<int>& id_list, const DataList& params);
public:
    PVSABuilder builder;
    VSANode* root;
    std::unordered_set<std::string> added_example_set;
    VSADifferentProgramGenerator(const PVSABuilder& _builder);
    virtual void addExample(const IOExample& example);
    virtual ProgramList getDifferentProgram(const IOExample& example, int num);
    virtual ~VSADifferentProgramGenerator() = default;
};


#endif //ISTOOL_DIFFERENT_PROGRAM_GENERATOR_H
