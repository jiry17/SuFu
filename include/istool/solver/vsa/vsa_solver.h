//
// Created by pro on 2021/12/30.
//

#ifndef ISTOOL_VSA_SOLVER_H
#define ISTOOL_VSA_SOLVER_H

#include "vsa_builder.h"
#include "istool/solver/solver.h"
#include "istool/ext/vsa/top_down_model.h"

class VSAProgramSelector {
public:
    virtual PProgram select(VSANode* node) = 0;
    virtual ~VSAProgramSelector() = default;
};

class VSAMinimalProgramSelector: public VSAProgramSelector {
public:
    TopDownModel* model;
    VSAMinimalProgramSelector(TopDownModel* _model);
    virtual PProgram select(VSANode* node);
    virtual ~VSAMinimalProgramSelector();
};

class VSARandomProgramSelector: public VSAProgramSelector {
    Env* env;
public:
    VSARandomProgramSelector(Env* _env);
    virtual PProgram select(VSANode* node);
    virtual ~VSARandomProgramSelector() = default;
};

class BasicVSASolver: public PBESolver {
    VSANode* buildVSA(const ExampleList& example_list, TimeGuard* guard);
public:
    PVSABuilder builder;
    IOExampleSpace* io_space;
    VSAProgramSelector* selector;
    std::unordered_map<std::string, VSANode*> cache;
    BasicVSASolver(Specification* spec, const PVSABuilder& _builder, VSAProgramSelector* _selector);
    virtual FunctionContext synthesis(const ExampleList& example_list, TimeGuard* guard);
    virtual ~BasicVSASolver();
};

#endif //ISTOOL_VSA_SOLVER_H
