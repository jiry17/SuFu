//
// Created by pro on 2022/5/19.
//

#ifndef ISTOOL_EQUIVALENCE_CHECKER_TOOL_H
#define ISTOOL_EQUIVALENCE_CHECKER_TOOL_H

#include "istool/basic/program.h"
#include "istool/basic/example_sampler.h"
#include "istool/basic/example_space.h"
#include "istool/ext/z3/z3_extension.h"

class EquivalenceCheckTool {
public:
    virtual PProgram insertProgram(const PProgram& p) = 0;
    virtual PProgram queryProgram(const PProgram& p) = 0;
    virtual Data getConst(Program* p) = 0;
};

typedef std::shared_ptr<EquivalenceCheckTool> PEquivalenceCheckTool;

class FiniteEquivalenceCheckerTool: public EquivalenceCheckTool {
public:
    DataStorage inp_pool;
    Env* env;
    std::unordered_map<std::string, PProgram> feature_map;
    std::string getFeature(Program* p);
    FiniteEquivalenceCheckerTool(Env* _env, FiniteIOExampleSpace* fio);
    virtual PProgram insertProgram(const PProgram& p);
    virtual PProgram queryProgram(const PProgram& p);
    virtual Data getConst(Program* p);
    virtual ~FiniteEquivalenceCheckerTool() = default;
};

namespace selector::random {
    class DiffTreeNode {
    public:
        virtual ~DiffTreeNode() = default;
    };

    class DiffTreeInternalNode: public DiffTreeNode{
    public:
        Example inp;
        std::unordered_map<std::string, DiffTreeNode*> children;
        DiffTreeInternalNode(const Example& _inp);
        virtual ~DiffTreeInternalNode();
    };

    class DiffTreeLeaf: public DiffTreeNode {
    public:
        PProgram prog;
        DiffTreeLeaf(const PProgram& _prog);
        virtual ~DiffTreeLeaf() = default;
    };
}

class Z3EquivalenceCheckerTool: public EquivalenceCheckTool {
    bool checkEqual(Program* x, Program* y, Example* inp);
    selector::random::DiffTreeNode* insertProgram(selector::random::DiffTreeNode* node, const PProgram& p, PProgram& res);
public:
    Env* env;
    Z3Extension* ext;
    selector::random::DiffTreeNode* root;
    ExampleList random_example;
    z3::expr_vector param_list;
    TypeList inp_types;
    int KRandomTestNum;
    PExampleGenerator sampler;

    std::unordered_map<std::string, PProgram> query_cache;
    std::unordered_map<std::string, Data> const_cache;

    virtual Data getConst(Program* p);
    virtual PProgram insertProgram(const PProgram& p);
    virtual PProgram queryProgram(const PProgram& p);

    Z3EquivalenceCheckerTool(Env* _env, const TypeList& _inp_types, const PExampleGenerator &_sampler, int _KRandomTestNum);
    virtual ~Z3EquivalenceCheckerTool();
};

#endif //ISTOOL_EQUIVALENCE_CHECKER_TOOL_H
