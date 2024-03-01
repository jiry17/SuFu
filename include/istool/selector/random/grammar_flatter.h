//
// Created by pro on 2022/5/2.
//

#ifndef ISTOOL_GRAMMAR_FLATTER_H
#define ISTOOL_GRAMMAR_FLATTER_H

#include "istool/basic/grammar.h"
#include "equivalence_checker_tool.h"
#include "istool/solver/maxflash/topdown_context_graph.h"

class FlattenGrammar {
protected:
    virtual TopDownGraphMatchStructure* getMatchStructure(int node_id, const PProgram& program) const = 0;
public:
    struct ParamInfo {
        PType type;
        PProgram program;
        ParamInfo(const PType& _type, const PProgram& _program);
        ParamInfo() = default;
    };
    TopDownContextGraph* graph;
    std::unordered_map<std::string, TopDownGraphMatchStructure*> match_cache;
    std::vector<ParamInfo> param_info_list;
    Env* env;
    void print() const;
    TopDownGraphMatchStructure* getMatchStructure(const PProgram& program);
    Example getFlattenInput(const Example& input) const;
    FlattenGrammar(TopDownContextGraph* _graph, Env* _env);
    ~FlattenGrammar();
};

// todo: fix the memory leak leaded by prob_model.
class FlattenGrammarBuilder {
public:
    Grammar* grammar;
    TopDownModel* model;
    FlattenGrammarBuilder(Grammar* _grammar, TopDownModel* _model);
    virtual FlattenGrammar* getFlattenGrammar(int depth) = 0;
    virtual ~FlattenGrammarBuilder() = default;
};

class TrivialFlattenGrammar: public FlattenGrammar {
protected:
    virtual TopDownGraphMatchStructure* getMatchStructure(int node_id, const PProgram& program) const;
public:
    std::unordered_map<std::string, int> param_map;
    TrivialFlattenGrammar(TopDownContextGraph* _g, Env* env, int flatten_num, ProgramChecker* validator);
    ~TrivialFlattenGrammar() = default;
};

class TrivialFlattenGrammarBuilder: public FlattenGrammarBuilder {
public:
    Env* env;
    int flatten_num;
    ProgramChecker* validator;
    TrivialFlattenGrammarBuilder(Grammar* g, TopDownModel* model, Env* _env, int _flatten_num, ProgramChecker* _validator);
    virtual FlattenGrammar* getFlattenGrammar(int depth);
    ~TrivialFlattenGrammarBuilder();
};

class MergedFlattenGrammar: public FlattenGrammar {
protected:
    virtual TopDownGraphMatchStructure* getMatchStructure(int node_id, const PProgram& program) const;
public:
    std::unordered_map<std::string, int> param_map;
    PEquivalenceCheckTool tool;
    MergedFlattenGrammar(TopDownContextGraph* _g, Env* env, int flatten_num, ProgramChecker* validator, const PEquivalenceCheckTool& tool);
    ~MergedFlattenGrammar() = default;
};

class MergedFlattenGrammarBuilder: public FlattenGrammarBuilder {
public:
    Env* env;
    int flatten_num;
    ProgramChecker* validator;
    PEquivalenceCheckTool tool;
    MergedFlattenGrammarBuilder(Grammar* g, TopDownModel* model, Env* _env, int _flatten_num,
            ProgramChecker* _validator, const PEquivalenceCheckTool& tool);
    virtual FlattenGrammar* getFlattenGrammar(int depth);
    virtual ~MergedFlattenGrammarBuilder();
};

#endif //ISTOOL_GRAMMAR_FLATTER_H
