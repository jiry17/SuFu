//
// Created by pro on 2021/12/4.
//

#ifndef ISTOOL_GRAMMAR_H
#define ISTOOL_GRAMMAR_H

#include "program.h"

class Rule;

class NonTerminal {
public:
    PType type;
    std::string name;
    std::vector<Rule*> rule_list;
    int id = 0;
    NonTerminal(const std::string& _name, const PType& _type);
    ~NonTerminal();
};

typedef std::vector<NonTerminal*> NTList;

class Rule {
public:
    NTList param_list;
    Rule(const NTList& _param_list);
    virtual PProgram buildProgram(const ProgramList& sub_list) = 0;
    virtual std::string toString() const = 0;
    virtual std::string getSemanticsName() const = 0;
    virtual Rule* clone(const NTList& new_param_list) = 0;
    virtual ~Rule() = default;
};

class ConcreteRule: public Rule {
public:
    PSemantics semantics;
    ConcreteRule(const PSemantics& _semantics, const NTList& _param_list);
    virtual int getSize() const {return 1;}
    virtual PProgram buildProgram(const ProgramList& sub_list);
    virtual std::string toString() const;
    virtual std::string getSemanticsName() const;
    virtual Rule* clone(const NTList& new_param_list);
    ~ConcreteRule() = default;
};

class Grammar {
public:
    NonTerminal* start;
    NTList symbol_list;
    Grammar(NonTerminal* _start, const NTList& _symbol_list, bool _is_remove_empty=true);
    void removeUseless();
    void indexSymbol() const;
    void print() const;
    ~Grammar();
};

namespace grammar {
    Grammar* copyGrammar(Grammar* grammar);
    Grammar* generateHeightLimitedGrammar(Grammar* grammar, int limit);
    std::string getFreeName(Grammar* grammar);
    PProgram getMinimalProgram(Grammar* grammar);
    ParamSemantics* getParamSemantics(Rule* rule);
    ConstSemantics* getConstSemantics(Rule* rule);
    bool isFinite(Grammar* grammar);
    int getMaxSize(Grammar* grammar);
}


#endif //ISTOOL_GRAMMAR_H
