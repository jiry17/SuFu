//
// Created by pro on 2022/1/12.
//

#ifndef ISTOOL_ENUM_UTIL_H
#define ISTOOL_ENUM_UTIL_H

#include "enum.h"
#include <unordered_set>

class TrivialOptimizer: public Optimizer {
public:
    virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p);
    virtual void clear();
};

class RuleBasedOptimizer: public Optimizer {
    static const std::unordered_set<std::string> KComOpSet, KAssocOpSet;
public:
    virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p);
    virtual void clear();
};

class TrivialVerifier: public Verifier {
public:
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~TrivialVerifier() = default;
};

class OBEOptimizer: public Optimizer {
public:
    ProgramChecker* is_runnable;
    std::unordered_map<std::string, ExampleList> example_pool;
    std::unordered_set<std::string> visited_set;
    Env* env;
    OBEOptimizer(ProgramChecker* _is_runnable, const std::unordered_map<std::string, ExampleList>& _pool, Env* _env);
    virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p);
    virtual void clear();
    virtual ~OBEOptimizer();
};

// Collect the first n programs
class NumberLimitedVerifier: public Verifier {
public:
    std::vector<FunctionContext> result;
    int n;
    Verifier* v;
    NumberLimitedVerifier(int _n, Verifier* _v);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~NumberLimitedVerifier() = default;
};

// Collect all programs with size at most n
class SizeLimitedVerifier: public Verifier {
public:
    std::vector<FunctionContext> result;
    int size_limit;
    Verifier* v;
    SizeLimitedVerifier(int _size_limit, Verifier* _v);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~SizeLimitedVerifier() = default;
};

namespace solver {
    // Return whether timeout
    bool collectAccordingNum(const std::vector<PSynthInfo>& info_list, int n, std::vector<FunctionContext>& result, EnumConfig c);
    bool collectAccordingSize(const std::vector<PSynthInfo>& info_list, int size_limit, std::vector<FunctionContext>& result, EnumConfig c);
}


#endif //ISTOOL_ENUM_UTIL_H
