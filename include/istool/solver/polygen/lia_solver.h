//
// Created by pro on 2022/1/4.
//

#ifndef ISTOOL_LIA_SOLVER_H
#define ISTOOL_LIA_SOLVER_H

#include "istool/solver/solver.h"
#include "istool/ext/z3/z3_extension.h"
#include "gurobi_c++.h"

struct LIAResult {
public:
    enum class Status {
        SUCCESS, INFEASIBLE, TIMEOUT
    } status;
    int c_val;
    std::vector<int> param_list;
    LIAResult(Status status);
    LIAResult(const std::vector<int>& _param_list, int _c_val);
    Data run(const Example& example) const;
    std::string toString() const;
};

class BaseLIASolver: public PBESolver, public IterativeSolver {
public:
    BaseLIASolver(Specification* _spec, const ProgramList& _program_list);
    virtual void* relax(TimeGuard* guard);
    virtual BaseLIASolver* clone(Specification* spec, const ProgramList& program_list) = 0;
    PProgram trivialSolve(const ExampleList& example_list);
    std::pair<IOExampleList, ProgramList> initializeExamples(const ExampleList& example_list);
    PProgram buildProgram(const LIAResult& result, const ProgramList& considered_programs, Env* env);
    IOExampleSpace* io_example_space;
    ProgramList program_list;
    PSynthInfo info;
    int KTermIntMax, KConstIntMax, KMaxCost;
    double KRelaxTimeLimit = 0.1;
    virtual ~BaseLIASolver() = default;
};

class LIASolver: public BaseLIASolver {
public:
    LIASolver(Specification* _spec, const ProgramList& _program_list);
    GRBEnv env;

    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    virtual BaseLIASolver* clone(Specification* spec, const ProgramList& program_list);

    double KGurobiTimeOut;
};

class GreedyLIASolver: public BaseLIASolver {
public:
    int KRandomTestNum;
    GreedyLIASolver(Specification* _spec, const ProgramList& _program_list);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    virtual BaseLIASolver* clone(Specification* spec, const ProgramList& program_list);
};

namespace solver {
    namespace lia {
        extern const std::string KTermIntMaxName;
        extern const std::string KConstIntMaxName;
        extern const std::string KMaxCostName;
        extern const std::string KRandomTestNumName;
        extern const std::string KDefaultGurobiTimeOutName;
        extern const std::string KIsGurobiName;
        LIAResult solveLIA(GRBEnv& env, const std::vector<IOExample>& example_list, int t_max, int c_max, int cost_limit, TimeGuard* guard = nullptr);
        PProgram adjustLIAResultIntoGrammar(const PProgram& x, Grammar* grammar);
        BaseLIASolver* getLIASolver(Specification* spec);
    }
}


#endif //ISTOOL_LIA_SOLVER_H
