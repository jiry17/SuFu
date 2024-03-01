//
// Created by pro on 2022/1/3.
//

#ifndef ISTOOL_STUN_H
#define ISTOOL_STUN_H

#include "istool/solver/solver.h"

class TermSolver {
public:
    Specification* spec;
    PSynthInfo term_info;
    TermSolver(Specification* _spec, const PSynthInfo& info);
    virtual ProgramList synthesisTerms(const ExampleList& example_list, TimeGuard* guard = nullptr) = 0;
    virtual ~TermSolver() = default;
};
typedef std::function<TermSolver*(Specification*, const PSynthInfo& info)> TermSolverBuilder;

class Unifier {
public:
    Specification* spec;
    PSynthInfo unify_info;
    Unifier(Specification* _spec, const PSynthInfo& info);
    virtual PProgram unify(const ProgramList& term_list, const ExampleList& example_list, TimeGuard* guard = nullptr) = 0;
    virtual ~Unifier() = default;
};
typedef std::function<Unifier*(Specification*, const PSynthInfo& info)> UnifierBuilder;
#define DefaultSTUNBuilder(name, spec, info) ([](Specification* spec, const PSynthInfo& info){return new name(spec, info);})

class STUNSolver: public PBESolver {
public:
    TermSolver* term_solver;
    Unifier* unifier;
    std::string func_name;
    STUNSolver(Specification* spec, const PSynthInfo& term_info, const PSynthInfo& unify_info,
            const TermSolverBuilder& term_builder, const UnifierBuilder& unifier_builder);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    virtual ~STUNSolver();
};


namespace solver {
    // Make a trivial split. Do not gaurantee the result is equivalent to the original grammar.
    // Return {nullptr, nullptr} if failed;
    std::pair<PSynthInfo, PSynthInfo> divideSpecForSTUN(const PSynthInfo& info);
    // Add a special treatment for the BV track, where if0 is used instead of ite.
    std::pair<PSynthInfo, PSynthInfo> divideSyGuSSpecForSTUN(const PSynthInfo& info, Env* env);
}


#endif //ISTOOL_STUN_H
