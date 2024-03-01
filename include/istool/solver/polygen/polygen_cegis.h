//
// Created by pro on 2022/1/8.
//

#ifndef ISTOOL_POLYGEN_CEGIS_H
#define ISTOOL_POLYGEN_CEGIS_H

#include "polygen_term_solver.h"
#include "polygen_condition_solver.h"

class CEGISPolyGen: public VerifiedSolver {
    TermSolver* term_solver;
    PolyGenConditionSolver* cond_solver;
    IOExampleSpace* io_space;
public:
    CEGISPolyGen(Specification* spec, TermSolver* _term_solver, PolyGenConditionSolver* _condition_solver, Verifier* _v);
    ~CEGISPolyGen();
    virtual FunctionContext synthesis(TimeGuard* guard = nullptr);
};

class StagedCEGISPolyGen: public Solver {
    TermSolver* term_solver;
    PolyGenConditionSolver* cond_solver;
    IOExampleList example_list;
    int verify_pos;
    int verifyTerms(const ProgramList& term_list);
    int verifyProgram(const PProgram& program);
    ProgramList synthesisTerms(TimeGuard* guard);
    PProgram unify(const ProgramList& term_list, TimeGuard* guard);
public:
    StagedCEGISPolyGen(Specification* spec, TermSolver* _term_solver, PolyGenConditionSolver* _cond_solver);
    ~StagedCEGISPolyGen();
    virtual FunctionContext synthesis(TimeGuard* guard = nullptr);
};

namespace solver::polygen {
    PProgram constructDecisionList(const ProgramList& term_list, const ProgramList& cond_list);
    ProgramList updateConditionList(const ProgramList& term_list, const ProgramList& cond_list, const IOExampleList& examples, PolyGenConditionSolver* solver, TimeGuard* guard);
}

#endif //ISTOOL_POLYGEN_CEGIS_H
