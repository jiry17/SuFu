//
// Created by pro on 2022/1/22.
//

#ifndef ISTOOL_LIFTING_SOLVER_H
#define ISTOOL_LIFTING_SOLVER_H

#include "istool/basic/program.h"
#include "lifting_problem.h"

class LiftingResInfo {
public:
    PType F;
    PProgram c, m;
    LiftingResInfo(const PType& _F, const PProgram& _c, const PProgram& _m);
};

class LiftingRes {
public:
    PEnv env;
    PProgram p, h, f;
    std::vector<LiftingResInfo> info_list;
    LiftingRes(const PProgram& _p, const PProgram& _h, const PProgram& _f, const std::vector<LiftingResInfo>& _info_list, const PEnv& _env);
    std::string toString() const;
    void styledPrint() const;
};

class SingleLiftingRes {
public:
    PProgram p, h, f;
    LiftingResInfo info;
    SingleLiftingRes(const PProgram& _p, const PProgram& _h, const PProgram& _f, const LiftingResInfo& _info);
    std::string toString() const;
};

class LiftingSolver {
public:
    LiftingTask* task;
    LiftingSolver(LiftingTask* _task);
    virtual LiftingRes synthesis(TimeGuard* guard) = 0;
    virtual ~LiftingSolver() = default;
};

class SFSolver {
public:
    PartialLiftingTask* task;
    SFSolver(PartialLiftingTask* _task);

    // return (useful components in h, f)
    virtual std::pair<PProgram, PProgram> synthesis(TimeGuard* guard) = 0;
    virtual ~SFSolver() = default;
};

typedef std::function<SFSolver*(PartialLiftingTask*)> SFSolverBuilder;

#endif //ISTOOL_LIFTING_SOLVER_H
