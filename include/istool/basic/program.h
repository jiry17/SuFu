//
// Created by pro on 2021/12/4.
//

#ifndef ISTOOL_PROGRAM_H
#define ISTOOL_PROGRAM_H

#include "semantics.h"

class Program;
typedef std::shared_ptr<Program> PProgram;
typedef std::vector<PProgram> ProgramList;
typedef std::vector<ProgramList> ProgramStorage;

class Program {
public:
    PSemantics semantics;
    ProgramList sub_list;
    Program(const PSemantics& _semantics, const ProgramList& _sub_list);
    int size() const;
    Data run(ExecuteInfo* info) const;
    std::string toString() const;
    virtual ~Program() = default;
};

class ProgramChecker {
public:
    virtual bool isValid(Program* p) = 0;
    virtual ~ProgramChecker() = default;
};

class AllValidProgramChecker: public ProgramChecker {
public:
    virtual bool isValid(Program* p);
    virtual ~AllValidProgramChecker() = default;
};

typedef std::function<PProgram(const PSemantics&, const ProgramList&)> ProgramConstructor;

namespace program {
    PProgram buildParam(int id, const PType& type = nullptr);
    PProgram buildConst(const Data& w);
    PProgram programMap(Program* p, const ProgramConstructor& c);
    PProgram rewriteParam(const PProgram& p, const ProgramList& param_list);
}

#endif //ISTOOL_PROGRAM_H
