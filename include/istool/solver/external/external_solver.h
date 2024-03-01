//
// Created by pro on 2022/5/24.
//

#ifndef ISTOOL_EXTERNAL_SOLVER_H
#define ISTOOL_EXTERNAL_SOLVER_H

#include "istool/solver/solver.h"

class ExternalSolver {
public:
    std::string install_path;
    Env* env;
    const std::string solver_name;
    virtual void init();
    virtual FunctionContext invoke(const std::string& benchmark_file, TimeGuard* guard) = 0;
    ExternalSolver(Env* _env, const std::string& _solver_name, const std::string& _install_path);
};

class ExternalSyGuSPBESolver: public PBESolver {
    void printSyGuSFile(const std::vector<Example>& example_list, const std::string& file_name);
public:
    ExternalSolver* solver;
    IOExampleSpace* io_space;
    bool is_new_style;
    ExternalSyGuSPBESolver(Specification* spec, ExternalSolver* solver, bool is_new_style);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    ~ExternalSyGuSPBESolver();
};

namespace solver::external {
    extern const std::string KExternalMemoryLimitName;
    // time_limit: seconds, memory_limit: GBs
    void runCommand(const std::string& command, const std::string& pre_command="", int time_limit = -1, int memory_limit = -1);
    std::string createRandomFile(const std::string& suffix);
    void setExternalMemoryLimit(Env* env, int limit);
    int getExternalMemoryLimit(Env* env);
}

#endif //ISTOOL_EXTERNAL_SOLVER_H
