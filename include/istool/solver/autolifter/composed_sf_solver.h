//
// Created by pro on 2022/1/24.
//

#ifndef ISTOOL_COMPOSED_SF_SOLVER_H
#define ISTOOL_COMPOSED_SF_SOLVER_H

#include "istool/basic/bitset.h"
#include "basic/lifting_solver.h"
#include "sf_verifier.h"
#include <queue>
#include <unordered_set>

namespace solver::autolifter {
    struct EnumerateInfo {
        std::vector<int> ind_list;
        Bitset info;
        EnumerateInfo(const std::vector<int> &_ind_list);
        std::string toString() const;
        ~EnumerateInfo() = default;
    };

    struct MaximalInfoList {
        std::vector<EnumerateInfo*> info_list;
        int size;
        void clear();
        MaximalInfoList();
        bool add(EnumerateInfo* info);
        bool isExistResult(EnumerateInfo* info);
    };
}

class ComposedSFSolver: public SFSolver {
    PProgram synthesisFromH();
    int KComposedNum, KExtraTurnNum;
    bool KIsFullH;
    double KEnumerateTimeOut;
    Env* env;
    std::vector<std::pair<int, int>> example_list;
    ProgramList program_space;
    std::vector<Bitset> program_info_list;
    std::vector<std::vector<solver::autolifter::EnumerateInfo*>> info_storage;
    std::vector<solver::autolifter::MaximalInfoList> maximal_list;
    solver::autolifter::MaximalInfoList global_maximal;
    std::vector<std::queue<solver::autolifter::EnumerateInfo*>> working_list;
    std::unordered_map<std::string, solver::autolifter::EnumerateInfo*> uncovered_info_set;
    int next_component_id = 0;

    bool isSatisfyExample(Program* p, const std::pair<int, int>& example);
    void getMoreComponent(TimeGuard* guard);
    void initNewProgram(TimeGuard* guard);
    ProgramList synthesisFromExample(TimeGuard* guard);
    void addCounterExample(const std::pair<int, int>& counter_example);
    void constructMoreInfo(solver::autolifter::EnumerateInfo* info);
    bool addUncoveredInfo(solver::autolifter::EnumerateInfo* info);
    solver::autolifter::EnumerateInfo* getNextComposition(int k, TimeGuard* guard);
    ProgramList getProgramListFromInfo(solver::autolifter::EnumerateInfo* info);
    std::pair<solver::autolifter::EnumerateInfo*, solver::autolifter::EnumerateInfo*> recoverResult(
            int pos, solver::autolifter::EnumerateInfo* info);
    std::pair<solver::autolifter::EnumerateInfo*, solver::autolifter::EnumerateInfo*> constructResult(
            solver::autolifter::EnumerateInfo* info, int limit);

public:
    SFVerifier* v;
    ComposedSFSolver(PartialLiftingTask* task);
    virtual std::pair<PProgram, PProgram> synthesis(TimeGuard* guard);
    ~ComposedSFSolver();
};

namespace solver::autolifter {
    extern const std::string KComposedNumName;
    extern const std::string KIsFullHName;
    extern const std::string KExtraTurnNumName;
}

#endif //ISTOOL_COMPOSED_SF_SOLVER_H
