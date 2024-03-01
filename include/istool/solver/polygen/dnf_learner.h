//
// Created by pro on 2022/1/7.
//

#ifndef ISTOOL_DNF_LEARNER_H
#define ISTOOL_DNF_LEARNER_H

#include "istool/solver/solver.h"
#include "istool/basic/bitset.h"
#include <unordered_set>
#include <set>

namespace polygen {
    struct CmpInfo {
        PProgram cmp;
        Bitset P, N;
        CmpInfo(const PProgram& _cmp, const Bitset &_P, const Bitset& _N);
        std::string toString() const;
    };

    typedef std::shared_ptr<CmpInfo> PCmpInfo;

    struct CmpPlan {
        std::vector<PCmpInfo> cmp_list;
        Bitset rem_example;
        CmpPlan(const std::vector<PCmpInfo>& _cmp_list, const Bitset& _rem);
        CmpPlan() = default;
        int operator < (const CmpPlan& plan) const;
        void print() const;
    };

    struct ClausePlan {
        std::vector<int> cmp_id_list;
        Bitset P, N;
        ClausePlan(const std::vector<int>& _list, const Bitset& _P, const Bitset& _N);
        bool cover(const ClausePlan& plan) const;
    };

}

class DNFLearner: public PBESolver {
    // Parameters for enumerating possible predicates
    double KRelaxTimeLimit;
    bool KIsAllowError;
    int size_limit = 0;
    ProgramStorage pred_pool;

    // Parameters for generating exmaple-related predicates.
    int pred_pos;
    std::unordered_set<std::string> pred_list_set;
    std::vector<std::vector<polygen::PCmpInfo>> info_storage;

    // Others
    ExampleList positive_list, negative_list;
    TimeGuard* guard;
    IOExampleSpace* io_space;
    int KMaxClauseNum;
    polygen::PCmpInfo buildInfo(const PProgram& program);
    void relax();
    std::vector<polygen::PCmpInfo> getNextInfoList();
    void clear();

    std::set<polygen::CmpPlan> visited_plan;
    polygen::PCmpInfo buildSimplifiedInfo(const polygen::ClausePlan& plan, const std::vector<polygen::PCmpInfo> &info_list);
    std::vector<polygen::ClausePlan> getAllClause(int l, int r, const std::vector<polygen::PCmpInfo>& info_list,
            const Bitset& rem_list, int lim);
    PProgram searchForCondition(const polygen::CmpPlan& plan, const std::vector<polygen::PCmpInfo>& info_list, int rem_num);
    PProgram searchForCondition(const std::vector<polygen::PCmpInfo>& info_list, int rem_num);
public:
    PSynthInfo pred_info;
    DNFLearner(Specification* spec);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    ~DNFLearner();
};

namespace solver {
    namespace polygen {
        extern const std::string KMaxClauseNumName;
        extern const std::string KIsAllowErrorName;
    }
}

#endif //ISTOOL_DNF_LEARNER_H
