//
// Created by pro on 2022/1/5.
//

#ifndef ISTOOL_POLYGEN_TERM_SOLVER_H
#define ISTOOL_POLYGEN_TERM_SOLVER_H

#include "istool/solver/stun/stun.h"
#include "istool/basic/bitset.h"
#include <unordered_map>
#include <map>
#include <set>

namespace polygen {
    struct AssignmentInfo {
        FunctionContext term;
        Bitset P; // satisfied example

        AssignmentInfo(const FunctionContext &_term);
        void update(const ExampleList &example_list, ExampleSpace *example_space);
    };

    struct SampleInfo {
        int status;
        std::vector<int> example_list;
        AssignmentInfo* result;
        SampleInfo(const std::vector<int>& _example_list);
        void print() const;
    };

    typedef std::shared_ptr<SampleInfo> PSampleInfo;

    struct TermPlan {
        int n; // number of terms
        std::vector<AssignmentInfo*> term_list;
        std::vector<int> rem_example;
        Bitset info;
        std::vector<PSampleInfo> sample_list;

        TermPlan(int _n, const std::vector<AssignmentInfo*>& _term_list);
        bool checkCover(SampleInfo* sample);
        PSampleInfo generateSampleInfoWithUpperBound(int lim); // index of all examples must be smaller than lim
        PSampleInfo generateSampleInfoWithLowerBound(int lim); // must include an example with index no smaller than lim
    };

    struct TermPlanCmp {
        bool operator ()(TermPlan* p1, TermPlan* p2) const;
    };

    struct TermSolverCache {
        std::unordered_map<std::string, AssignmentInfo*> info_map;
        std::map<std::vector<int>, polygen::AssignmentInfo*> solved_sample;
        std::set<polygen::TermPlan*, polygen::TermPlanCmp> plan_set;
        TermPlan* buildTermPlan(int n, const std::vector<AssignmentInfo*>& term_list);
        ~TermSolverCache();
    };
}

class EnumeratePolyGenTermSolver: public TermSolver {
public:
    std::vector<polygen::AssignmentInfo> info_pool;
    IOExampleSpace* example_space;
    IOExampleList previous_example_list;

    double KRelaxTimeOut, KRelaxFactor;
    std::vector<int> term_num_list;
    void relax(TimeGuard* guard);
    int KMaxTermNum;

    void updateInfo();
    void updateExamples(const ExampleList& example_list);
    std::vector<int> getUsefulTermIndex(const std::vector<int>& current, const Bitset& B);
    std::vector<int> search(int num, const std::vector<int>& index_list, const Bitset& full, int remain_num, std::unordered_map<std::string, std::vector<int>>& cache, TimeGuard* guard);

    ProgramList synthesisTerms(const ExampleList& example_list, TimeGuard* guard = nullptr);
    EnumeratePolyGenTermSolver(Specification* spec, const PSynthInfo& info);
};

class PolyGenTermSolver: public TermSolver {
    // aux functions
    std::vector<polygen::TermSolverCache*> cache;
    std::set<polygen::TermPlan*> visited_plan;
    TimeGuard* guard = nullptr;
    Specification* term_spec;
    ExampleList example_list;
    int solver_id;
    polygen::AssignmentInfo* buildAssignmentInfo(const FunctionContext& term);
    void performSample(polygen::SampleInfo* sample);
    void extendStart(polygen::TermPlan* plan, int sample_num);
    int calculateRandomTime(int branch_num, int example_num);
    void extendPlan(polygen::TermPlan* plan, polygen::AssignmentInfo* info, polygen::TermPlan* father,  int sample_num);
    polygen::TermPlan* buildTermPlan(polygen::TermPlan* father, polygen::AssignmentInfo* info, int sample_num);
    std::vector<polygen::AssignmentInfo*> getNextAssignment(polygen::TermPlan* plan, int n, int rem_branch);
    bool search(polygen::TermPlan* plan, ProgramList& result, int n, int rem_branch);
    ProgramList getTerms(int n, int k);
    ProgramList getTerms();

    int KMaxTermNum, KMaxExampleNum, KRandomFactor;
    std::vector<PBESolver*> domain_solver_list;
public:
    ProgramList synthesisTerms(const ExampleList& new_example_list, TimeGuard* guard = nullptr);
    PolyGenTermSolver(Specification* spec, const PSynthInfo& info, const PBESolverBuilder& builder);
    ~PolyGenTermSolver();
};

namespace solver {
    namespace polygen {
        extern const std::string KMaxTermNumName;
        extern const std::string KMaxExampleNumName;
        extern const std::string KRandomFactorName;
    }
}

#endif //ISTOOL_POLYGEN_TERM_SOLVER_H
