//
// Created by pro on 2022/9/26.
//

#ifndef ISTOOL_INCRE_PLP_SOLVER_H
#define ISTOOL_INCRE_PLP_SOLVER_H

#include "istool/basic/grammar.h"
#include "istool/basic/bitset.h"
#include "incre_plp.h"
#include "istool/solver/autolifter/composed_sf_solver.h"

namespace incre::autolifter {
    struct UnitInfo {
        AuxProgram program;
        Bitset info;
        bool is_error;
        UnitInfo(const AuxProgram& _program, const Bitset& _info, bool _is_error = false);
    };

    class AuxProgramEvaluateUtil {
    public:
        PLPTask* task;
        AuxProgramEvaluateUtil(PLPTask* _task);
        virtual Data execute(const AuxProgram& program, int example_id) = 0;
        virtual std::vector<AuxProgram> constructAuxProgram(const AuxProgram& program) = 0;
        virtual std::vector<AuxProgram> getDefaultAuxPrograms() = 0;
        virtual ~AuxProgramEvaluateUtil() = default;
    };

    class IncrePLPSolver {
        std::string example2String(const std::pair<int, int>& example);
        AuxProgramEvaluateUtil* evaluate_util;
        std::vector<AuxProgram> unfoldComponents(const std::vector<AuxProgram>& program_list);
    public:
        int KComposedNum, KExtraTurnNum;
        Env* env;
        PLPTask* task;
        std::vector<std::pair<int, int>> example_list;
        std::vector<int> error_example_list;

        void addExample(const std::pair<int, int>& example);
        void addErrorExample(int example_id);
        std::vector<UnitInfo> mergeUnits(int compress_size, int aux_size);

        // Used to get components
        UnitInfo init(const AuxProgram& program);
        void getMoreComponent();
        std::vector<UnitInfo> component_info_list;
        int current_size = 0;

        // Used to enumerate compositions
        std::vector<std::vector<solver::autolifter::EnumerateInfo*>> info_storage;
        std::vector<solver::autolifter::MaximalInfoList> maximal_list;
        solver::autolifter::MaximalInfoList global_maximal;
        std::vector<std::queue<solver::autolifter::EnumerateInfo*>> working_list;
        std::unordered_map<std::string, solver::autolifter::EnumerateInfo*> uncovered_info_set;
        int next_component_id = 0;

        // Used to verify
        int verify_num = 0, verify_pos = 0;
        int KVerifyBaseNum, KExampleTimeOut, KExampleEnlargeFactor;
        std::pair<int, int> verify(const std::vector<AuxProgram>& aux_list);

        // Used for synthesis
        bool addUncoveredInfo(solver::autolifter::EnumerateInfo* info);
        void constructInfo(solver::autolifter::EnumerateInfo* info);
        std::pair<solver::autolifter::EnumerateInfo*, solver::autolifter::EnumerateInfo*> recoverResult(
                int pos, solver::autolifter::EnumerateInfo* info);
        std::pair<solver::autolifter::EnumerateInfo*, solver::autolifter::EnumerateInfo*> constructResult(
                solver::autolifter::EnumerateInfo* info, int limit);
        solver::autolifter::EnumerateInfo* getNextComponent(int k, TimeGuard* guard);
        std::vector<AuxProgram> extractResultFromInfo(solver::autolifter::EnumerateInfo* info);
        std::vector<AuxProgram> synthesisFromExample(TimeGuard* guard);

    public:
        IncrePLPSolver(Env* _env, PLPTask* _task);
        ~IncrePLPSolver();
        PLPRes synthesis(TimeGuard* guard);
    };

    extern const std::string KIsMergeVarName;
    extern const std::string KIsIncludeDirectValueName;
}

#endif //ISTOOL_INCRE_PLP_SOLVER_H
