//
// Created by pro on 2023/7/14.
//

#ifndef ISTOOL_INCRE_EXTENDED_AUTOLIFTER_H
#define ISTOOL_INCRE_EXTENDED_AUTOLIFTER_H

#include "incre_plp.h"
#include "istool/basic/specification.h"
#include "istool/incre/incre_solver.h"

namespace incre::autolifter {
    struct FullPairExample {
    public:
        int align_id, x, y;
    };

    struct InputUnfoldInfo {
        std::unordered_set<std::string> ds_input;
        DataList scalar_input;
        std::string structure_feature;
        InputUnfoldInfo() = default;
        InputUnfoldInfo(const std::unordered_set<std::string>& _ds_input, const DataList& _scalar_input, const std::string& _feature);
    };

    class NonScalarExecutionTool {
    public:
        IncreInfo* info;
        IncreExamplePool* pool;
        Env* env;
        int KUnfoldDepth;

        NonScalarExecutionTool(IncreInfo* _info, Env* _env, int _KUnfoldDepth);
        virtual InputUnfoldInfo runInp(int align_id, int example_id, const TypedProgramList& program) = 0;
        virtual bool runOup(int align_id, int example_id, const TypedProgramList& program, const InputUnfoldInfo& inp_info, std::unordered_map<std::string, Data>& result) = 0;
        bool isValid(FullPairExample& example, const TypedProgramList& program);
        virtual ~NonScalarExecutionTool() = default;
    };

    class BasicNonScalarExecutionTool: public NonScalarExecutionTool {
    public:
        std::vector<std::vector<std::string>> local_inp_names;
        BasicNonScalarExecutionTool(IncreInfo* _info,  Env* _env, int _KUnfoldDepth);
        void prepareGlobal(int align_id, int example_id);
        virtual InputUnfoldInfo runInp(int align_id, int example_id, const TypedProgramList& program);
        virtual bool runOup(int align_id, int example_id, const TypedProgramList& program, const InputUnfoldInfo& inp_info, std::unordered_map<std::string, Data>& result);

        virtual ~BasicNonScalarExecutionTool() = default;
    };

    class NonScalarAlignSolver {
    public:
        IncreInfo* info;
        Env* env;
        void acquireExamples(int target_num);
        int KVerifyBase, KExampleTimeOut;
        int compress_num;
        std::vector<int> KVerifyNumList;
        std::pair<int, int> verify_pos;

        virtual void addCounterExample(const FullPairExample& example) = 0;
        virtual TypedProgramList synthesisFromExample() = 0;
        virtual FullPairExample verify(const TypedProgramList& res) = 0;
        TypedProgramList solve();

        NonScalarAlignSolver(IncreInfo* _info);
    };

    class BasicNonScalarSolver: public NonScalarAlignSolver {
    public:
        std::vector<FullPairExample> example_list;
        std::vector<PSynthInfo> cinfo_list;
        NonScalarExecutionTool* runner;
        FullPairExample verify(const TypedProgramList& res);
        TypedProgramList synthesisFromExample();
        virtual void addCounterExample(const FullPairExample& example);
        BasicNonScalarSolver(IncreInfo* _info, NonScalarExecutionTool* _runner);
    };


    class IncreNonScalarSolver: public IncreSolver {
    public:
        PEnv env;
        int KUnfoldDepth, KExampleTimeOut;
        TypedProgramList align_list;
        NonScalarExecutionTool* runner;
        NonScalarAlignSolver* aux_solver;
        IncreNonScalarSolver(IncreInfo* _info, const PEnv& _env, NonScalarAlignSolver* _aux_solver);
        Ty getFinalType(const Ty& type);
        virtual IncreSolution solve();
        virtual ~IncreNonScalarSolver() = default;
        Term synthesisComb(int align_id);
    };


    InputUnfoldInfo unfoldInput(const Data& data, int depth);
    void mergeInputInfo(InputUnfoldInfo& base, const InputUnfoldInfo& extra);
    bool unfoldOutput(const Data& data, const InputUnfoldInfo& info, int depth_limit, std::unordered_map<std::string, Data>& result);
    Data executeCompress(const Data& data, const TypedProgramList& program_list, Env* env);
    Data executeCompress(const Data& data, int compress_id, const PProgram& program, Env* env);
    extern const std::string KUnfoldDepthName;

    namespace comb {
        PatternList getDSScheme(const IOExampleList& example_list, int depth_limit, Env* env);
        bool isMatchUsage(const Pattern& pattern, const Data& oup, const DataList& inp);
        Term usage2Term(const Pattern& pattern, const TermList& term, const Ty& type);
    }
}

#endif //ISTOOL_INCRE_EXTENDED_AUTOLIFTER_H
