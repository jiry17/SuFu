//
// Created by pro on 2022/9/27.
//

#ifndef ISTOOL_INCRE_PLP_H
#define ISTOOL_INCRE_PLP_H

#include "istool/basic/grammar.h"
#include "istool/basic/example_space.h"
#include "istool/incre/analysis/incre_instru_runtime.h"
#include "istool/incre/analysis/incre_instru_info.h"

namespace incre::autolifter {
    typedef std::pair<PType, PProgram> TypedProgram;
    typedef std::vector<TypedProgram> TypedProgramList;
    typedef std::pair<TypedProgram, TypedProgram> AuxProgram;
    typedef std::vector<AuxProgram> PLPRes;

    class IncreIOExample {
    public:
        DataList local_input, global_input, full_input;
        Data oup;
        IncreIOExample(const DataList& _local, const DataList& _global, const Data& _oup);
        DataList getAuxInput(const Data& compress);
        std::string toString() const;
    };


    class FExampleSpace {
        void addExample();
        Data runCompress(int example_id, Program* prog);
        Data runAux(int example_id, const Data& content, Program* prog);

        // cache
        std::unordered_map<std::string, DataList*> aux_cache, oup_cache;
    public:
        // cache util
        void extendAuxCache(const AuxProgram& program, DataList* cache_item, int length);
        void extendOupCache(const PProgram& program, const std::vector<int>& path, DataList* cache_item, int length);
        DataList* getAuxCache(const AuxProgram& program, int length);
        DataList* getOupCache(const PProgram& program, const std::vector<int>& path, int length);
        void registerAuxCache(const AuxProgram& program, const DataList& oup_list);
        void registerOupCache(const PProgram& program, const std::vector<int>& path, const DataList& oup_list);

        std::vector<std::pair<std::string, PType>> value_list, global_input_list;
        std::vector<IncreIOExample> example_list;
        Env* env;
        IncreExamplePool* pool;
        int current_example_id;

        int tau_id;
        FExampleSpace(IncreExamplePool* _pool, int _tau_id, const PEnv& _env, AlignTypeInfoData* pass_info);
        void switchTo(int example_id);

        Data runAux(int example_id, const AuxProgram& aux);
        std::string example2String(const IOExample& example);
        std::string example2String(int id);
        Data runOup(int example_id, Program* program, const std::vector<int>& path);

        int acquireExample(int target_num, TimeGuard* guard);
    };

    class GrammarEnumerateTool {
        void extend();
    public:
        Grammar* grammar;
        std::vector<TypedProgramList> program_pool;
        int size_limit;
        TypedProgramList* acquirePrograms(int target_size);
        GrammarEnumerateTool(Grammar* _grammar);
        ~GrammarEnumerateTool();
    };

    class PLPTask {
    public:
        FExampleSpace* example_space;
        std::vector<GrammarEnumerateTool*> aux_grammar_list;
        GrammarEnumerateTool* compress_grammar;
        std::vector<TypedProgramList> pre_res_list;
        TypedProgram target;
        int oup_compress_id;
        std::vector<int> path;
        DataList* oup_cache;

        Data runInp(int example_id, const AuxProgram& program);
        void extendOupCache(int length);
        Data runOup(int example_id);
        IOExample getIO(int example_id, const std::vector<AuxProgram>& aux_list);
        int acquireExample(int target_num, int timeout);

        PLPTask(FExampleSpace* _example_space, const std::vector<GrammarEnumerateTool*>& _aux_grammar_list,
                const std::vector<TypedProgramList>& _pre_res,
                GrammarEnumerateTool* _compress_grammar, const TypedProgram& _target, const std::vector<int>& _path, int _oup_compress_id);
    };

    Data eliminateCompress(const Data& data);
    Data openLabeledCompress(const Data& data, int label);
    std::string aux2String(const AuxProgram& program);
}

#endif //ISTOOL_INCRE_PLP_H
