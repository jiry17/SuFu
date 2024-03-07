//
// Created by pro on 2022/1/20.
//

#ifndef ISTOOL_PAIRWISE_EXAMPLE_SPACE_H
#define ISTOOL_PAIRWISE_EXAMPLE_SPACE_H

#include "istool/basic/program.h"
#include "istool/basic/specification.h"
#include "streamed_example_space.h"

class CombinatorGrammarBuilder {
public:
    virtual Grammar* buildGrammar(Program* p, const TypeList& inp_list) = 0;
    virtual ~CombinatorGrammarBuilder() = default;
};

class LiftingCache {
public:
    std::unordered_map<std::string, int> program_map;
    std::vector<DataList*> cache;
    DataList* registerProgram(Program* program, const DataList& res);
    DataList* getCache(Program* program);
    ~LiftingCache();
};

class LiftingModInfo {
public:
    PProgram m;
    PType F;
    PStreamedExampleSpace example_space;
    LiftingCache mod_cache, fmap_cache;
    Env* env;
    CombinatorGrammarBuilder* c_builder;

    Data getModResult(Program* p, int id);
    Data getFMapResult(Program* p, int id);

    DataList* getModCache(Program* p);
    DataList* registerModCache(Program* p, const DataList& res);
    int extendModCache(Program* p, DataList* cache, int target);

    DataList* getFMapCache(Program* p);
    DataList* registerFMapCache(Program* p, const DataList& res);
    int extendFMapCache(Program* p, DataList* cache, int target);

    LiftingModInfo(const PProgram& _m, const PType& _F, const PStreamedExampleSpace& _example_space, CombinatorGrammarBuilder* _c_builder);
    ~LiftingModInfo();

    void print() const;
};

typedef std::shared_ptr<LiftingModInfo> PLiftingModInfo;

class LiftingTask {
public:
    std::vector<PLiftingModInfo> info_list;
    PProgram p, h;
    PSynthInfo f_info;
    PEnv env;
    ~LiftingTask() = default;
    LiftingTask(const std::vector<PLiftingModInfo>& _info_list, const PProgram& _p, const PProgram& _h, const PSynthInfo& _f_info, const PEnv& _env);
};

class PartialLiftingTask {
public:
    PLiftingModInfo info;
    PProgram p, h;
    PSynthInfo f_info;
    PEnv env;
    PartialLiftingTask(const PLiftingModInfo& _info, const PProgram& _p, const PProgram& _h, const PSynthInfo& _f_info, const PEnv& _env);
    void print() const;
    ~PartialLiftingTask() = default;
};

namespace solver::autolifter {
    PProgram rewriteCombinator(Type* F, const PProgram& pre_h, const PProgram& pre_f, const PProgram& pre_c,
            const PProgram& new_h, const PProgram& new_f);
    PType getCInputType(const PType& F, Program* h, Program* f, Env* env);
}

#endif //ISTOOL_PAIRWISE_EXAMPLE_SPACE_H
