//
// Created by pro on 2023/7/26.
//

#ifndef ISTOOL_INCRE_NONSCALAR_OC_H
#define ISTOOL_INCRE_NONSCALAR_OC_H

#include "incre_nonscalar_autolifter.h"
#include "istool/solver/autolifter/composed_sf_solver.h"
#include <set>

namespace incre::autolifter {

    typedef std::vector<std::unordered_set<int>> IndexStorage;

    struct DSEnumerateInfoData {
    public:
        Bitset full, covered;
        std::vector<int> index_list;
        IndexStorage offer_list, require_list;
        DSEnumerateInfoData(const Bitset& _full, const Bitset& _covered, const std::vector<int>& _index_list,
                            const IndexStorage& _offer_list, const IndexStorage& _require_list);
    };
    typedef std::shared_ptr<DSEnumerateInfoData> DSEnumerateInfo;

    struct ClosedEnumerateInfoData {
    public:
        Bitset full, covered;
        bool is_include_start;
        std::vector<int> index_list;
        int flag;
        ClosedEnumerateInfoData(const Bitset& _full, const Bitset& _covered, bool _is_include_start, const std::vector<int>& _index_list, int flag = 0);
    };
    typedef std::shared_ptr<ClosedEnumerateInfoData> ClosedEnumerateInfo;

    struct EnumerateInfoMaximumList {
    public:
        std::vector<ClosedEnumerateInfo> info_list;
        bool isCovered(const ClosedEnumerateInfo& info) const;
        void insert(const ClosedEnumerateInfo& info);
        ClosedEnumerateInfo merge(const ClosedEnumerateInfo& info) const;
        EnumerateInfoMaximumList() = default;
    };

    typedef std::pair<std::string, std::unordered_set<std::string>> OutputUnfoldInfo;

    class OCRunner {
    public:
        IncreInfo* info;
        IncreExamplePool* pool;
        Env* env;
        int KInputUnfoldDepth, KOutputUnfoldDepth;
        std::vector<std::vector<std::string>> local_names;

        OCRunner(IncreInfo* _info, Env* _env, int _KInputUnfoldDepth, int _KOutputUnfoldDepth);
        InputUnfoldInfo runInp(int align_id, int example_id, const TypedProgramList& program_list);
        InputUnfoldInfo runOriginalInp(int align_id, int example_id);
        InputUnfoldInfo runNewInp(int align_id, int example_id, int compress_id, const PProgram& program);
        OutputUnfoldInfo runOup(int align_id, int example_id, const TypedProgramList& program_list);
        OutputUnfoldInfo runOriginalOup(int align_id, int example_id);
        OutputUnfoldInfo runNewOup(int align_id, int example_id, int compress_id, const PProgram& program);
        OutputUnfoldInfo runOup(int align_id, int example_id, int compress_id, const PProgram& program);
    };

    struct ComponentInfo {
        int compress_id;
        PType type;
        PProgram program;
        DSEnumerateInfo info;
        ComponentInfo(int _compress_id, const PType& type, const PProgram& _program, const DSEnumerateInfo& _info);
    };

    class ClosedEnumerateInfoGenerator {
    public:
        OCRunner* runner;
        std::vector<std::unordered_map<std::string, int>> index_manager;
        int getIndex(int example_id, const std::string& name);

        std::vector<GrammarEnumerateTool*> enumerate_tool_list;
        int enumerate_size = 0;
        int KComposeNum;
        std::vector<FullPairExample> scalar_example_list, ds_example_list;
        std::vector<ComponentInfo> component_info_list;

        std::unordered_map<std::string, std::vector<DSEnumerateInfo>> state_require_map, component_offer_map;
        bool isPrunedOff(const DSEnumerateInfo& info);
        bool isValidInsert(const DSEnumerateInfo & base, const DSEnumerateInfo & extra);
        bool isValidGroup(const DSEnumerateInfo& current);
        std::vector<DSEnumerateInfo> getOffer(const DSEnumerateInfo& info);
        std::vector<DSEnumerateInfo> getRequire(const DSEnumerateInfo& info);

        std::vector<DSEnumerateInfo> closed_list;
        std::vector<std::vector<DSEnumerateInfo>> close_merge_storage;
        std::unordered_set<std::string> result_cache;
        int component_index = 0, closed_index = 0;

        void acquireMoreComponent();
        void insertClosedInfo(const DSEnumerateInfo& info);
        void insertComponent(int compress_id, const TypedProgram& program);
        void updateComponentWithDSExample(const ComponentInfo& info, int example_id);
        void updateComponentWithScalarExample(const ComponentInfo& info, int example_id);
        virtual void addCounterExample(const FullPairExample& example);
        ClosedEnumerateInfo getNextEnumerateInfo();
        void addNextComponent();
        TypedProgramList index2ProgramList(const std::vector<int>& index_list);

        ClosedEnumerateInfoGenerator(IncreInfo* _info, OCRunner* _runner);
    };

    class OCNonScalarAlignSolver: public NonScalarAlignSolver {
    public:
        OCRunner* runner;
        int KComposeNum;
        ClosedEnumerateInfoGenerator generator;
        std::vector<ClosedEnumerateInfo> group_list;

        std::vector<std::vector<ClosedEnumerateInfo>> working_storage;
        std::vector<EnumerateInfoMaximumList> size_grouped_list;
        std::unordered_set<std::string> considered_set;
        std::vector<std::pair<int, int>> progress_list;

        virtual void addCounterExample(const FullPairExample& example);
        virtual TypedProgramList synthesisFromExample();
        virtual FullPairExample verify(const TypedProgramList& program_list);
        ClosedEnumerateInfo tryMerge(const ClosedEnumerateInfo& current, int remain_size);
        OCNonScalarAlignSolver(IncreInfo* _info, OCRunner* _runner);
    };

    ClosedEnumerateInfo mergeInfo(const ClosedEnumerateInfo & x, const ClosedEnumerateInfo& y);
    DSEnumerateInfo mergeDSInfo(const DSEnumerateInfo& x, const DSEnumerateInfo& y);
    bool isCovered(const ClosedEnumerateInfo& x, const ClosedEnumerateInfo& y);
    bool isFull(const ClosedEnumerateInfo& x, const ClosedEnumerateInfo& y);
    OutputUnfoldInfo unfoldOutput(const Data& data, int depth);
}

#endif //ISTOOL_INCRE_NONSCALAR_OC_H
