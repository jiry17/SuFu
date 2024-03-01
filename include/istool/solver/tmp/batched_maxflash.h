//
// Created by pro on 2023/1/3.
//

#ifndef ISTOOL_BATCHED_MAXFLASH_H
#define ISTOOL_BATCHED_MAXFLASH_H

#include "istool/basic/program.h"
#include "istool/basic/semantics.h"
#include "istool/ext/vsa/witness_value.h"
#include "istool/solver/solver.h"
#include <queue>
#include <istool/ext/vsa/vsa_extension.h>
#include "istool/solver/maxflash/topdown_context_graph.h"

namespace tmp {
    class BatchedMaxFlashNode;

    struct ComposeState {
        std::vector<int> id_list;
        double prob;
        int pos;
        ComposeState(const std::vector<int>& _id_list, double _prob, int _pos);
    };

    struct ComposeStateCmp {
        bool operator()(ComposeState* x, ComposeState* y) {
            return x->prob > y->prob;
        }
    };

    typedef std::pair<PProgram, double> ResultUnit;

    class BatchedMaxFlashEdge {
    public:
        PSemantics semantics;
        std::vector<BatchedMaxFlashNode*> node_list;
        double weight, lower_bound;
        std::vector<ResultUnit> result;
        std::priority_queue<ComposeState*, std::vector<ComposeState*>, ComposeStateCmp> Q;

        double updateLowerBound();
        void extendTopState();
        ResultUnit constructProgram();
        void updateTopState();
        bool updateComposeState(ComposeState* state);
        BatchedMaxFlashEdge(const std::vector<BatchedMaxFlashNode*> &_node_list, const PSemantics &_semantics, double _weight);
    };

    class BatchedMaxFlashNode {
    public:
        std::string oup_string, name;
        int node_id;
        std::vector<BatchedMaxFlashEdge> edge_list;
        bool is_build_edge;
        double lower_bound;
        std::vector<ResultUnit> result;

        BatchedMaxFlashNode(int _node_id, double _lower_bound, const std::string &oup_string);
        std::string toString() const;
        virtual ~BatchedMaxFlashNode() = default;
        double getLowerBound(int rank);
        virtual void updateLowerBound() = 0;
        virtual void insert(const ResultUnit& unit) = 0;
    };

    class BatchedSingleMaxFlashNode : public BatchedMaxFlashNode {
    public:
        WitnessData oup;
        BatchedSingleMaxFlashNode(int _node_id, double _lower_bound, const WitnessData &_oup);
        virtual ~BatchedSingleMaxFlashNode() = default;
        virtual void updateLowerBound();
        virtual void insert(const ResultUnit& unit);
    };

    class BatchedMultiMaxFlashNode : public BatchedMaxFlashNode {
        void getValidResultsFromR();
    public:
        BatchedMaxFlashNode *l;
        BatchedSingleMaxFlashNode *r;
        ExecuteInfo* info;
        int l_pos;

        BatchedMultiMaxFlashNode(int _node_id, BatchedMaxFlashNode *_l, BatchedSingleMaxFlashNode *_r, ExecuteInfo* _info);
        virtual ~BatchedMultiMaxFlashNode() = default;
        virtual void updateLowerBound();
        virtual void insert(const ResultUnit& unit);
    };

    class BatchedMaxFlash: public VerifiedSolver {
        IOExampleList example_list;
        Grammar* grammar;
        IOExampleSpace* example_space;
        std::string target_name;
        std::vector<ExecuteInfo*> info_list;
        std::vector<std::unordered_map<std::string, BatchedMultiMaxFlashNode*>> multi_node_map;
        std::vector<std::unordered_map<std::string, BatchedSingleMaxFlashNode*>> single_node_map;
        VSAEnvSetter setter;
        VSAExtension* ext;
        int env_id;

        void addExample(const IOExample& example);
        BatchedMaxFlashNode* initNode(int state, const WitnessTerm& oup, int example_id);
        void buildEdge(BatchedMaxFlashNode* node, int example_id);
        bool searchUpTo(BatchedMaxFlashNode* node, int example_id, double upper_bound, int rank);
        bool search(BatchedMaxFlashNode* node, int example_id, double upper_bound, int rank);
        PProgram synthesisWithExamples(int rank);
        void prepareEnv(int example_id);
    public:
        TopDownContextGraph* graph;
        double limit;
        BatchedMaxFlash(Specification* spec, Verifier* v, TopDownModel* _graph, const VSAEnvSetter& setter);
        virtual FunctionContext synthesis(TimeGuard* guard = nullptr);
        ProgramList batchedSynthesis(int target_num, TimeGuard* guard = nullptr);
    };
}


#endif //ISTOOL_BATCHED_MAXFLASH_H
