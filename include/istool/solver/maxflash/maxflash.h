//
// Created by pro on 2022/1/13.
//

#ifndef ISTOOL_MAXFLASH_H
#define ISTOOL_MAXFLASH_H

#include "maxflash_vsa.h"
#include "topdown_context_graph.h"
#include "istool/solver/solver.h"
#include "istool/ext/vsa/vsa_extension.h"

class MaxFlash: public VerifiedSolver {
    IOExampleList counter_example_list;
    std::vector<std::unordered_map<std::string, MaxFlashNode*>> combined_node_map, single_node_map;
    int KIterProbStep;
    VSAEnvSetter setter;
    int env_id;

    void prepareEnv(int example_id);
    void insertExample(const Example& example);
    void clear();
    // example_id > 0: including all examples in [0, example_id]
    // example_id <= 0: including a single example -example_id
    MaxFlashNode* initNode(int id, const WitnessTerm& oup_list, int example_id);
    bool getBestProgram(MaxFlashNode* node, int example_id, double upper_bound);
    void buildEdge(MaxFlashNode* node, int example_id);

public:
    VSAExtension* ext;
    IOExampleSpace* io_space;
    TopDownContextGraph* graph;
    MaxFlash(Specification* _spec, Verifier* _v, TopDownModel* model, const VSAEnvSetter& _setter);
    virtual FunctionContext synthesis(TimeGuard *guard);
    ~MaxFlash();
};

namespace solver {
    namespace maxflash {
        extern const std::string KIterProbStepName;
    }
}

#endif //ISTOOL_MAXFLASH_H
