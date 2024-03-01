//
// Created by pro on 2022/2/21.
//

#ifndef ISTOOL_AUTOLIFTER_DSL_H
#define ISTOOL_AUTOLIFTER_DSL_H

#include "istool/solver/autolifter/basic/lifting_problem.h"

namespace dsl::autolifter {
    class LiftingModConfigInfo {
    public:
        PProgram m;
        PType F;
        std::vector<int> extra_consts;
        std::vector<PSemantics> extra_semantics;
        LiftingModConfigInfo(const PProgram& _m, const PType& _F, const std::vector<int>& _extra_consts={}, const std::vector<PSemantics>& _extra_semantics={});
        ~LiftingModConfigInfo() = default;
    };

    class LiftingConfigInfo {
    public:
        PType inp_type;
        PProgram p;
        PEnv env;
        std::vector<LiftingModConfigInfo> mod_list;
        std::vector<PSemantics> extra_semantics;
        LiftingConfigInfo(const PProgram& _p, const PType& _inp_type, const PEnv& _env,
                const std::vector<LiftingModConfigInfo>& _mod_list, const std::vector<PSemantics>& _extra_semantics={});
    };

    LiftingTask* buildLiftingTask(const LiftingConfigInfo& info);
    void prepareEnv(Env* env);
}

#endif //ISTOOL_AUTOLIFTER_DSL_H
