//
// Created by pro on 2022/2/23.
//

#ifndef ISTOOL_DEEPCODER_DSL_H
#define ISTOOL_DEEPCODER_DSL_H

#include "istool/basic/env.h"
#include "istool/basic/grammar.h"

namespace dsl::deepcoder {
    class DeepCoderGrammarInfo {
    public:
        PType oup_type;
        TypeList param_list;
        std::vector<PSemantics> extra_semantics;
        DeepCoderGrammarInfo(const TypeList& _param_list, const PType& _oup_type={}, const std::vector<PSemantics>& _extra_semantics={});
    };
    Grammar* getDefaultDeepCoderGrammar(Env* env, const DeepCoderGrammarInfo& info, bool is_remove_empty=true);
    void prepareEnv(Env* env);
}

#endif //ISTOOL_DEEPCODER_DSL_H
