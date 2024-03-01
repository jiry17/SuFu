//
// Created by pro on 2022/2/22.
//

#ifndef ISTOOL_CLIA_DSL_H
#define ISTOOL_CLIA_DSL_H

#include "istool/basic/env.h"
#include "istool/basic/grammar.h"

namespace dsl::clia {
    class CLIAGrammarInfo {
    public:
        PType oup_type;
        TypeList param_list;
        std::vector<int> const_list;
        std::vector<PSemantics> extra_semantics;
        CLIAGrammarInfo(const TypeList& _param_list, const PType& _oup_type = {},
                const std::vector<int>& _const_list = {0, 1}, const std::vector<PSemantics>& _extra_semantics = {});
    };

    Grammar* getDefaultCLIAGrammar(Env* env, const CLIAGrammarInfo& info, bool is_remove_empty=true);
    void prepareEnv(Env* env);
}

#endif //ISTOOL_CLIA_DSL_H
