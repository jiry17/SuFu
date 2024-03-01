//
// Created by pro on 2022/8/1.
//

#ifndef ISTOOL_COMPOSED_RULE_H
#define ISTOOL_COMPOSED_RULE_H

#include "istool/basic/grammar.h"

class ComposedRule: public Rule {
public:
    PProgram composed_sketch;
    ComposedRule(const PProgram& sketch, const NTList& param_list);
    virtual PProgram buildProgram(const ProgramList& sub_list);
    virtual std::string toString() const;
    virtual std::string getSemanticsName() const;
    virtual std::string toHaskell(std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num, int func_num, std::string &node_name);
    virtual std::string evalRuleToHaskell(std::string node_name, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, std::vector<std::string>& var_list,
        std::string oup_type, std::vector<std::pair<PType, int> > &env_type_list,
        std::vector<int> &tuple_len);
    virtual Rule* clone(const NTList& new_param_list);
    virtual ~ComposedRule() = default;
};

namespace ext::grammar {
    Grammar* rewriteComposedRule(Grammar* g);
}

#endif //ISTOOL_COMPOSED_RULE_H
