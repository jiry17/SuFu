//
// Created by zyw on 2023/5/19.
//

#ifndef ISTOOL_INCRE_TO_HASKELL_H
#define ISTOOL_INCRE_TO_HASKELL_H

#include "istool/basic/grammar.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/incre/autolifter/incre_autolifter_solver.h"
#include "istool/incre/language/incre.h"

namespace incre {
    void tyToHaskell(const std::shared_ptr<TyData> &ty, bool in_def_ind);
    void patternToHaskell(const std::shared_ptr<PatternData> &pattern);
    void termToHaskell(const std::shared_ptr<TermData> &term, bool after_constructor, bool is_func_name);
    void termParamToHaskell(const std::shared_ptr<TermData> &term, bool first_param);
    void bindingToHaskell(const std::shared_ptr<BindingData> &binding);
    void bindingTyToHaskell(const std::shared_ptr<BindingData> &binding, const std::string &name);
    void commandToHaskell(const std::shared_ptr<CommandData> &command);
    // output SimpleMergeable of defined data type
    void outputSimpleMergeable(const std::shared_ptr<CommandData> &command);
    void preOutput();
    void outputRefEnv();
    // get env
    void envToHaskell(std::vector<std::pair<std::vector<std::string>, Grammar *> > &final_grammar,
        std::vector<std::pair<PType, int> > &env_type_list);
    // get grammar for each hole
    void grammarToHaskell(Grammar *grammar, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num);
    // get program space for each hole
    void spaceToHaskell(Grammar *grammar, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num);
    // void evalTupleToHaskell(std::vector<std::pair<std::vector<std::string>, Grammar *> >& final_grammar);
    // get eval function for each hole
    void evalToHaskell(Grammar *grammar, int func_num, std::unordered_map<std::string, int>& name_to_expr_num,
        std::vector<std::string>& param_list, std::vector<std::pair<PType, int> > &env_type_list);
    // get eval string for hole
    void getEvalString(std::vector<std::string> &eval_string_for_each_hole,
        std::vector<std::pair<std::vector<std::string>, Grammar *> >& final_grammar,
        TyList &final_type_list,
        std::vector<std::pair<PType, int> > &env_type_list);
    // output some helper functions
    void outputHelperFunc();
    bool compareInput(std::pair<Term, Data> &a, std::pair<Term, Data> &b);
    void postOutput(const std::vector<std::pair<Term, Data>> &io_pairs, int time_limit);
    // helper function for io pairs
    std::string getInputString(TmApp* tm_app);
    void programToHaskell(const std::shared_ptr<ProgramData> &prog, 
        const std::vector<std::pair<Term, Data>> &io_pairs, 
        incre::IncreInfo *info,
        const incre::IncreAutoLifterSolver *solver, const std::string &path,
        TyList &final_type_list, int time_limit);
}

#endif //ISTOOL_INCRE_TO_HASKELL_H
