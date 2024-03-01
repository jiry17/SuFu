//
// Created by zyw on 2023/5/19.
//

#include "istool/incre/io/incre_to_haskell.h"
#include "istool/basic/config.h"
#include "istool/basic/grammar.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/incre/language/incre.h"
#include "glog/logging.h"
#include <fstream>
#include <iostream>
#include <set>
#include <cstring>

using namespace incre;
bool debug_haskell = false;
int floor_num_haskell = 0;
// save all constructors, the first letter of constructor should be upper character
std::set<std::string> construct;
// some func names need to be modified because are reserved words. 
// modification method: add "'" after name
std::set<std::string> modified_func_name = {"main", "sum", "min", "max", "length", "div", "minimum", "maximum", "concat", "repeat", "head", "abs", "last", "map", "solve", "from", "product", "scanl", "merge"};
// pre_func_name is the actual func name, post_func_name is used in fix term
std::string pre_func_name, post_func_name;
int in_fix_func = 0;
std::vector<std::string> eval_string_for_each_hole;
int hole_num_now = 0;

namespace {
    bool _isNeedBracket(TermType def_type) {
        return !(def_type == incre::TermType::VALUE ||
                 def_type == incre::TermType::VAR || def_type == incre::TermType::TUPLE
                 || def_type == incre::TermType::PROJ);
    }

    void printSpace(int num) {
        while (num--) {
            std::cout << "  ";
        }
    }

    // output deriving clause for defined type
    void outputDeriving(std::string func_name) {
      printSpace(1);
      std::cout << "deriving stock (Generic, Show, Eq)" << std::endl;
      printSpace(1);
      std::cout << "deriving (Mergeable, EvaluateSym, ToCon " << func_name << ", ExtractSymbolics)" << std::endl;
      printSpace(2);
      std::cout << "via (Default " << func_name << ")" << std::endl;
    }
}

void incre::tyToHaskell(const std::shared_ptr<TyData> &ty, bool in_def_ind = false) {
    if (debug_haskell) std::cout << std::endl << "[zyw: tyToHaskell] ";
    if (ty->getType() == TyType::INT) {
        if (debug_haskell) std::cout << std::endl;
        std::cout << "SymInteger";
    } else if (ty->getType() == TyType::VAR) {
        if (debug_haskell) std::cout << "[VAR]" << std::endl;
        auto* ty_var = dynamic_cast<TyVar*>(ty.get());
        std::cout << ty_var->name;
    } else if (ty->getType() == TyType::UNIT) {
        if (debug_haskell) std::cout << std::endl;
        std::cout << "Unit";
    } else if (ty->getType() == TyType::BOOL) {
        if (debug_haskell) std::cout << std::endl;
        std::cout << "SymBool";
    } else if (ty->getType() == TyType::TUPLE) {
        if (debug_haskell) std::cout << "[TUPLE]" << std::endl;
        auto* ty_tuple = dynamic_cast<TyTuple*>(ty.get());
        bool flag = false;
        if (!in_def_ind) {
            std::cout << "(";
            for (auto& tuple_field : ty_tuple->fields) {
                if (flag) {
                    std::cout << ", ";
                    tyToHaskell(tuple_field);
                } else {
                    tyToHaskell(tuple_field);
                    flag = true;
                }
            }
            std::cout << ")";
        } else {
            for (auto& tuple_field : ty_tuple->fields) {
                if (flag) {
                    std::cout << " ";
                    tyToHaskell(tuple_field);
                } else {
                    tyToHaskell(tuple_field);
                    flag = true;
                }
            }
        }
    } else if (ty->getType() == TyType::IND) {
        if (debug_haskell) std::cout << "[IND]" << std::endl;
        auto* ty_ind = dynamic_cast<TyInductive*>(ty.get());
        std::cout << ty_ind->name << std::endl;
        bool flag = false;
        for (auto& ind_cons : ty_ind->constructors) {
            printSpace(floor_num_haskell);
            if (flag) {
                std::cout << "| ";
            } else {
                std::cout << "= ";
                flag = true;
            }
            construct.insert(ind_cons.first);
            // first char of constructors should be capitalised
            ind_cons.first[0] = std::toupper(ind_cons.first[0]);
            std::cout << ind_cons.first << " ";
            tyToHaskell(ind_cons.second, true);
            std::cout << std::endl;
        }
    } else if (ty->getType() == TyType::COMPRESS) {
        if (debug_haskell) std::cout << "[COMPRESS]" << std::endl;
        auto* ty_compress = dynamic_cast<TyCompress*>(ty.get());
        std::cout << "Compress ";
        tyToHaskell(ty_compress->content);
    } else if (ty->getType() == TyType::ARROW) {
        if (debug_haskell) std::cout << "[ARROW]" << std::endl;
        auto* ty_arrow = dynamic_cast<TyArrow*>(ty.get());
        incre::TyType source_type = ty_arrow->source->getType();
        bool need_bracket = !(source_type == incre::TyType::INT ||
                              source_type == incre::TyType::UNIT || source_type == incre::TyType::BOOL
                              || source_type == incre::TyType::VAR ||  source_type == incre::TyType::COMPRESS);
        if (need_bracket) std::cout << "(";
        tyToHaskell(ty_arrow->source);
        if (need_bracket) std::cout << ")";
        std::cout << " -> ";
        tyToHaskell(ty_arrow->target);
    } else {
        LOG(FATAL) << "Unknown Ty";
    }
}

void incre::patternToHaskell(const std::shared_ptr<PatternData> &pattern) {
    if (debug_haskell) std::cout << std::endl << "[zyw: patternToHaskell]" << std::endl;
    if (pattern->getType() == PatternType::UNDER_SCORE) {
        if (debug_haskell) std::cout << std::endl;
        std::cout << "_";
    } else if (pattern->getType() == PatternType::VAR) {
        if (debug_haskell) std::cout << "[VAR]" << std::endl;
        auto* pt_var = dynamic_cast<PtVar*>(pattern.get());
        std::cout << pt_var->name;
    } else if (pattern->getType() == PatternType::TUPLE) {
        if (debug_haskell) std::cout << "[TUPLE]" << std::endl;
        auto* pt_tuple = dynamic_cast<PtTuple*>(pattern.get());
        bool flag = false;
        for (auto& pat : pt_tuple->pattern_list) {
            if (flag) {
                std::cout << " ";
            } else {
                flag = true;
            }
            bool need_bracket = (pat->getType() == PatternType::CONSTRUCTOR
                    || pat->getType() == PatternType::TUPLE);
            if (need_bracket) std::cout << "(";
            patternToHaskell(pat);
            if (need_bracket) std::cout << ")";
        }
    } else if (pattern->getType() == PatternType::CONSTRUCTOR) {
        if (debug_haskell) std::cout << "[CONSTRUCTOR]" << std::endl;
        auto* pt_cons = dynamic_cast<PtConstructor*>(pattern.get());
        // first char of constructors should be capitalised
        pt_cons->name[0] = std::toupper(pt_cons->name[0]);
        std::cout << pt_cons->name << " ";
        bool need_bracket = (pt_cons->pattern->getType() == PatternType::CONSTRUCTOR);
        if (need_bracket) std::cout << "(";
        patternToHaskell(pt_cons->pattern);
        if (need_bracket) std::cout << ")";
    } else {
        LOG(FATAL) << "Unknown pattern";
    }
}

void incre::termToHaskell(const std::shared_ptr<TermData> &term, bool after_constructor = false, bool is_func_name = false) {
    if (debug_haskell) std::cout << std::endl << "[zyw: termToHaskell]" << std::endl;
    if (term->getType() == TermType::VALUE) {
        if (debug_haskell) std::cout << "[VALUE]" << std::endl;
        auto* tm_value = dynamic_cast<TmValue*>(term.get());
        if (tm_value->data.isNull() || tm_value->data.toString() == "unit") {
            std::cout << "Unit";
        } else {
            std::string s = tm_value->data.toString();
            if (s == ">" || s == "<" || s == ">=" || s == "<=" || s == "==" || s == "&&" || s == "||") s += "~";
            if (s == "true") s = "(toSym True)";
            if (s == "false") s = "(toSym False)";
            if (s == "and") s = "&&~";
            if (s == "or") s = "||~";
            if (s == "/") s = "div'";
            if (modified_func_name.find(s) != modified_func_name.end() && is_func_name) s += "'";
            std::cout << s;
        }
    } else if (term->getType() == TermType::IF) {
        if (debug_haskell) std::cout << "[IF]" << std::endl;
        auto* tm_if = dynamic_cast<TmIf*>(term.get());
        std::cout << "mrgIte (";
        termToHaskell(tm_if->c);
        std::cout << ")";
        floor_num_haskell++;
        std::cout << std::endl;
        printSpace(floor_num_haskell);
        std::cout << "(";
        termToHaskell(tm_if->t);
        std::cout << ")";
        std::cout << std::endl;
        printSpace(floor_num_haskell);
        std::cout << "(";
        termToHaskell(tm_if->f);
        std::cout << ")";
        floor_num_haskell--;
    } else if (term->getType() == TermType::VAR) {
        if (debug_haskell) std::cout << "[VAR]" << std::endl;
        auto* tm_var = dynamic_cast<TmVar*>(term.get());
        if (construct.find(tm_var->name) != construct.end()) {
            tm_var->name[0] = std::toupper(tm_var->name[0]);
        }
        if (modified_func_name.find(tm_var->name) != modified_func_name.end() && is_func_name) {
            tm_var->name += "'";
        }
        std::cout << tm_var->name;
        if (debug_haskell) std::cout << std::endl << "[Term_VAR_END]" << std::endl;
    } else if (term->getType() == TermType::LET) {
        if (debug_haskell) std::cout << "[LET]" << std::endl;
        auto* tm_let = dynamic_cast<TmLet*>(term.get());
        incre::TermType def_type = tm_let->def->getType();
        bool need_bracket = !(def_type == incre::TermType::VALUE ||
                              def_type == incre::TermType::VAR || def_type == incre::TermType::TUPLE
                              || def_type == incre::TermType::PROJ);
        std::cout << "let" << std::endl;
        floor_num_haskell++;
        printSpace(floor_num_haskell);
        std::cout << tm_let->name << " =" << std::endl;
        floor_num_haskell++;
        printSpace(floor_num_haskell);
        if (need_bracket) std::cout << "(";
        termToHaskell(tm_let->def);
        if (need_bracket) std::cout << ")";
        floor_num_haskell -= 2;
        std::cout << std::endl;
        printSpace(floor_num_haskell);
        std::cout << "in" << std::endl;
        printSpace(floor_num_haskell);
        termToHaskell(tm_let->content);
    } else if (term->getType() == TermType::TUPLE) {
        if (debug_haskell) std::cout << "[TUPLE]" << std::endl;
        auto* tm_tuple = dynamic_cast<TmTuple*>(term.get());
        bool flag = false;
        // if after_constructor = true, format of tuple is Cons h t, otherwise should be Cons (h, t)
        if (!after_constructor) std::cout << "(";
        for (auto& tuple_field : tm_tuple->fields) {
            if (flag) {
                if (!after_constructor) std::cout << ", ";
                else std::cout << " ";
            } else {
                flag = true;
            }
            incre::TermType content_type = tuple_field->getType();
            bool need_bracket = !(content_type == incre::TermType::VALUE ||
                                  content_type == incre::TermType::VAR || content_type == incre::TermType::TUPLE
                                  || content_type == incre::TermType::PROJ);
            if (need_bracket) std::cout << "(";
            termToHaskell(tuple_field);
            if (need_bracket) std::cout << ")";
        }
        if (!after_constructor) std::cout << ")";
    } else if (term->getType() == TermType::PROJ) {
        if (debug_haskell) std::cout << "[PROJ]" << std::endl;
        auto* tm_proj = dynamic_cast<TmProj*>(term.get());
        incre::TermType content_type = tm_proj->content->getType();
        bool need_bracket = !(content_type == incre::TermType::VALUE ||
                              content_type == incre::TermType::VAR || content_type == incre::TermType::TUPLE
                              || content_type == incre::TermType::PROJ);
        /* if (tm_proj->id == 1) std::cout << "(get1th ";
        else if (tm_proj->id == 2) std::cout << "(get2th ";
        else if (tm_proj->id == 3) std::cout << "(get3th "; */
        if (tm_proj->id == 1) std::cout << "(fst ";
        else if (tm_proj->id == 2) std::cout << "(snd ";
        else std::cout << "(get" << std::to_string(tm_proj->id) << "th ";
        // else LOG(FATAL) << "Unknown PROJ->ID";
        if (need_bracket) std::cout << "(";
        termToHaskell(tm_proj->content);
        if (need_bracket) std::cout << ")";
        std::cout << ")";
    } else if (term->getType() == TermType::ABS) {
        if (debug_haskell) std::cout << "[ABS]" << std::endl;
        auto* tm_abs = dynamic_cast<TmAbs*>(term.get());
        termToHaskell(tm_abs->content);
    } else if (term->getType() == TermType::APP) {
        if (debug_haskell) std::cout << "[APP]" << std::endl;
        auto* tm_app = dynamic_cast<TmApp*>(term.get());
        std::string func_name = tm_app->func->toString();
        if (func_name == "+" || func_name == "-" || func_name == "*" || func_name == "<" || func_name == ">" || func_name == "<=" || func_name == ">=" || func_name == "==" || func_name == "&&" || func_name == "||" || func_name == "and" || func_name == "or") {
            incre::TermType param_type = tm_app->param->getType();
            bool need_bracket = !(param_type == incre::TermType::VALUE ||
                                  param_type == incre::TermType::VAR || param_type == incre::TermType::TUPLE
                                  || param_type == incre::TermType::PROJ);
            if (need_bracket) std::cout << "(";
            termToHaskell(tm_app->param);
            if (need_bracket) std::cout << ")";
            std::cout << " ";
            termToHaskell(tm_app->func, false, true);
            std::cout << " ";
        } else if (func_name == "not") {
            std::cout << "mrgIte ((";
            termToHaskell(tm_app->param);
            std::cout << ") ==~ (toSym True)) (toSym False) (toSym True)";
        }
        else {
            /*if (in_fix_func && func_name == post_func_name) {
              std::cout << pre_func_name;
            } else if (modified_func_name.find(func_name) != modified_func_name.end()) {
              // if the func name is reserved word, add "'" at the end
              std::cout << func_name << "'";
            } else {
              termToHaskell(tm_app->func);
            }*/
            termToHaskell(tm_app->func, false, true);
            incre::TermType param_type = tm_app->param->getType();
            bool need_bracket = !(param_type == incre::TermType::VALUE ||
                                  param_type == incre::TermType::VAR || param_type == incre::TermType::TUPLE
                                  || param_type == incre::TermType::PROJ);
            std::cout << " ";
            if (need_bracket) std::cout << "(";
            if (construct.find(func_name) != construct.end()) {
                termToHaskell(tm_app->param, true);
            } else {
                termToHaskell(tm_app->param);
            }
            if (need_bracket) std::cout << ")";
        }
    } else if (term->getType() == TermType::FIX) {
        if (debug_haskell) std::cout << "[FIX]" << std::endl;
        in_fix_func++;
        auto* tm_fix = dynamic_cast<TmFix*>(term.get());
        if (tm_fix->content->getType() != TermType::ABS) {
            std::cout << "error: fix.content is not Term::ABS!" << std::endl;
        } else {
            auto* tm_first_content = dynamic_cast<TmAbs*>(tm_fix->content.get());
            std::string fix_func_name = tm_first_content->name;
            std::cout << "let" << std::endl;
            floor_num_haskell++;
            printSpace(floor_num_haskell);
            std::cout << fix_func_name << " ";
            termParamToHaskell(tm_fix->content, true);
            std::cout << "= " << std::endl;
            floor_num_haskell++;
            printSpace(floor_num_haskell);
            termToHaskell(tm_first_content->content);
            floor_num_haskell -= 2;
            std::cout << std::endl;
            printSpace(floor_num_haskell);
            std::cout << "in" << std::endl;
            printSpace(floor_num_haskell);
            std::cout << fix_func_name << " ";
        }
        in_fix_func--;
        if (debug_haskell) std::cout << std::endl << "[FIX_END]" << std::endl;
    } else if (term->getType() == TermType::MATCH) {
        if (debug_haskell) std::cout << "[MATCH]" << std::endl;
        auto* tm_match = dynamic_cast<TmMatch*>(term.get());
        std::cout << "case ";
        termToHaskell(tm_match->def);
        std::cout << " of" << std::endl;
        floor_num_haskell++;
        for (auto &match_case : tm_match->cases) {
            printSpace(floor_num_haskell);
            patternToHaskell(match_case.first);
            std::cout << " -> ";
            floor_num_haskell++;
            auto term_type = match_case.second->getType();
            if (term_type == TermType::LET || term_type == TermType::FIX ||
                term_type == TermType::MATCH || term_type == TermType::ALIGN) {
                std::cout << std::endl;
                printSpace(floor_num_haskell);
            }
            termToHaskell(match_case.second);
            floor_num_haskell--;
            std::cout << std::endl;
        }
        floor_num_haskell--;
        printSpace(floor_num_haskell);
    } else if (term->getType() == TermType::LABEL) {
        auto* tm_label = dynamic_cast<TmLabel*>(term.get());
        auto need_bracket = _isNeedBracket(tm_label->content->getType());
        std::cout << "label ";
        if (need_bracket) std::cout << "(";
        termToHaskell(tm_label->content);
        if (need_bracket) std::cout << ")";
        std::cout << " ";
    } else if (term->getType() == TermType::UNLABEL) {
        auto* tm_unlabel = dynamic_cast<TmUnLabel*>(term.get());
        auto need_bracket = _isNeedBracket(tm_unlabel->content->getType());
        std::cout << "unlabel ";
        if (need_bracket) std::cout << "(";
        termToHaskell(tm_unlabel->content);
        if (need_bracket) std::cout << ")";
        std::cout << " ";
    } else if (term->getType() == TermType::ALIGN) {
        auto* tm_align = dynamic_cast<TmAlign*>(term.get());
        auto need_bracket = _isNeedBracket(tm_align->content->getType());
        if (hole_num_now > eval_string_for_each_hole.size()) {
            LOG(FATAL) << "not enough eval string for hole, hole_num_now = " << hole_num_now; 
        }
        std::cout << eval_string_for_each_hole[hole_num_now++];
        /*
        if (need_bracket) std::cout << "(";
        termToHaskell(tm_align->content);
        if (need_bracket) std::cout << ")";
        std::cout << " ";
        */
    } else {
        LOG(FATAL) << "Unknown term";
    }
}

// 获取函数的参数
void incre::termParamToHaskell(const std::shared_ptr<TermData> &term, bool first_param = true) {
    if (term->getType() == TermType::ABS) {
        if (debug_haskell) std::cout << "[ABS-PARAM]" << std::endl;
        auto* tm_abs = dynamic_cast<TmAbs*>(term.get());
        if (!first_param || in_fix_func == 0) {
            std::cout << tm_abs->name << " ";
        }
        termParamToHaskell(tm_abs->content, false);
    }
}

void incre::bindingToHaskell(const std::shared_ptr<BindingData> &binding) {
    if (debug_haskell) std::cout << std::endl << "[zyw: bindingToHaskell]" << std::endl;
    floor_num_haskell++;
    if (binding->getType() == BindingType::TYPE) {
        if (debug_haskell) std::cout << "[TYPE]" << std::endl;
        auto* type_binding = dynamic_cast<TypeBinding*>(binding.get());
        tyToHaskell(type_binding->type);
    } else if (binding->getType() == BindingType::TERM) {
        if (debug_haskell) std::cout << "[TERM]" << std::endl;
        auto* term_binding = dynamic_cast<TermBinding*>(binding.get());
        // std::cout << "term_binding->term: " << termType2String(term_binding->term->getType()) << std::endl;
        termParamToHaskell(term_binding->term);
        std::cout << "= " << std::endl;
        printSpace(floor_num_haskell);
        termToHaskell(term_binding->term);
        if (debug_haskell) std::cout << std::endl << "[Binding_TERM_term_BEGIN]" << std::endl;
        if (term_binding->type) {
            std::cout << ": ";
            tyToHaskell(term_binding->type);
            if (debug_haskell) std::cout << std::endl << "[Binding_TERM_type_END]" << std::endl;
        }
    } else if (binding->getType() == BindingType::VAR) {
        if (debug_haskell) std::cout << "[VAR]" << std::endl;
        auto* var_type_binding = dynamic_cast<VarTypeBinding*>(binding.get());
        tyToHaskell(var_type_binding->type);
    } else {
        LOG(FATAL) << "Unknown binding";
    }
    floor_num_haskell--;
}

// 输出binding的类型（如果有的话），用于函数的类型声明。name是函数名
void incre::bindingTyToHaskell(const std::shared_ptr<BindingData> &binding, const std::string &name) {
    if (debug_haskell) std::cout << std::endl << "[zyw: bindingTyToHaskell]" << std::endl;
    floor_num_haskell++;
    if (binding->getType() == BindingType::TYPE) {
        if (debug_haskell) std::cout << "[TYPE-getty]" << std::endl;
        auto* type_binding = dynamic_cast<TypeBinding*>(binding.get());
        std::cout << name << " :: ";
        tyToHaskell(type_binding->type);
        std::cout << std::endl;
    } else if (binding->getType() == BindingType::TERM) {
        if (debug_haskell) std::cout << "[TERM-getty]" << std::endl;
        auto* term_binding = dynamic_cast<TermBinding*>(binding.get());
        // 如果有type就print，否则可以通过自动类型推导得到，没有类型声明Haskell也可以运行
        if (term_binding->type) {
            std::cout << name << " :: ";
            tyToHaskell(term_binding->type);
            std::cout << std::endl;
        }
        // 如果是fix构成的函数，第一个参数(abs)的类型为该函数的类型
        else if (term_binding->term->getType() == TermType::FIX) {
            if (debug_haskell) std::cout << "[FIX-getty]" << std::endl;
            pre_func_name = name;
            in_fix_func = 1;
            auto* fix_term = dynamic_cast<TmFix*>(term_binding->term.get());
            if (fix_term->content->getType() == TermType::ABS) {
                auto* first_abs_term = dynamic_cast<TmAbs*>(fix_term->content.get());
                post_func_name = first_abs_term->name;
                if (debug_haskell) std::cout << std::endl << "zyw: pre_func_name = " << pre_func_name << ", post_func_name = " << post_func_name << std::endl;
                std::cout << name << " :: ";
                tyToHaskell(first_abs_term->type);
                std::cout << std::endl;
            } else {
              LOG(FATAL) << "Unknown fix func";
            }
        }
    } else if (binding->getType() == BindingType::VAR) {
        if (debug_haskell) std::cout << "[VAR]" << std::endl;
        auto* var_type_binding = dynamic_cast<VarTypeBinding*>(binding.get());
        std::cout << name << " :: ";
        tyToHaskell(var_type_binding->type);
        std::cout << std::endl;
    } else {
        LOG(FATAL) << "Unknown binding";
    }
    floor_num_haskell--;
}

void incre::commandToHaskell(const std::shared_ptr<CommandData> &command) {
    floor_num_haskell = 0;
    for (auto deco: command->decorate_set) {
        std::cout << "@" << decorate2String(deco) << " ";
    }
    if (debug_haskell) std::cout << std::endl << "[zyw: commandToHaskell]" << std::endl;
    if (command->getType() == CommandType::IMPORT) {
        if (debug_haskell) std::cout << "[IMPORT]" << std::endl;
        auto* command_import = dynamic_cast<CommandImport*>(command.get());
        std::cout << command_import->name;
        for (auto& command: command_import->commands) {
            commandToHaskell(command);
        }
    } else if (command->getType() == CommandType::BIND) {
        if (debug_haskell) std::cout << "[BIND]" << std::endl;
        auto* command_bind = dynamic_cast<CommandBind*>(command.get());
        // if the func name is reserved word, add "'" at the end
        if (modified_func_name.find(command_bind->name) != modified_func_name.end()) {
          command_bind->name += "'";
        }
        // 不一定需要？
        // bindingTyToHaskell(command_bind->binding, command_bind->name);
        std::cout << command_bind->name << " ";
        bindingToHaskell(command_bind->binding);
        std::cout << std::endl;
        // pre_func_name = post_func_name = "";
        in_fix_func = 0;
    } else if (command->getType() == CommandType::DEF_IND) {
        if (debug_haskell) std::cout << "[DEF_IND]" << std::endl;
        auto* command_def = dynamic_cast<CommandDefInductive*>(command.get());
        auto* ty_ind = dynamic_cast<TyInductive*>(command_def->_type.get());
        // name of the newly defined type
        std::string def_name = ty_ind->name;
        if (debug_haskell) std::cout << std::endl << "zyw: def_name = " << def_name << std::endl;
        std::cout << "data ";
        floor_num_haskell++;
        tyToHaskell(command_def->_type);
        floor_num_haskell--;
        outputDeriving(def_name);
    } else {
        LOG(FATAL) << "Unknown command";
    }
}

// output SimpleMergeable of defined data type
void incre::outputSimpleMergeable(const std::shared_ptr<CommandData> &command) {
    if (command->getType() != CommandType::DEF_IND) {
        LOG(FATAL) << "command type is not def_ind";
    }
    auto* command_def = dynamic_cast<CommandDefInductive*>(command.get());
    auto* ty_ind = dynamic_cast<TyInductive*>(command_def->_type.get());
    // name of the newly defined type
    std::string def_name = ty_ind->name;
    std::cout << "instance SimpleMergeable " << def_name << " where" << std::endl;
    std::cout << "  mrgIte cond l r = go cond l r" << std::endl;
    std::cout << "    where" << std::endl;
    
}

// some output before processing commands, read from incre-tests/pre_output.txt
void incre::preOutput() {
    std::string pre_output_path = config::KSourcePath + "incre-tests/pre_output.txt";
    std::ifstream pre_output_file(pre_output_path);
    if (pre_output_file.is_open()) {
        std::string line;
        while (std::getline(pre_output_file, line)) {
            std::cout << line << '\n';
        }
        pre_output_file.close();
    } else {
        std::cout << "Unable to open pre_output_file\n";
    }
}

// output the define of data RefEnv
void incre::outputRefEnv() {
    std::string pre_output_path = config::KSourcePath + "incre-tests/output_RefEnv.txt";
    std::ifstream pre_output_file(pre_output_path);
    if (pre_output_file.is_open()) {
        std::string line;
        while (std::getline(pre_output_file, line)) {
            std::cout << line << '\n';
        }
        pre_output_file.close();
    } else {
        std::cout << "Unable to open pre_output_file\n";
    }
}

void incre::envToHaskell(std::vector<std::pair<std::vector<std::string>, Grammar* > > &final_grammar, std::vector<std::pair<PType, int> > &env_type_list) {
    bool flag = false;
    for (auto [_, grammar]: final_grammar) {
        for (auto* node: grammar->symbol_list) {
            for (auto* rule: node->rule_list) {
                std::string semantics_name = rule->getSemanticsName();
                if (semantics_name.substr(0, 5) == "Param") {
                    std::string name_now = node->type->getHaskellName();
                    flag = false;
                    for (auto type: env_type_list) {
                        if (name_now == (type.first)->getHaskellName()) {
                            flag = true;
                            break;
                        }
                    }
                    if (!flag) {
                        env_type_list.push_back(std::make_pair(node->type, env_type_list.size()));
                    }
                    break;
                }
            }
        }
    }

    if (env_type_list.size() == 0) {
        std::shared_ptr<Type> ty_ptr = std::dynamic_pointer_cast<Type>(std::make_shared<TBool>());
        env_type_list.push_back(std::make_pair(ty_ptr, 0));
    }

/*
data EnvValue
    = Env1 SymInteger
    | Env2 SymBool
    | Env3 (SymInteger, SymInteger)
    deriving (Show, Generic)
    deriving (EvaluateSym) via (Default EnvValue)*/
    std::cout << "data EnvValue" << std::endl;
    for (int i = 0; i < env_type_list.size(); ++i) {
        if (!i) {
            std::cout << "  =";
        } else {
            std::cout << "  |";
        }
        std::cout <<" Env" <<  std::to_string(i) << " " << env_type_list[i].first->getHaskellName() << std::endl;
    }
    std::cout << "  deriving (Show, Generic)" << std::endl << "  deriving (EvaluateSym) via (Default EnvValue)" << std::endl << std::endl;

/*
instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env1 _ -> 0 :: Int
          Env2 _ -> 1
          Env3 _ -> 2
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env1 l) (Env1 r) -> Env1 $ mrgIte cond l r
          1 -> SimpleStrategy $ \cond (Env2 l) (Env2 r) -> Env2 $ mrgIte cond l r
          2 -> SimpleStrategy $ \cond (Env3 l) (Env3 r) -> Env3 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env1 l) (Env1 r) = Env1 $ mrgIte cond l r
      go cond (Env2 l) (Env2 r) = Env2 $ mrgIte cond l r
      go cond (Env3 l) (Env3 r) = Env3 $ mrgIte cond l r
      go _ _ _ = error "Should not happen"

$(makeUnionWrapper "u" ''EnvValue)
*/
    std::cout << "instance Mergeable EnvValue where" << std::endl;
    std::cout << "  rootStrategy =" << std::endl;
    std::cout << "    SortedStrategy" << std::endl;
    std::cout << "      ( \\case" << std::endl;
    for (int i = 0; i < env_type_list.size(); ++i) {
        std::cout << "          Env" << std::to_string(i) << " _ -> " << std::to_string(i) << " :: Int" << std::endl;
    }
    std::cout << "      )" << std::endl;
    std::cout << "      ( htmemo $ \\case" << std::endl;
    for (int i = 0; i < env_type_list.size(); ++i) {
        std::cout << "          " << std::to_string(i);
        std::cout << " -> SimpleStrategy $ \\cond (Env" << std::to_string(i) << " l) (Env" << std::to_string(i);
        std::cout << " r) -> Env" << std::to_string(i) << " $ mrgIte cond l r" << std::endl;
    }
    std::cout << "          _ -> error \"Should not happen\"" << std::endl;
    std::cout << "      )" << std::endl << std::endl;

    std::cout << "instance SimpleMergeable EnvValue where" << std::endl;
    std::cout << "  mrgIte cond l r = go cond l r" << std::endl;
    std::cout << "    where" << std::endl;
    for (int i = 0; i < env_type_list.size(); ++i) {
        std::cout << "      go cond (Env" << std::to_string(i) << " l) (Env" << std::to_string(i);
        std::cout << " r) = Env" << std::to_string(i) << " $ mrgIte cond l r" << std::endl;
    }
    std::cout << "      go _ _ _ = error \"Should not happen\"" << std::endl;

    std::cout << std::endl << "$(makeUnionWrapper \"u\" ''EnvValue)" << std::endl << std::endl;

    outputRefEnv();

/*
evalVar1 :: RefEnv -> Ident -> SymInteger
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"
*/ 
    for (int i = 0; i < env_type_list.size(); ++i) {
        std::cout << "evalVar" << std::to_string(i) << " :: RefEnv -> Ident -> " << env_type_list[i].first->getHaskellName() << std::endl;
        std::cout << "evalVar" << std::to_string(i) << " (RefEnv env) x =" << std::endl;
        std::cout << "    let v = evalFunc (RefEnv env) x in" << std::endl;
        std::cout << "    case v of" << std::endl;
        std::cout << "      Env" << std::to_string(i) << " sym -> sym" << std::endl;
        std::cout << "      _ -> error \"evalVar" << std::to_string(i) << ": variable type not matched\"" << std::endl << std::endl;
    }
}

/*
data AExpr
  = I SymInteger
  | Var (UnionM Ident)
  | Add (UnionM AExpr) (UnionM AExpr)
  | AList List
  | If (UnionM BExpr) (UnionM AExpr) (UnionM AExpr)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon AExpr)
    via (Default AExpr)
*/
void incre::grammarToHaskell(Grammar *grammar, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num) {
    name_to_expr_num[grammar->start->name] = 0;
    next_expr_num = 1;
    for (auto* node: grammar->symbol_list) {
        if (name_to_expr_num.find(node->name) == name_to_expr_num.end()) {
            name_to_expr_num[node->name] = next_expr_num++;
        }
    }
    for (auto* node: grammar->symbol_list) {
        std::cout << "data Expr" << func_num << "_" << name_to_expr_num[node->name] << std::endl;
        bool flag = false;
        for (auto* rule: node->rule_list) {
            std::string semantics_name = rule->getSemanticsName();
            // discard al_error
            if (semantics_name.substr(0, 8) == "al_error") continue;
            if (!flag) {
                std::cout << "  = ";
                flag = true;
            } else {
                std::cout << "  | ";
            }
            std::cout << rule->toHaskell(name_to_expr_num, next_expr_num, func_num, node->name) << std::endl;
        }
        std::string node_name = std::to_string(func_num) + "_" + std::to_string(name_to_expr_num[node->name]);
        std::cout << "  deriving stock (Generic, Show)" << std::endl
            << "  deriving (Mergeable, EvaluateSym, ToCon Expr" << node_name << ")"
            << std::endl << "    via (Default Expr" << node_name << ")" << std::endl;
        std::cout << std::endl;
    }
}

/*
instance GenSym (Int) Expr0_0 where
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam1]
    genSingle1 = [mrgCzero]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do 
        e1 <- (gen1 (gendepth - 1)) 
        e2 <- (gen1 (gendepth - 1)) 
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd e1 e2])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do 
        e1 <- (gen0 (gendepth - 1)) 
        e2 <- (gen0 (gendepth - 1)) 
        res <- chooseUnionFresh (genSingle1 ++ [mrgAccess0 e1] ++ [mrgAccess1 e2])
        return res
*/
void incre::spaceToHaskell(Grammar *grammar, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num) {
    std::string start_node_name = std::to_string(func_num) + "_" + std::to_string(0);
    std::cout << "instance GenSym (Int) Expr" << start_node_name << " where " << std::endl;
    std::cout << "  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr" << start_node_name << ")" << std::endl;
    std::cout << "  fresh gendepth = gen0 gendepth " << std::endl;
    std::cout << "    where" << std::endl;

    // (has_single = true) means the node has at least one constructor without any param, so it can have genSingle func
    std::vector<bool> has_single(next_expr_num, false);
    bool flag = false;
    for (auto* node: grammar->symbol_list) {
        int node_num = name_to_expr_num[node->name];
        for (auto* rule: node->rule_list) {
            if (rule->param_list.size() == 0) {
                has_single[node_num] = true;
                break;
            }
        }
        // if has_single = true, output "genSingle0 = [mrgParam1]"
        flag = false;
        if (has_single[node_num]) {
            std::cout << "    genSingle" << std::to_string(node_num) <<" = ";
            for (auto* rule: node->rule_list) {
                if (rule->param_list.size() == 0) {
                    if (!flag) flag = true;
                    else std::cout << " ++ ";
                    std::cout << "[mrg" << rule->getSemanticsName() << std::to_string(func_num) << "_" << std::to_string(node_num) << "]";
                }
            }
            std::cout << std::endl;
        } else {
            LOG(FATAL) << "Unexpected datatype: has_sinle = false, func_num = " << func_num;
        }
    }
    for (auto* node: grammar->symbol_list) {
        int node_num = name_to_expr_num[node->name];
        std::string node_name = std::to_string(func_num) + "_" + std::to_string(node_num);
        std::vector<int> max_num_of_param(next_expr_num, 0);
        std::vector<int> param_num_now(next_expr_num, 0);
        for (auto* rule: node->rule_list) {
            std::string semantics_name = rule->getSemanticsName();
            if (semantics_name.substr(0, 8) == "al_error") continue;
            for (auto &param: rule->param_list) {
                max_num_of_param[name_to_expr_num[param->name]]++;
            }
        }
        std::cout << "    gen" << std::to_string(node_num) << " gendepth";
        // grammar has genSingle
        if (has_single[node_num]) {
            std::cout << std::endl;
            std::cout << "      | gendepth <= 0 = chooseUnionFresh genSingle" << std::to_string(node_num) << std::endl;
            std::cout << "      | otherwise = do" << std::endl;
            for (int i = 0; i < next_expr_num; ++i) {
                for (int j = 0; j < max_num_of_param[i]; ++j) {
                    std::string param_name = "e" + std::to_string(i) + "_" + std::to_string(j);
                    std::cout << "        " << param_name << " <- (gen" << std::to_string(i) << " (gendepth - 1))"<< std::endl;
                }
            }
            std::cout << "        res <- chooseUnionFresh (genSingle" << std::to_string(node_num);
            for (auto* rule: node->rule_list) {
                if (rule->param_list.size() == 0) continue;
                std::string semantics_name = rule->getSemanticsName();
                if (semantics_name.substr(0, 8) == "al_error") continue;
                std::cout << " ++ [";
                std::cout << "mrg" << rule->getSemanticsName() << std::to_string(func_num) << "_" << std::to_string(node_num);
                for (auto &param: rule->param_list) {
                    int expr_num = name_to_expr_num[param->name];
                    if (param_num_now[expr_num] > max_num_of_param[expr_num]) {
                        LOG(FATAL) << "not enough param!";
                    }
                    int param_num = param_num_now[expr_num]++;
                    std::cout << " e" << std::to_string(expr_num) << "_" << std::to_string(param_num);
                }
                std::cout << "]";
            }
            std::cout << ")" << std::endl << "        return res";
        }
        // grammar doesn't have genSignle
        else {
            std::cout << " = do" << std::endl;
            for (int i = 0; i < next_expr_num; ++i) {
                for (int j = 0; j < max_num_of_param[i]; ++j) {
                    std::string param_name = "e" + std::to_string(i) + "_" + std::to_string(j);
                    std::cout << "        " << param_name << " <- (gen" << std::to_string(i) << " (gendepth - 1))"<< std::endl;
                }
            }
            std::cout << "        res <- chooseUnionFresh (";
            flag = false;
            for (auto* rule: node->rule_list) {
                if (rule->param_list.size() == 0) continue;
                std::string semantics_name = rule->getSemanticsName();
                if (semantics_name.substr(0, 8) == "al_error") continue;
                if (!flag) flag = true;
                else std::cout << " ++ ";
                std::cout << "[mrg" << rule->getSemanticsName() << std::to_string(func_num) << "_" << std::to_string(node_num);
                for (auto &param: rule->param_list) {
                    int expr_num = name_to_expr_num[param->name];
                    if (param_num_now[expr_num] > max_num_of_param[expr_num]) {
                        LOG(FATAL) << "not enough param!";
                    }
                    int param_num = param_num_now[expr_num]++;
                    std::cout << " e" << std::to_string(expr_num) << "_" << std::to_string(param_num);
                }
                std::cout << "]";
            }
            std::cout << ")" << std::endl << "        return res";
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

/* void incre::evalTupleToHaskell(std::vector<std::pair<std::vector<std::string>, Grammar *> > &final_grammar) {
    int max_tuple_size = 0;
    for (int i = 0; i < final_grammar.size(); ++i) {
        auto [_, grammar] = final_grammar[i];
        for (auto* node: grammar->symbol_list) {
            for (auto* rule: node->rule_list) {
                std::string semantics_name = rule->getSemanticsName();
                if (semantics_name.substr(0, 6) == "Access") {
                    max_tuple_size = std::max(max_tuple_size, (semantics_name[6] - '0' + 1));
                }
            }
        }
    }
    if (max_tuple_size == 2) {
        std::cout << "get1th :: (SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get1th (a, _) = a" << std::endl << std::endl;
        std::cout << "get2th :: (SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get2th (_, b) = b" << std::endl << std::endl;
    } else if (max_tuple_size == 3) {
        std::cout << "get1th :: (SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get1th (a, _, _) = a" << std::endl << std::endl;
        std::cout << "get2th :: (SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get2th (_, b, _) = b" << std::endl << std::endl;
        std::cout << "get3th :: (SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get3th (_, _, c) = c" << std::endl << std::endl;
    } else if (max_tuple_size == 4) {
        std::cout << "get1th :: (SymInteger, SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get1th (a, _, _, _) = a" << std::endl << std::endl;
        std::cout << "get2th :: (SymInteger, SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get2th (_, b, _, _) = b" << std::endl << std::endl;
        std::cout << "get3th :: (SymInteger, SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get3th (_, _, c, _) = c" << std::endl << std::endl;
        std::cout << "get4th :: (SymInteger, SymInteger, SymInteger, SymInteger) -> SymInteger" << std::endl;
        std::cout << "get4th (_, _, _, d) = d" << std::endl << std::endl;
    }
} */

void incre::evalToHaskell(Grammar *grammar, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num,
        std::vector<std::string>& param_list, std::vector<std::pair<PType, int> > &env_type_list) {
    std::vector<int> tuple_len;
    for (auto* node: grammar->symbol_list) {
        std::string oup_type = node->type->getHaskellName();
        int count = node->type->getTupleLen();
        // int count = std::count(oup_type.begin(), oup_type.end(), ',');
        tuple_len.push_back(count);
    }
    /* for (auto num: tuple_len) {
        std::cout << ", " << num;
    }
    std::cout << std::endl;*/
    for (auto* node: grammar->symbol_list) {
        std::string node_name = std::to_string(func_num) + "_" + std::to_string(name_to_expr_num[node->name]);
        std::string oup_type = node->type->getHaskellName();
        std::cout << "eval" << node_name << " :: RefEnv -> Expr"
            << node_name << " -> " << oup_type << std::endl;
        for (auto* rule: node->rule_list) {
            std::string semantics_name = rule->getSemanticsName();
            // discard al_error
            if (semantics_name.substr(0, 8) == "al_error") continue;
            std::cout << rule->evalRuleToHaskell(node_name, func_num, name_to_expr_num, param_list, node->type->getHaskellName(), env_type_list, tuple_len) << std::endl;
        }
        std::cout << std::endl;
        /*
        evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymBool
        evalU1_1 env = onUnion (eval1_1 env)
        */
        std::cout << "evalU" << node_name << " :: RefEnv -> UnionM Expr"
            << node_name << " -> " << oup_type << std::endl;
        std::cout << "evalU" << node_name << " env = onUnion (eval"
            << node_name << " env)" << std::endl;
        std::cout << std::endl;
    }
}

void incre::getEvalString(std::vector<std::string> &eval_string_for_each_hole,
        std::vector<std::pair<std::vector<std::string>, Grammar *> >& final_grammar,
        TyList &final_type_list,
        std::vector<std::pair<PType, int> > &env_type_list) {
    int hole_size = final_grammar.size();
    bool flag = false;
    
    for (int i = 0; i < hole_size; ++i) {
        std::vector<std::pair<int, std::string> > param2type;
        std::vector<std::pair<int, int> > param2eval_var_num;
        auto& [param_list_for_hole, grammar] = final_grammar[i];
        for (auto* node: grammar->symbol_list) {
            std::string node_type = node->type->getHaskellName();
            for (auto* rule: node->rule_list) {
                std::string semantics_name = rule->getSemanticsName();
                if (semantics_name.substr(0, 5) == "Param") {
                    if (rule->param_list.size() == 0) {
                        // std::cout << semantics_name << std::endl;
                        int param_num = semantics_name[5] - '0';
                        param2type.push_back(std::make_pair(param_num, node_type));
                    } else {
                        LOG(FATAL) << "Unexpected rule begin with Param, semantics_name = " << semantics_name;
                    }
                }
            }
        }
        for (auto& [param_num, node_type]: param2type) {
            // std::cout << param_num << ", " << node_type << std::endl;
            flag = false;
            for (auto& [ptype, eval_var_num]: env_type_list) {
                if (node_type == ptype->getHaskellName()) {
                    flag = true;
                    param2eval_var_num.push_back(std::make_pair(param_num, eval_var_num));
                    break;
                }
            }
            if (!flag) {
                LOG(FATAL) << "Unexpected type of param, param_type = " << node_type;
            }
        }
        if (param2type.size() != param2eval_var_num.size()) {
            LOG(FATAL) << "Size of param2type and param2eval_var_num not equal, param2type = " << param2type.size() <<
                ", param2eval_var_num = " << param2eval_var_num.size();
        }

        // evalU0_0 (RefEnv [("tmp5", (EnvTuple tmp5))]) ((genSym (1::Int) "hole1") :: (UnionM Expr0_0))
        flag = false;
        std::string res = "evalU" + std::to_string(i) + "_0" + " (RefEnv [";
        for (auto& [param_num, eval_var_num]: param2eval_var_num) {
            std::string param_name = param_list_for_hole[param_num];
            // std::cout << param_num << ", " << param_name << ", " << eval_var_num << std::endl;
            if (!flag) flag = true;
            else res += ", ";
            res += ("(\"" + param_name + "\", (Env" + std::to_string(eval_var_num) + " " + param_name + "))");
        }
        res += "]) ((genSym (1::Int) \"hole" + std::to_string(i) + "\") :: (UnionM Expr" + std::to_string(i) + "_0))";
        eval_string_for_each_hole.push_back(res);
    }
}

void incre::outputHelperFunc() {
    std::string pre_output_path = config::KSourcePath + "incre-tests/output_HelperFunc.txt";
    std::ifstream pre_output_file(pre_output_path);
    if (pre_output_file.is_open()) {
        std::string line;
        while (std::getline(pre_output_file, line)) {
            std::cout << line << '\n';
        }
        pre_output_file.close();
    } else {
        std::cout << "Unable to open pre_output_file\n";
    }
}

bool incre::compareInput(std::pair<Term, Data> &a, std::pair<Term, Data> &b) {
    return a.first->toString().length() > b.first->toString().length();
}

// some output after processing commands, read from incre-tests/post_output.txt
void incre::postOutput(const std::vector<std::pair<Term, Data>> &io_pairs, int time_limit) {
    std::string post_output_path = config::KSourcePath + "incre-tests/post_output.txt";
    std::ifstream post_output_file(post_output_path);
    if (post_output_file.is_open()) {
        std::string line;
        while (std::getline(post_output_file, line)) {
            std::cout << line << '\n';
        }
        post_output_file.close();
    } else {
        std::cout << "Unable to open post_output_file\n";
    }

    /*
    example:
    let pairs = [(Nil Unit, 0)
                , ((Cons (2) (Cons (-5) (Cons (-1) (Cons (-4) (Nil Unit))))), (-8))
                ]
    */
    int l = io_pairs.size();
    for (int i = 0; i < l; ++i) {
        auto* tm_app = dynamic_cast<TmApp*>(io_pairs[i].first.get());
        /* auto* tm_func = dynamic_cast<TmApp*>(tm_app->func.get());
        auto* tm_value = dynamic_cast<TmValue*>(tm_app->param.get());
        std::cout << tm_app->toString() << std::endl;
        std::cout << tm_func->toString() << std::endl;
        std::cout << tm_func->param->toString() << std::endl;
        std::cout << tm_value->toString() << std::endl;

        std::cout << termType2String(tm_app->getType()) << std::endl;
        std::cout << termType2String(tm_func->getType()) << std::endl;
        std::cout << termType2String(tm_func->param->getType()) << std::endl;
        std::cout << termType2String(tm_value->getType()) << std::endl;*/
        
        printSpace(8);
        if (i) std::cout << ", ";
        std::string result = io_pairs[i].second.value->toHaskell(true);
        if (result == "true") result = "True";
        if (result == "false") result = "False";
        std::cout << "((" << getInputString(tm_app) << "), " << result << ")" << std::endl;
    }
    printSpace(8);
    std::cout << "]" << std::endl;
/*
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just value -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
*/
    std::cout << "    startTime <- getCurrentTime" << std::endl;
    std::cout << "    result <- SysTimeout.timeout (" << std::to_string(time_limit) << " * 60 * 1000000) $ ioPair pairs" << std::endl;
    std::cout << "    endTime <- getCurrentTime" << std::endl;
    std::cout << "    let elapsedTime = diffUTCTime endTime startTime" << std::endl;
    std::cout << "    case result of" << std::endl;
    std::cout << "        Just _ -> putStrLn $ \"Time: \" ++ show elapsedTime ++ \" seconds\"" << std::endl;
    std::cout << "        Nothing -> putStrLn \"Timeout occurred\"" << std::endl;
}

std::string incre::getInputString(TmApp* tm_app) {
    std::string res = "";
    auto* tm_value = dynamic_cast<TmValue*>(tm_app->param.get());
    if (tm_app->func->getType() == TermType::APP) {
        auto* tm_func = dynamic_cast<TmApp*>(tm_app->func.get());
        res += getInputString(tm_func);
        res += ", (" + tm_value->data.value->toHaskell() + ")";
    } else {
        res += "(" + tm_value->data.value->toHaskell() + ")";
    }
    return res;
}

void incre::programToHaskell(const std::shared_ptr<ProgramData> &prog, 
    const std::vector<std::pair<Term, Data>> &io_pairs, 
    incre::IncreInfo *info,
    const incre::IncreAutoLifterSolver *solver, const std::string &path,
    TyList &final_type_list, int time_limit) {
    construct.clear();
    std::ofstream outFile(path);
    std::streambuf *cout_buf = std::cout.rdbuf();
    if (!path.empty()) {
        std::cout.rdbuf(outFile.rdbuf());  
    }
    
    // output some content
    preOutput();

    std::cout << std::endl << "------program space begin----" << std::endl;
    
    // get program space
    std::vector<std::unordered_map<std::string, int> > name_to_expr_num;
    std::vector<int> next_expr_num;
    std::vector<std::pair<std::vector<std::string>, Grammar *> > final_grammar;
    // type for replacing compress type
    for (int i = 0; i < info->align_infos.size(); ++i) {
        auto [param_list, grammar] = buildFinalGrammar(info, i, final_type_list);
        final_grammar.push_back(std::make_pair(param_list, grammar));
        name_to_expr_num.push_back(std::unordered_map<std::string, int>());
        next_expr_num.push_back(0);
    }

    // output env
    std::vector<std::pair<PType, int> > env_type_list;
    envToHaskell(final_grammar, env_type_list);
    std::cout << "{- env_type_list: " << std::endl;
    for (auto [ptype, i]: env_type_list) {
        std::cout << ptype->getHaskellName() << std::endl;
    }
    std::cout << "-}" << std::endl << std::endl;

    // output grammar
    for (int i = 0; i < info->align_infos.size(); ++i) {
        Ty oup_type = info->align_infos[i]->oup_type;
        Ty actual_oup_type = incre::getFinalType(oup_type, final_type_list);
        std::cout << "-- output_type: " << actual_oup_type->toString() << std::endl;
        auto [param_list, grammar] = final_grammar[i];
        // output param_list
        std::cout << "-- param_list:";
        for (auto& param: param_list) std::cout << " " << param;
        std::cout << std::endl;
        grammarToHaskell(grammar, i, name_to_expr_num[i], next_expr_num[i]);
    }

    // $(makeUnionWrapper "mrg" ''Expr0_0)
    for (int i = 0; i < info->align_infos.size(); ++i) {
        for (int j = 0; j < next_expr_num[i]; ++j) {
            std::cout << "$(makeUnionWrapper \"mrg\" ''Expr";
            std::cout << std::to_string(i) << "_" << std::to_string(j) << ")" << std::endl;
        }
    }
    std::cout << std::endl;

    // output program space
    for (int i = 0; i < info->align_infos.size(); ++i) {
        auto [_ , grammar] = final_grammar[i];
        spaceToHaskell(grammar, i, name_to_expr_num[i], next_expr_num[i]);
    }

    // output eval tuple function
    // evalTupleToHaskell(final_grammar);

    // output eval function
    for (int i = 0; i < info->align_infos.size(); ++i) {
        auto [param_list, grammar] = final_grammar[i];
        evalToHaskell(grammar, i, name_to_expr_num[i], param_list, env_type_list);
    }

    std::cout << std::endl << "------program space end----" << std::endl << std::endl
        << "------spec begin-------" << std::endl;

    // get var for each hole
    std::cout << "{-";
    for (int i = 0; i < info->align_infos.size(); ++i) {
        std::cout << std::endl << "Hole grammar for #" << i << std::endl;
        auto& [param_list, _] = final_grammar[i];
        for (auto& param: param_list) {
            std::cout << param << " ";
        }
        std::cout << std::endl;
    }
    std::cout << "-}" << std::endl;

    // get eval clause for each hole
    getEvalString(eval_string_for_each_hole, final_grammar, final_type_list, env_type_list);

    // output def_inductive command
    for (auto &command : prog->commands) {
        if (command->getType() == CommandType::DEF_IND) {
            std::cout << std::endl;
            commandToHaskell(command);
        }
    }

    // output some helper functions
    outputHelperFunc();

    // output spec function
    for (auto &command: prog->commands) {
        if (command->getType() == CommandType::BIND) {
            std::cout << std::endl;
            commandToHaskell(command);
        }
    }
    std::cout << std::endl << "------spec end-------" << std::endl << std::endl
        << "------main function-----" << std::endl;
    
    // output main function
    postOutput(io_pairs, time_limit);

    if (!path.empty()) {
        std::cout.rdbuf(cout_buf);
    }
}
