//
// Created by pro on 2022/8/1.
//

#include <istool/ext/composed_semantics/composed_semantics.h>
#include <iostream>
#include "istool/ext/composed_semantics/composed_rule.h"
#include "glog/logging.h"

ComposedRule::ComposedRule(const PProgram &sketch, const NTList &param_list):
    Rule(param_list), composed_sketch(sketch) {
}

namespace {
    std::string _buildSketch(Program* sketch, const NTList& param_list) {
        auto* ps = dynamic_cast<ParamSemantics*>(sketch->semantics.get());
        if (ps) return param_list[ps->id]->name;
        std::vector<std::string> sub_list(sketch->sub_list.size());
        for (int i = 0; i < sketch->sub_list.size(); ++i) {
            sub_list[i] = _buildSketch(sketch->sub_list[i].get(), param_list);
        }
        return sketch->semantics->buildProgramString(sub_list);
    }

    std::string _buildSketchToHaskell(Program* sketch, const NTList& param_list,
            std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num,
            int func_num, std::string &node_name) {
        std::string res = sketch->semantics->getName();
        // modify constructor
        if (res == "+") res = "Cadd";
        else if (res == "-") res = "Csub";
        else if (res == "*") res = "Cmul";
        else if (res == "=") res = "Ceq";
        else if (res == "<") res = "Cless";
        else if (res == "<=") res = "Cleq";
        else if (res == "<") res = "Cgreat";
        else if (res == "<=") res = "Cgeq";
        else if (res == "&&") res = "Cand";
        else if (res == "||") res = "Cor";
        else if (res == "!") res = "Cnot";
        else if (res == "0") res = "Czero";
        else if (res == "(0) (0)") res = "CZero2";
        else if (res == "(0) (0) (0)") res = "CZero3";
        else if (res == "(0) (0) (0) (0)") res = "CZero4";
        else if (res == "(0) (0) (0) (0) (0)") res = "CZero5";
        else if (res == "(0) (0) (0) (0) (0) (0)") res = "CZero6";
        else if (res == "(false) (0)") res = "CFalseZero";
        else if (res == "(false) (0) (0)") res = "CFalseZero2";
        else if (res == "(false) (0) (0) (false)") res = "CBIIB";
        else if (res == "(false) (false)") res = "CFalse2";
        else if (res == "(false) (false) (false)") res = "CFalse3";
        else if (res == "(false) (false) (false) (false)") res = "CFalse4";
        else if (res == "1") res = "Cone";
        else if (res == "ite") res = "CIte";
        else if (res == "false") res = "CFalse";
        else if (res == "true") res = "CTrue";
        else if (res == "left") res = "Left'";
        else if (res == "right") res = "Right'";
        else if (!(res[0] >= 'a' && res[0] <= 'z' || res[0] >= 'A' && res[0] <= 'Z')) {
            std::cout << "error: res is not a letter!" << std::endl;
            return res;
        }
        else res[0] = std::toupper(res[0]);

        if (res.substr(0, 6) == "Access") {
            if (name_to_expr_num.find(param_list[0]->name) == name_to_expr_num.end()) {
                name_to_expr_num[param_list[0]->name] = next_expr_num++;
            }
            int expr_num = name_to_expr_num[param_list[0]->name];
            res += "_" + std::to_string(expr_num) + "_";
        }

        // change name of semantics
        sketch->semantics->name = res;

        res += std::to_string(func_num) + "_" + std::to_string(name_to_expr_num[node_name]); 

        if (param_list.empty())
        for (auto &param: param_list) {
            if (name_to_expr_num.find(param->name) == name_to_expr_num.end()) {
                name_to_expr_num[param->name] == next_expr_num++;
            }
        }
        auto* ps = dynamic_cast<ParamSemantics*>(sketch->semantics.get());
        if (ps) return param_list[ps->id]->name;
        std::vector<std::string> sub_list(sketch->sub_list.size());
        for (int i = 0; i < sketch->sub_list.size(); ++i) {
            sub_list[i] = _buildSketchToHaskell(sketch->sub_list[i].get(), param_list, name_to_expr_num, next_expr_num, func_num, node_name);
        }
        return sketch->semantics->buildProgramStringToHaskell(sub_list);
    }
}

std::string ComposedRule::toString() const {
    return _buildSketch(composed_sketch.get(), param_list);
}

std::string ComposedRule::getSemanticsName() const {
    Program* sketch = composed_sketch.get();
    return sketch->semantics->getName();
}

std::string ComposedRule::toHaskell(std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num, int func_num, std::string &node_name) {
    return _buildSketchToHaskell(composed_sketch.get(), param_list, name_to_expr_num, next_expr_num, func_num, node_name);
}

std::string ComposedRule::evalRuleToHaskell(std::string node_name, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num, std::vector<std::string>& var_list,
        std::string oup_type, std::vector<std::pair<PType, int> > &env_type_list,
        std::vector<int> &tuple_len) {
    Program* sketch = composed_sketch.get();
    std::vector<int> param_num;
    std::vector<std::string> param_name;
    std::string semantics_name = sketch->semantics->name;

    for (int i = 0; i < param_list.size(); ++i) {
        param_num.push_back(name_to_expr_num[param_list[i]->name]);
        param_name.push_back("p" + std::to_string(i));
    }
    
    std::string res = "eval" + node_name + " env (" + semantics_name + node_name;
    for (int i = 0; i < param_list.size(); ++i) {
        res += " " + param_name[i];
    }
    res += ") = ";
    if (semantics_name == "Cadd" || semantics_name == "Csub" || semantics_name == "Cmul"
        || semantics_name == "Ceq" || semantics_name == "Cless" || semantics_name == "Cleq"
        || semantics_name == "Cgreat" || semantics_name == "Cgeq"
        || semantics_name == "Cand" || semantics_name == "Cor") {
        if (param_list.size() != 2) {
            std::cout << "error: param size != 2" << std::endl;
            return res;
        }
        std::unordered_map<std::string, std::string> func_name_map = {{"Cadd", "+"},
            {"Csub", "-"}, {"Cmul", "*"}, {"Ceq", "==~"}, {"Cless", "<~"},
            {"Cleq", "<=~"}, {"Cgreat", ">~"}, {"Cgeq", ">=~"}, {"Cand", "&&~"},
            {"Cor", "||~"}};
        res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ") ";
        res += func_name_map[semantics_name];
        res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[1])
            + " env " + param_name[1] + ") ";
    }
    else if (semantics_name == "Cnot") {
        if (param_list.size() != 1) {
            std::cout << "error: param size != 1" << std::endl;
            return res;
        }
        res += " if ((evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ") == (toSym True)) then (toSym False) else (toSym True)";
    }
    else if (semantics_name == "Czero") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "0";
    }
    else if (semantics_name == "Cone") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "1";
    }
    else if (semantics_name == "CZero2") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(0,0)";
    }
    else if (semantics_name == "CZero3") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(0,0,0)";
    }
    else if (semantics_name == "CZero4") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(0,0,0,0)";
    }
    else if (semantics_name == "CZero5") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(0,0,0,0,0)";
    }
    else if (semantics_name == "CZero6") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(0,0,0,0,0,0)";
    }
    else if (semantics_name == "CFalseZero") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),0)";
    }
    else if (semantics_name == "CFalseZero2") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),0,0)";
    }
    else if (semantics_name == "CBIIB") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),0,0,(toSym False))";
    }
    else if (semantics_name == "CFalse2") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),(toSym False))";
    }
    else if (semantics_name == "CFalse3") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),(toSym False),(toSym False))";
    }
    else if (semantics_name == "CFalse4") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "((toSym False),(toSym False),(toSym False),(toSym False))";
    }
    else if (semantics_name == "CFalse") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(toSym False)";
    }
    else if (semantics_name == "CTrue") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(toSym True)";
    }
    else if (semantics_name == "Unit") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "Unit";
    }
    else if (semantics_name == "CIte") {
        if (param_list.size() != 3) {
            std::cout << "error: param size != 3" << std::endl;
        }
        res += "if";
        res += " ((evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ") == (toSym True))";
        res += " then";
        res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[1])
            + " env " + param_name[1] + ")";
        res += " else";
        res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[2])
            + " env " + param_name[2] + ")";
    }
    // Access1_2_0_3
    // get 1(second) element from expr2, length of expr2 can get from tuple_len[2]
    else if (semantics_name.substr(0, 6) == "Access") {
        if (param_list.size() != 1) {
            std::cout << "error: param size != 1" << std::endl;
            return res;
        }
        int num_in_tuple = semantics_name[6] - '0' + 1;
        int expr_num = semantics_name[8] - '0';
        int total_len = tuple_len[expr_num];
        if (total_len == 2) {
            if (num_in_tuple == 1) res += "fst ";
            else res += "snd ";
        } else {
            res += "get" + std::to_string(num_in_tuple) + "from" + std::to_string(total_len) + " ";
        }
        /*if (semantics_name[6] == '0') res += "getFirst ";
        else if (semantics_name[6] == '1') res += "getSecond ";
        else if (semantics_name[6] == '2') res += "getThird ";*/
        res += "(evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ")";
    }
    else if (semantics_name.substr(0, 4) == "Prod") {
        res += "(";
        for (int i = 0; i < param_num.size(); ++i) {
            if (i) res += ", ";
            res += "(evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[i])
                + " env " + param_name[i] + ")";
        }
        res += ")";
    }
    else if (semantics_name.substr(0, 5) == "Param") {
        int param_num = semantics_name[5] - '0';
        int num_in_type_list = -1;
        for (int i = 0; i < env_type_list.size(); ++i) {
            if (oup_type == env_type_list[i].first->getHaskellName()) {
                num_in_type_list = i;
                break;
            }
        }
        if (num_in_type_list == -1) {
            LOG(FATAL) << "Unexpected type in Param" << std::to_string(param_num) << ", type = " << oup_type;
        }
        res += "evalVar";
        res += std::to_string(num_in_type_list);
        res += (" env \"" + var_list[param_num] + "\"");
    }
    else if (semantics_name.substr(0, 6) == "Al_inf") {
        if (param_list.size() != 0) {
            std::cout << "error: param size != 0" << std::endl;
            return res;
        }
        res += "(100 :: SymInteger)";
    }
    else {
        if (semantics_name == "Max") semantics_name = "max'";
        else if (semantics_name == "Min") semantics_name = "min'";
        else if (semantics_name == "Sum") semantics_name = "sum'";
        else if (semantics_name == "Div") semantics_name = "div'";
        else {
            semantics_name[0] = std::tolower(semantics_name[0]);
        }
        res += semantics_name;
        for (int i = 0; i < param_list.size(); ++i) {
            res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[i])
                + " env " + param_name[i] + ")";
        }
    }
    return res;
}

PProgram ComposedRule::buildProgram(const ProgramList &sub_list) {
    return program::rewriteParam(composed_sketch, sub_list);
}

Rule * ComposedRule::clone(const NTList &new_param_list) {
    return new ComposedRule(composed_sketch, new_param_list);
}

namespace {
    ComposedSemantics* _getComposedSemantics(Rule* rule) {
        auto* cr = dynamic_cast<ConcreteRule*>(rule);
        if (cr) return dynamic_cast<ComposedSemantics*>(cr->semantics.get());
        return nullptr;
    }
}

Grammar * ext::grammar::rewriteComposedRule(Grammar *g) {
    g->indexSymbol(); int n = g->symbol_list.size();
    NTList symbol_list(n);
    for (int i = 0; i < n; ++i) {
        symbol_list[i] = new NonTerminal(g->symbol_list[i]->name, g->symbol_list[i]->type);
    }
    for (int symbol_id = 0; symbol_id < n; ++symbol_id) {
        auto* symbol = symbol_list[symbol_id];
        for (auto* rule: g->symbol_list[symbol_id]->rule_list) {
            ComposedSemantics* cs = _getComposedSemantics(rule);
            NTList param_list(rule->param_list.size());
            for (int i = 0; i < param_list.size(); ++i) {
                param_list[i] = symbol_list[rule->param_list[i]->id];
            }
            if (!cs) {
                symbol->rule_list.push_back(rule->clone(param_list));
                continue;
            }
            assert(cs->param_num == rule->param_list.size());
            symbol->rule_list.push_back(new ComposedRule(cs->body, param_list));
        }
    }
    return new Grammar(symbol_list[g->start->id], symbol_list);
}