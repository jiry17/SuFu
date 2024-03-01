//
// Created by pro on 2021/12/4.
//

#include "istool/basic/grammar.h"
#include "glog/logging.h"
#include <set>
#include <queue>
#include <unordered_set>
#include <iostream>

NonTerminal::NonTerminal(const std::string &_name, const PType& _type): name(_name), type(_type) {
}

NonTerminal::~NonTerminal() {
    for (auto* r: rule_list) delete r;
}

Rule::Rule(const NTList& _param_list): param_list(_param_list) {
}

ConcreteRule::ConcreteRule(const PSemantics &_semantics, const NTList &_param_list):
    semantics(_semantics), Rule(_param_list) {
}
std::string ConcreteRule::toString() const {
    std::string res = semantics->getName();
    if (param_list.empty()) return res;
    for (int i = 0; i < param_list.size(); ++i) {
        if (i) res += " "; else res += "(";
        res += param_list[i]->name;
    }
    return res + ")";
}

std::string ConcreteRule::getSemanticsName() const {
    return semantics->getName();
}

std::string ConcreteRule::toHaskell(std::unordered_map<std::string, int>& name_to_expr_num, int& next_expr_num, int func_num, std::string &node_name) {
    std::string res = semantics->getName();
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
    else if (res == "1") res = "Cone";
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
    else if (res == "ite") res = "CIte";
    else if (res == "false") res = "CFalse";
    else if (res == "true") res = "CTrue";
    else if (res == "left") res = "Left'";
    else if (res == "right") res = "Right'";
    else if (!(res[0] >= 'a' && res[0] <= 'z' || res[0] >= 'A' && res[0] <= 'Z')) {
        std::cout << "error: res is not a letter!, res = " << res << std::endl;
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
    semantics->name = res;

    res += std::to_string(func_num) + "_" + std::to_string(name_to_expr_num[node_name]);

    if (param_list.empty()) return res;
    for (int i = 0; i < param_list.size(); ++i) {
        res += " ";
        if (name_to_expr_num.find(param_list[i]->name) == name_to_expr_num.end()) {
            name_to_expr_num[param_list[i]->name] = next_expr_num++;
        }
        res += "(UnionM Expr" + std::to_string(func_num) + "_" + std::to_string(name_to_expr_num[param_list[i]->name]) + ")";
    }
    return res;
}

std::string ConcreteRule::evalRuleToHaskell(std::string node_name, int func_num,
        std::unordered_map<std::string, int>& name_to_expr_num,
        std::vector<std::string>& var_list,
        std::string oup_type, std::vector<std::pair<PType, int> > &env_type_list,
        std::vector<int> &tuple_len) {
    std::vector<int> param_num;
    std::vector<std::string> param_name;
    std::string semantics_name = semantics->name;

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
        // mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
        res += " mrgIte ((evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ") ==~ (toSym True)) (toSym False) (toSym True)";
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
        // mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
        res += "mrgIte";
        res += " ((evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[0])
            + " env " + param_name[0] + ") ==~ (toSym True))";
        res += " (evalU" + std::to_string(func_num) + "_" + std::to_string(param_num[1])
            + " env " + param_name[1] + ")";
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

PProgram ConcreteRule::buildProgram(const ProgramList &sub_list) {
    return std::make_shared<Program>(semantics, sub_list);
}
Rule * ConcreteRule::clone(const NTList &new_param_list) {
    return new ConcreteRule(semantics, new_param_list);
}

Grammar::Grammar(NonTerminal *_start, const NTList &_symbol_list, bool is_remove_empty): start(_start), symbol_list(_symbol_list) {
    int pos = -1;
    for (int i = 0; i < symbol_list.size(); ++i) {
        if (symbol_list[i]->name == start->name) {
            pos = i;
        }
    }
    if (pos == -1) {
        LOG(FATAL) << "Start symbol (" << start->name << ") not found";
    }
    std::swap(symbol_list[0], symbol_list[pos]);
    if (is_remove_empty) removeUseless();
}
Grammar::~Grammar() {
    for (auto* symbol: symbol_list) delete symbol;
}

namespace {
    void _dfs(NonTerminal* symbol, std::vector<bool>& is_visited) {
        if (is_visited[symbol->id]) return;
        is_visited[symbol->id] = true;
        for (auto* rule: symbol->rule_list) {
            for (auto* sub: rule->param_list) _dfs(sub, is_visited);
        }
    }
}

void Grammar::removeUseless() {
    indexSymbol();
    std::vector<bool> is_empty(symbol_list.size(), true);
    while (1) {
        bool is_converged = true;
        for (auto* symbol: symbol_list) {
            if (!is_empty[symbol->id]) continue;
            for (auto* r: symbol->rule_list) {
                bool flag = false;
                for (auto* sub: r->param_list) {
                    if (is_empty[sub->id]) {
                        flag = true;
                        break;
                    }
                }
                if (!flag) {
                    is_empty[symbol->id] = false;
                    is_converged = false;
                }
            }
        }
        if (is_converged) break;
    }
    if (is_empty[0]) {
        for (int i = 1; i < symbol_list.size(); ++i) delete symbol_list[i];
        symbol_list.resize(1);
        for (auto* rule: start->rule_list) delete rule;
        start->rule_list.clear();
        return;
    }
    for (auto* node: symbol_list) {
        int now = 0;
        for (auto* rule: node->rule_list) {
            bool flag = false;
            for (auto* sub: rule->param_list) {
                if (is_empty[sub->id]) {
                    flag = true; break;
                }
            }
            if (flag) delete rule;
            else node->rule_list[now++] = rule;
        }
        node->rule_list.resize(now);
    }
    std::vector<bool> is_visited(symbol_list.size(), false);
    _dfs(start, is_visited);
    int now = 0;
    for (auto* symbol: symbol_list) {
        if (is_visited[symbol->id]) symbol_list[now++] = symbol;
        else delete symbol;
    }
    symbol_list.resize(now);
}

void Grammar::indexSymbol() const {
    for (int i = 0; i < symbol_list.size(); ++i) {
        symbol_list[i]->id = i;
    }
}

void Grammar::print() const {
    std::cout << "start: " << start->name << std::endl;
    for (auto* node: symbol_list) {
        std::cout << "node: " << node->name << std::endl;
        for (auto *rule: node->rule_list) {
            std::cout << "  " << rule->toString() << std::endl;
        }
    }
}

namespace {
    bool _getSymbolDepth(NonTerminal* symbol, std::unordered_map<NonTerminal*, int>& cache, std::unordered_set<NonTerminal*>& stack_set) {
        if (stack_set.find(symbol) != stack_set.end()) return true;
        if (cache.find(symbol) != cache.end()) return cache[symbol];
        stack_set.insert(symbol);
        int res = 0;
        for (auto* rule: symbol->rule_list) {
            for (auto* sub: rule->param_list) {
                if (_getSymbolDepth(sub, cache, stack_set)) return true;
                res = std::max(res, cache[sub] + 1);
            }
        }
        return cache[symbol] = res;
    }

    int _getSymbolDepth(NonTerminal* symbol) {
        std::unordered_map<NonTerminal*, int> cache;
        std::unordered_set<NonTerminal*> stack_set;
        if (_getSymbolDepth(symbol, cache, stack_set)) return 1e9;
        return cache[symbol];
    }
}

Grammar * grammar::generateHeightLimitedGrammar(Grammar *grammar, int limit) {
    grammar->indexSymbol();
    std::vector<NTList> symbol_pool(limit + 1, NTList(grammar->symbol_list.size(), nullptr));
    std::vector<int> height_list;
    for (auto* symbol: grammar->symbol_list) {
        height_list.push_back(_getSymbolDepth(symbol));
    }
    for (int h = 0; h <= limit; ++h) {
        for (int i = 0; i < grammar->symbol_list.size(); ++i) {
            auto* symbol = grammar->symbol_list[i];
            if (h > height_list[i]) {
                symbol_pool[h][i] = symbol_pool[h - 1][i];
                continue;
            }
            auto* new_symbol = new NonTerminal(symbol->name + "@" + std::to_string(h), symbol->type);
            symbol_pool[h][i] = new_symbol;
            for (auto* rule: symbol->rule_list) {
                if (!h && !rule->param_list.empty()) continue;
                NTList sub_list;
                for (auto* param: rule->param_list) {
                    sub_list.push_back(symbol_pool[h - 1][param->id]);
                }
                new_symbol->rule_list.push_back(rule->clone(sub_list));
            }
        }
    }
    NTList symbol_list;
    for (auto* symbol: grammar->symbol_list) {
        for (int i = 0; i <= limit && i <= height_list[symbol->id]; ++i) {
            symbol_list.push_back(symbol_pool[i][symbol->id]);
        }
    }
    auto g = new Grammar(symbol_pool[limit][0], symbol_list);
    return g;
}

Grammar * grammar::copyGrammar(Grammar *grammar) {
    int n = grammar->symbol_list.size();
    NTList symbols(n); grammar->indexSymbol();
    for (auto* symbol: grammar->symbol_list) {
        auto* new_symbol = new NonTerminal(symbol->name, symbol->type);
        symbols[symbol->id] = new_symbol;
    }
    for (auto* symbol: grammar->symbol_list) {
        for (auto* rule: symbol->rule_list) {
            NTList param_list;
            for (auto* sub_node: rule->param_list) {
                param_list.push_back(symbols[sub_node->id]);
            }
            symbols[symbol->id]->rule_list.push_back(rule->clone(param_list));
        }
    }
    auto* res = new Grammar(symbols[grammar->start->id], symbols);
    return res;
}

namespace {
    bool _isUsedName(Grammar* g, const std::string& name) {
        for (auto* symbol: g->symbol_list) {
            if (symbol->name == name) return true;
        }
        return false;
    }
}

std::string grammar::getFreeName(Grammar *grammar) {
    int id = 0;
    while (_isUsedName(grammar, "tmp@" + std::to_string(id))) ++id;
    return "tmp@" + std::to_string(id);
}

namespace {
    const int KINF = 1e9;
}

ConstSemantics * grammar::getConstSemantics(Rule *rule) {
    auto* cr = dynamic_cast<ConcreteRule*>(rule);
    if (cr) return dynamic_cast<ConstSemantics*>(cr->semantics.get());
    return nullptr;
}

ParamSemantics* grammar::getParamSemantics(Rule *rule) {
    auto* cr = dynamic_cast<ConcreteRule*>(rule);
    if (cr) return dynamic_cast<ParamSemantics*>(cr->semantics.get());
    return nullptr;
}

PProgram grammar::getMinimalProgram(Grammar *grammar) {
    grammar->indexSymbol();
    int n = grammar->symbol_list.size();
    std::vector<int> d(n, KINF);
    ProgramList res(n, nullptr);
    std::priority_queue<std::pair<int, int>> Q;

    std::vector<std::vector<std::pair<NonTerminal*, Rule*>>> reversed_edge(n);
    for (auto* symbol: grammar->symbol_list) {
        for (auto* rule: symbol->rule_list) {
            for (auto* sub: rule->param_list) {
                reversed_edge[sub->id].emplace_back(symbol, rule);
            }
        }
    }

    for (auto* symbol: grammar->symbol_list) {
        bool is_start = false;
        for (auto* rule: symbol->rule_list) {
            if (rule->param_list.empty()) {
                ProgramList empty_list;
                res[symbol->id] = rule->buildProgram(empty_list);
                d[symbol->id] = 1; is_start = true;
                break;
            }
        }
        if (is_start) Q.push({d[symbol->id], symbol->id});
    }

    while (!Q.empty()) {
        auto k = Q.top(); Q.pop();
        if (d[k.second] != k.first) continue;
        for (auto& [node, edge]: reversed_edge[k.second]) {
            ProgramList sub_list;
            int size = 1;
            for (auto* sub: edge->param_list) {
                size = std::min(d[node->id], size + d[sub->id]);
                sub_list.push_back(res[sub->id]);
            }
            if (size < d[node->id]) {
                d[node->id] = size;
                res[node->id] = edge->buildProgram(sub_list);
                Q.push({d[node->id], node->id});
            }
        }
    }
    return res[0];
}

bool grammar::isFinite(Grammar *grammar) {
    grammar->indexSymbol();
    int n = grammar->symbol_list.size();
    std::vector<bool> flag(n, false);
    for (int _ = 0; _ < n; _++) {
        bool is_found = false;
        for (auto* symbol: grammar->symbol_list) {
            if (flag[symbol->id]) continue;
            bool is_full = true;
            for (auto* rule: symbol->rule_list) {
                for (auto* sub: rule->param_list) {
                    is_full &= flag[sub->id];
                }
            }
            if (is_full) {
                flag[symbol->id] = true; is_found = true; break;
            }
        }
        if (!is_found) return false;
    }
    return true;
}

int grammar::getMaxSize(Grammar *grammar) {
    grammar->indexSymbol();
    int n = grammar->symbol_list.size();
    std::vector<int> max_size_list(n, -1);
    for (int _ = 0; _ < n; _++) {
        NonTerminal* finished_symbol = nullptr;
        for (auto* symbol: grammar->symbol_list) {
            if (max_size_list[symbol->id] != -1) continue;
            bool is_finished = true;
            for (auto* rule: symbol->rule_list) {
                for (auto* sub: rule->param_list) {
                    if (max_size_list[sub->id] == -1) {
                        is_finished = false; break;
                    }
                }
            }
            if (is_finished) {
                finished_symbol = symbol; break;
            }
        }
        if (!finished_symbol) return -1;
        max_size_list[finished_symbol->id] = 0;
        for (auto* rule: finished_symbol->rule_list) {
            int total_size = 1;
            for (auto* sub: rule->param_list) {
                total_size += max_size_list[sub->id];
            }
            max_size_list[finished_symbol->id] = std::max(max_size_list[finished_symbol->id], total_size);
        }
    }
    return max_size_list[grammar->start->id];
}