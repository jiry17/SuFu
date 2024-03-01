//
// Created by pro on 2022/3/9.
//

#include "istool/sygus/parser/parser.h"
#include "istool/sygus/parser/json_util.h"
#include "glog/logging.h"
#include <unordered_set>
#include <queue>

namespace {
    struct _GrammarDepthInfo {
    public:
        std::unordered_map<std::string, int> nt_depth_map;
        std::unordered_map<std::string, std::vector<std::vector<int>>> rule_depth_map;
        _GrammarDepthInfo(const std::unordered_map<std::string, int>& _nt_depth_map={},
                const std::unordered_map<std::string, std::vector<std::vector<int>>>& _rule_depth_map={}):
            nt_depth_map(_nt_depth_map), rule_depth_map(_rule_depth_map) {
        }
    };

    const int KINF = 1e9;

    std::pair<std::string, int> _getDepthLimitedSymbol(const std::string& full_name) {
        auto pos = full_name.find('@');
        if (pos == std::string::npos) return {full_name, KINF};
        auto name = full_name.substr(0, int(pos));
        int limit = std::stoi(full_name.substr(int(pos) + 1));
        return {name, limit};
    }

    bool _isSymbol(const std::string& full_name, const std::unordered_set<std::string>& symbol_set) {
        auto pos = full_name.find('@');
        if (pos == std::string::npos) return symbol_set.find(full_name) != symbol_set.end();
        auto name = full_name.substr(0, int(pos));
        if (symbol_set.find(name) == symbol_set.end()) return false;
        for (int i = int(pos) + 1; i < full_name.length(); ++i) {
            if (full_name[i] < '0' || full_name[i] > '9') return false;
        }
        return true;
    }

    bool _isOperatorRule(const Json::Value& node, const std::unordered_set<std::string>& symbol_set) {
        if (!node.isArray()) return false;
        for (int i = 0; i < node.size(); ++i) {
            if (!node[i].isString()) return false;
            if (i && !_isSymbol(node[i].asString(), symbol_set)) return false;
        }
        return true;
    }

    std::pair<_GrammarDepthInfo, Json::Value> _dealDepthLimit(const Json::Value& value) {
        int size = value.size();
        Json::Value res, grammar(value[size - 1]);
        for (int i = 0; i + 1 < size; ++i) res.append(value[i]);
        std::unordered_map<std::string, int> nt_depth_map;
        std::unordered_map<std::string, std::vector<std::vector<int>>> rule_depth_map;
        std::unordered_set<std::string> symbol_set;
        for (const auto& nt_node: grammar) symbol_set.insert(_getDepthLimitedSymbol(nt_node[0].asString()).first);

        Json::Value new_grammar;
        for (const auto& nt_node: grammar) {
            Json::Value new_node;
            auto node_info = _getDepthLimitedSymbol(nt_node[0].asString());
            if (nt_depth_map.find(node_info.first) != nt_depth_map.end()) {
                LOG(FATAL) << "Duplicated symbol " << node_info.first;
            }
            nt_depth_map[node_info.first] = node_info.second;
            new_node.append(node_info.first);
            new_node.append(nt_node[1]);

            Json::Value rule_list;
            std::vector<std::vector<int>> rule_depth_list;
            for (const auto& rule: nt_node[2]) {
                if (rule.isString()) {
                    auto rule_name = rule.asString();
                    auto sub_symbol_info = _getDepthLimitedSymbol(rule_name);
                    if (symbol_set.find(sub_symbol_info.first) != symbol_set.end()) {
                        rule_list.append(sub_symbol_info.first);
                        rule_depth_list.push_back({sub_symbol_info.second});
                        continue;
                    }
                }

                if (!_isOperatorRule(rule, symbol_set)) {
                    rule_list.append(rule);
                    rule_depth_list.emplace_back();
                    continue;
                }

                Json::Value new_rule; new_rule.append(rule[0]);
                std::vector<int> depth_limit_list;
                for (int i = 1; i < rule.size(); ++i) {
                    auto sub_node_info = _getDepthLimitedSymbol(rule[i].asString());
                    depth_limit_list.push_back(sub_node_info.second);
                    new_rule.append(sub_node_info.first);
                }
                rule_list.append(new_rule);
                rule_depth_list.push_back(depth_limit_list);
            }
            new_node.append(rule_list);
            new_grammar.append(new_node);
            rule_depth_map[node_info.first] = rule_depth_list;
        }
        res.append(new_grammar);
        _GrammarDepthInfo info(nt_depth_map, rule_depth_map);
        return {info, res};
    }

    Grammar* _rewriteGrammar(Grammar* grammar, const _GrammarDepthInfo& info) {
        std::unordered_map<std::string, NonTerminal*> limited_symbol_map;
        std::queue<std::pair<NonTerminal*, std::pair<NonTerminal*, int>>> Q;
        auto get_limited_symbol = [&](NonTerminal* symbol, int depth) -> NonTerminal*{
            std::string name = symbol->name;
            if (depth != KINF) name += "@" + std::to_string(depth);
            if (limited_symbol_map.find(name) != limited_symbol_map.end()) {
                return limited_symbol_map[name];
            }
            auto* limited_symbol = new NonTerminal(name, symbol->type);
            limited_symbol_map[name] = limited_symbol;
            Q.push({limited_symbol, {symbol, depth}});
            return limited_symbol;
        };

        auto* start = get_limited_symbol(grammar->start, info.nt_depth_map.find(grammar->start->name)->second);
        while (!Q.empty()) {
            auto* current = Q.front().first, *pre = Q.front().second.first;
            int d = Q.front().second.second; Q.pop();
            if (d == 0) continue;
            auto depth_list = info.rule_depth_map.find(pre->name)->second;
            assert(pre->rule_list.size() == depth_list.size());
            for (int i = 0; i < depth_list.size(); ++i) {
                auto* rule = pre->rule_list[i];
                assert(rule->param_list.size() == depth_list[i].size());
                NTList param_list;
                for (int j = 0; j < rule->param_list.size(); ++j) {
                    int new_depth = depth_list[i][j];
                    if (d != KINF) new_depth = std::min(new_depth, d - 1);
                    param_list.push_back(get_limited_symbol(rule->param_list[j], new_depth));
                }
                current->rule_list.push_back(rule->clone(param_list));
            }
        }

        NTList symbol_list;
        for (auto& item: limited_symbol_map) symbol_list.push_back(item.second);
        auto* res = new Grammar(start, symbol_list);
        delete grammar;
        return res;
    }
}


Specification * parser::getDepthLimitedSyGuSSpecFromJson(const Json::Value &value) {
    Json::Value normalized_root;
    std::unordered_map<std::string, _GrammarDepthInfo> info_map;
    for (const auto& entry: value) {
        if (entry[0].asString() == "synth-fun") {
            auto name = entry[1].asString();
            auto res = _dealDepthLimit(entry);
            normalized_root.append(res.second);
            info_map[name] = res.first;
        } else normalized_root.append(entry);
    }
    auto* spec = getSyGuSSpecFromJson(normalized_root);

    for (auto& info: spec->info_list) {
        info->grammar = _rewriteGrammar(info->grammar, info_map[info->name]);
        LOG(INFO) << "Limited grammar for target " << info->name;
        info->grammar->print();
    }
    return spec;
}