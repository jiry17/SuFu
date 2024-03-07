//
// Created by pro on 2021/12/27.
//

#include "istool/solver/enum/enum.h"
#include <queue>
#include <unordered_set>
#include "glog/logging.h"

namespace {
    int indexAllNT(const std::vector<PSynthInfo>& info_list) {
        int id = 0;
        for (const auto& info: info_list) {
            for (auto* symbol: info->grammar->symbol_list) {
                symbol->id = id++;
            }
        }
        return id;
    }

    void _getAllSizeScheme(int pos, int sum, const std::vector<std::vector<int>>& pool, std::vector<int>& tmp, std::vector<std::vector<int>>& res) {
        if (pos == pool.size()) {
            if (sum == 0) res.push_back(tmp);
            return;
        }
        for (auto size: pool[pos]) {
            if (size > sum) continue;
            tmp.push_back(size);
            _getAllSizeScheme(pos + 1, sum - size, pool, tmp, res);
            tmp.pop_back();
        }
    }

    std::vector<std::vector<int>> getAllSizeScheme(const std::vector<std::vector<int>>& size_pool, int sum) {
        std::vector<std::vector<int>> res;
        std::vector<int> tmp;
        _getAllSizeScheme(0, sum, size_pool, tmp, res);
        return res;
    }

    void buildAllCombination(int pos,  const ProgramStorage& pool, ProgramList& tmp, ProgramStorage& res) {
        if (pos == pool.size()) {
            res.push_back(tmp); return;
        }
        for (const auto& p: pool[pos]) {
            tmp.push_back(p);
            buildAllCombination(pos + 1, pool, tmp, res);
            tmp.pop_back();
        }
    }


    ProgramStorage merge(const std::vector<int>& id_list, int size, const std::vector<ProgramStorage>& storage_list) {
        std::vector<std::vector<int> > size_pool;
        for (int id: id_list) {
            std::vector<int> size_list;
            for (int i = 0; i < size; ++i) {
                const auto& pl = storage_list[id][i];
                if (!pl.empty()) size_list.push_back(i);
            }
            size_pool.push_back(size_list);
        }
        ProgramStorage res;
        for (const auto& scheme: getAllSizeScheme(size_pool, size - 1)) {
            ProgramStorage pool;
            for (int i = 0; i < id_list.size(); ++i) {
                pool.push_back(storage_list[id_list[i]][scheme[i]]);
            }
            ProgramList tmp;
            buildAllCombination(0, pool, tmp, res);
        }
        return res;
    }

    bool _isDirectRule(Rule* rule) {
        auto* cr = dynamic_cast<ConcreteRule*>(rule);
        return cr && dynamic_cast<DirectSemantics*>(cr->semantics.get());
    }

    std::vector<NonTerminal*> _getDirectOrder(Grammar* g) {
        std::unordered_map<NonTerminal*, std::vector<NonTerminal*>> edge_map;
        std::unordered_map<NonTerminal*, int> d;
        for (auto* s: g->symbol_list) {
            for (auto* r: s->rule_list) {
                if (_isDirectRule(r)) {
                    d[s]++; edge_map[r->param_list[0]].push_back(s);
                }
            }
        }
        std::vector<NonTerminal*> res;
        std::queue<NonTerminal*> Q;
        for (auto* s: g->symbol_list) if (!d[s]) Q.push(s);
        while (!Q.empty()) {
            auto* s = Q.front(); Q.pop();
            res.push_back(s);
            for (auto* t: edge_map[s]) {
                --d[t]; if (d[t] == 0) Q.push(t);
            }
        }
        if (res.size() != g->symbol_list.size()) {
            LOG(FATAL) << "Cyclic direct relation in the grammar";
        }
        return res;
    }
}

FunctionContext solver::enumerate(const std::vector<PSynthInfo> &info_list, const EnumConfig &c) {
    auto* v = c.v; auto* o = c.o; o->clear();
    int n = indexAllNT(info_list);
    std::vector<ProgramStorage> storage_list(n);
    for (auto& ps: storage_list) ps.emplace_back();
    std::vector<FunctionContext> res;
    std::vector<NTList> direct_order_list;
    for (const auto& info: info_list) direct_order_list.push_back(_getDirectOrder(info->grammar));

    for (int size = 1; size <= c.size_limit; ++size) {
        TimeCheck(c.guard);
        for (int pos = 0; pos < info_list.size(); ++pos) {
            auto& info = info_list[pos];
            for (auto* symbol: direct_order_list[pos]) {
                int id = symbol->id; storage_list[id].emplace_back();
                for (auto* rule: symbol->rule_list) {
                    if (_isDirectRule(rule)) {
                        for (const auto& p: storage_list[rule->param_list[0]->id][size]) {
                            if (!o->isDuplicated(info->name, symbol, p)) {
                                storage_list[symbol->id][size].push_back(p);
                            }
                        }
                    } else {
                        std::vector<int> sub_id_list;
                        for (auto *sub_symbol: rule->param_list) sub_id_list.push_back(sub_symbol->id);
                        ProgramStorage tmp = merge(sub_id_list, size, storage_list);
                        for (const auto &sub_list: tmp) {
                            TimeCheck(c.guard);
                            auto p = rule->buildProgram(sub_list);
                            if (o->isDuplicated(info->name, symbol, p))
                                continue;
                            storage_list[id][size].push_back(p);
                        }
                    }
                }
            }
        }
        int merge_size = int(info_list.size()) + size;
        std::vector<int> sub_id_list;
        for (const auto& info: info_list) {
            auto* start = info->grammar->start;
            sub_id_list.push_back(start->id);
        }
        ProgramStorage tmp = merge(sub_id_list, merge_size, storage_list);
        for (const auto& sub_list: tmp) {
            TimeCheck(c.guard);
            FunctionContext info;
            for (int i = 0; i < info_list.size(); ++i) {
                info[info_list[i]->name] = sub_list[i];
            }
            if (v->verify(info, nullptr)) return info;
        }
    }
    return {};
}

EnumConfig::EnumConfig(Verifier *_v, Optimizer *_o, TimeGuard* _guard): v(_v), o(_o), guard(_guard) {
}