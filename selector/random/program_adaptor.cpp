//
// Created by pro on 2022/5/5.
//

#include "istool/selector/random/program_adaptor.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

namespace {
    bool _isInteger(Program* p) {
        auto* cs = dynamic_cast<ConstSemantics*>(p->semantics.get());
        if (!cs) return false;
        return dynamic_cast<IntValue*>(cs->w.get());
    }

    int _getInteger(Program* p) {
        auto* cs = dynamic_cast<ConstSemantics*>(p->semantics.get());
        return theory::clia::getIntValue(cs->w);
    }

    PProgram _programAdaptorWithLIARules(NonTerminal* symbol, Program* p, Grammar* g, std::unordered_map<std::string, PProgram>& cache) {
        auto feature = std::to_string(symbol->id) + "@" + p->toString();
        if (cache.count(feature)) {
            return cache[feature];
        }
        for (auto* rule: symbol->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "Current implementation of ProgramAdaptor requires ConcreteRule";
            if (cr->semantics->name != p->semantics->name) continue;
            bool flag = true; ProgramList sub_list(rule->param_list.size());
            for (int i = 0; i < rule->param_list.size(); ++i) {
                auto res = _programAdaptorWithLIARules(rule->param_list[i], p->sub_list[i].get(), g, cache);
                if (!res) {
                    flag = false; break;
                }
                sub_list[i] = res;
            }
            if (flag) return cache[feature] = std::make_shared<Program>(cr->semantics, sub_list);
        }

        // special treatment for constants
        if (_isInteger(p)) {
            int w = _getInteger(p);
            if (w > 0) {
                for (auto *rule: symbol->rule_list) {
                    auto* cr = dynamic_cast<ConcreteRule*>(rule);
                    if (!cr) LOG(FATAL) << "Current implementation of ProgramAdaptor requires ConcreteRule";
                    if (cr->semantics->name == "+") {
                        for (int i = w / 2; i; --i) {
                            auto px = program::buildConst(BuildData(Int, i)), py = program::buildConst(BuildData(Int, w - i));
                            auto res_x = _programAdaptorWithLIARules(rule->param_list[0], px.get(), g, cache);
                            auto res_y = _programAdaptorWithLIARules(rule->param_list[1], py.get(), g, cache);
                            if (res_x && res_y) {
                                auto res = std::make_shared<Program>(cr->semantics, (ProgramList){res_x, res_y});
                                return cache[feature] = res;
                            }
                        }
                    }
                }
            } else if (w < 0) {
                for (auto* rule: symbol->rule_list) {
                    auto* cr = dynamic_cast<ConcreteRule*>(rule);
                    if (!cr) LOG(FATAL) << "Current implementation of ProgramAdaptor requires ConcreteRule";
                    if (cr->semantics->name == "-") {
                        auto px = program::buildConst(BuildData(Int, 0)), py = program::buildConst(BuildData(Int, -w));
                        auto res_x = _programAdaptorWithLIARules(rule->param_list[0], px.get(), g, cache);
                        auto res_y = _programAdaptorWithLIARules(rule->param_list[1], py.get(), g, cache);
                        if (res_x && res_y) {
                            auto res = std::make_shared<Program>(cr->semantics, (ProgramList){res_x, res_y});
                            return cache[feature] = res;
                        }
                    }
                }
            }
        }
        // special treatment for constant times a term
        if (p->semantics->getName() == "*") {
            auto pl = p->sub_list[0], pr = p->sub_list[1];
            if (_isInteger(pl.get()) || _isInteger(pr.get())) {
                if (!_isInteger(pl.get())) std::swap(pl, pr);
                int lw = _getInteger(pl.get());
                if (lw == 1) {
                    auto res = _programAdaptorWithLIARules(symbol, pr.get(), g, cache);
                    if (res) return cache[feature] = res;
                } else if (lw > 1) {
                    for (auto* rule: symbol->rule_list) {
                        auto* cr = dynamic_cast<ConcreteRule*>(rule);
                        if (!cr) LOG(FATAL) << "Current implementation of ProgramAdaptor requires ConcreteRule";
                        if (cr->semantics->getName() != "+") continue;
                        auto px = program::buildConst(BuildData(Int, lw / 2)), py = program::buildConst(
                                BuildData(Int, (lw + 1) / 2));
                        px = std::make_shared<Program>(p->semantics, (ProgramList) {px, pr});
                        py = std::make_shared<Program>(p->semantics, (ProgramList) {py, pr});
                        auto res_x = _programAdaptorWithLIARules(rule->param_list[0], px.get(), g, cache);
                        auto res_y = _programAdaptorWithLIARules(rule->param_list[1], py.get(), g, cache);
                        if (res_x && res_y) return cache[feature] = std::make_shared<Program>(cr->semantics, (ProgramList){res_x, res_y});
                    }
                } else if (lw < 0) {
                    for (auto* rule: symbol->rule_list) {
                        auto* cr = dynamic_cast<ConcreteRule*>(rule);
                        if (!cr) LOG(FATAL) << "Current implementation of ProgramAdaptor requires ConcreteRule";
                        if (cr->semantics->name == "-") {
                            auto px = program::buildConst(BuildData(Int, 0)), py = program::buildConst(BuildData(Int, -lw));
                            py = std::make_shared<Program>(p->semantics, (ProgramList){py, pr});
                            auto res_x = _programAdaptorWithLIARules(rule->param_list[0], px.get(), g, cache);
                            auto res_y = _programAdaptorWithLIARules(rule->param_list[1], py.get(), g, cache);
                            if (res_x && res_y) return cache[feature] = std::make_shared<Program>(cr->semantics, (ProgramList){res_x, res_y});
                        }
                    }
                }
            }
        }
        LOG(INFO) << "fail " << feature;
        return cache[feature] = nullptr;
    }
}

PProgram selector::adaptor::programAdaptorWithLIARules(Program* p, Grammar *g) {
    g->indexSymbol();
    std::unordered_map<std::string, PProgram> cache;
    auto res = _programAdaptorWithLIARules(g->start, p, g, cache);
    if (!res) LOG(WARNING) << "The adaption for program " << p->toString() << " fails";
    return res;
}