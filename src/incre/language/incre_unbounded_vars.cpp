//
// Created by pro on 2022/9/24.
//

#include "istool/incre/language/incre.h"
#include <unordered_set>
#include "glog/logging.h"

using namespace incre;

namespace {
    void _getUnboundedVars(TermData* term, std::unordered_set<std::string>& res, std::vector<std::string>& tmps);

#define GetHead(name) void __getUnboundedVars(Tm ## name* term, std::unordered_set<std::string>& res, std::vector<std::string>& tmps)
#define GetSub(x) _getUnboundedVars(term->x.get(), res, tmps)
#define GetSub2(x) _getUnboundedVars(x.get(), res, tmps)
#define GetCase(name) __getUnboundedVars(dynamic_cast<Tm ## name*>(term), res, tmps); break

    GetHead(Var) {
        for (const auto& name: tmps) {
            if (name == term->name) return;
        }
        res.insert(term->name);
    }
    GetHead(Fix) {
        GetSub(content);
    }
    GetHead(Label) {
        GetSub(content);
    }
    GetHead(UnLabel) {
        GetSub(content);
    }
    GetHead(Align) {
        GetSub(content);
    }
    GetHead(Proj) {
        GetSub(content);
    }
    GetHead(Tuple) {
        for (const auto& field: term->fields) GetSub2(field);
    }
    GetHead(Let) {
        GetSub(def);
        tmps.push_back(term->name);
        GetSub(content);
        tmps.pop_back();
    }
    GetHead(Abs) {
        tmps.push_back(term->name);
        GetSub(content);
        tmps.pop_back();
    }
    GetHead(App) {
        GetSub(func); GetSub(param);
    }
    void _collectNames(PatternData* pt, std::vector<std::string>& tmps) {
        switch (pt->getType()) {
            case PatternType::TUPLE: {
                auto* tp = dynamic_cast<PtTuple*>(pt);
                for (auto& sub: tp->pattern_list) _collectNames(sub.get(), tmps);
                return;
            }
            case PatternType::UNDER_SCORE: return;
            case PatternType::VAR: {
                auto* vp = dynamic_cast<PtVar*>(pt);
                tmps.push_back(vp->name);
                return;
            }
            case PatternType::CONSTRUCTOR: {
                auto* cp = dynamic_cast<PtConstructor*>(pt);
                _collectNames(cp->pattern.get(), tmps);
                return;
            }
        }
    }
    GetHead(Match) {
        GetSub(def);
        for (const auto& [pattern, branch]: term->cases) {
            int len = tmps.size();
            _collectNames(pattern.get(), tmps);
            GetSub2(branch);
            tmps.resize(len);
        }
    }
    GetHead(If) {
        GetSub(c); GetSub(t); GetSub(f);
    }


    void _getUnboundedVars(TermData* term, std::unordered_set<std::string>& res, std::vector<std::string>& tmps) {
        switch (term->getType()) {
            case TermType::VALUE: return;
            case TermType::VAR: GetCase(Var);
            case TermType::FIX: GetCase(Fix);
            case TermType::LABEL: GetCase(Label);
            case TermType::UNLABEL: GetCase(UnLabel);
            case TermType::ALIGN: GetCase(Align);
            case TermType::PROJ: GetCase(Proj);
            case TermType::TUPLE: GetCase(Tuple);
            case TermType::LET: GetCase(Let);
            case TermType::ABS: GetCase(Abs);
            case TermType::APP: GetCase(App);
            case TermType::MATCH: GetCase(Match);
            case TermType::IF: GetCase(If);
            case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << term->toString();
        }
    }
}

std::vector<std::string> incre::getUnboundedVars(TermData* term) {
    std::unordered_set<std::string> res;
    std::vector<std::string> tmps;
    _getUnboundedVars(term, res, tmps);
    return {res.begin(), res.end()};
}