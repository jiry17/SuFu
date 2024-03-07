//
// Created by pro on 2023/1/25.
//

#include "istool/incre/language/incre.h"
#include "istool/incre/language/incre_lookup.h"
#include "glog/logging.h"
#include <unordered_set>
#include <iostream>

using namespace incre;

namespace {

    bool _isLeafTerm(const Term& term) {
        if (term->getType() == TermType::ALIGN) return true;
        if (term->getType() == TermType::LABEL || term->getType() == TermType::UNLABEL) {
            return false;
        }
        auto sub_terms = incre::getSubTerms(term.get());
        for (auto& sub_term: sub_terms) {
            if (!_isLeafTerm(sub_term)) return false;
        }
        return true;
    }

    bool _isOverlap(const std::vector<std::string>& x, const std::vector<std::string>& y) {
        std::unordered_set<std::string> x_set;
        for (auto& name: x) x_set.insert(name);
        for (auto& name: y) {
            if (x_set.find(name) != x_set.end()) return true;
        }
        return false;
    }

    void _collectMovableTerms(const Term& term, std::vector<std::string>& tmp_names, TermList& leaves) {
        if (_isLeafTerm(term)) {
            if (term->getType() == TermType::VAR) return;
            if (_isOverlap(incre::getUnboundedVars(term.get()), tmp_names)) return;
            leaves.push_back(term);
            return;
        }
        switch (term->getType()) {
            case TermType::VAR:
            case TermType::VALUE:
            case TermType::WILDCARD:
            case TermType::ALIGN:
                LOG(FATAL) << "Unexpected TermType: " << term->toString();
            case TermType::LABEL:
            case TermType::UNLABEL:
            case TermType::APP:
            case TermType::PROJ:
            case TermType::FIX:
            case TermType::TUPLE: {
                auto sub_terms = incre::getSubTerms(term.get());
                for (auto& sub: sub_terms) _collectMovableTerms(sub, tmp_names, leaves);
                return;
            }
            case TermType::ABS: {
                return;
            }
            case TermType::LET: {
                auto* tl = dynamic_cast<TmLet*>(term.get());
                auto content = tl->content;
                tmp_names.push_back(tl->name);
                _collectMovableTerms(tl->content, tmp_names, leaves);
                tmp_names.pop_back();
                return;
            }
            case TermType::MATCH: {
                auto* tm = dynamic_cast<TmMatch*>(term.get());
                _collectMovableTerms(tm->def, tmp_names, leaves);
                for (auto& [pt, sub_term]: tm->cases) {
                    auto pt_vars = incre::getPatternVars(pt);
                    for (auto& name: pt_vars) tmp_names.push_back(name);
                    _collectMovableTerms(sub_term, tmp_names, leaves);
                    for (auto& _: pt_vars) tmp_names.pop_back();
                }
                return;
            }
            case TermType::IF: {
                auto* ti = dynamic_cast<TmIf*>(term.get());
                _collectMovableTerms(ti->c, tmp_names, leaves);
                return;
            }
        }
    }

    int tmp_id = 0;

    std::string _getTmpName() {
        return "tmp" + std::to_string(++tmp_id);
    }

    Term _rewriteTerm(const Term& term, const std::unordered_map<TermData*, std::string>& rewrite_map);

#define RewriteHead(name) Term _rewriteTerm(Tm ## name* term, const Term& _term, const std::unordered_map<TermData*, std::string>& rewrite_map)
#define RewriteCase(name) return _rewriteTerm(dynamic_cast<Tm ## name*> (term.get()), term, rewrite_map)

    RewriteHead(Align) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmAlign>(content);
    }
    RewriteHead(Label) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmLabel>(content);
    }
    RewriteHead(UnLabel) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmUnLabel>(content);
    }
    RewriteHead(App) {
        auto func = _rewriteTerm(term->func, rewrite_map);
        auto param = _rewriteTerm(term->param, rewrite_map);
        return std::make_shared<TmApp>(func, param);
    }
    RewriteHead(Match) {
        auto def = _rewriteTerm(term->def, rewrite_map);
        std::vector<std::pair<Pattern, Term>> cases;
        for (auto& [pattern, sub_term]: term->cases) {
            cases.emplace_back(pattern, _rewriteTerm(sub_term, rewrite_map));
        }
        return std::make_shared<TmMatch>(def, cases);
    }
    RewriteHead(Let) {
        auto def = _rewriteTerm(term->def, rewrite_map);
        auto content = _rewriteTerm(term->content, rewrite_map);
        auto* tv = dynamic_cast<TmVar*>(def.get());
        if (tv) {
            return incre::subst(content, term->name, def);
        }
        return std::make_shared<TmLet>(term->name, def, content);
    }
    RewriteHead(Proj) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmProj>(content, term->id);
    }
    RewriteHead(Tuple) {
        TermList fields;
        for (auto& field: term->fields) fields.push_back(_rewriteTerm(field, rewrite_map));
        return std::make_shared<TmTuple>(fields);
    }
    RewriteHead(If) {
        auto c = _rewriteTerm(term->c, rewrite_map);
        auto t = _rewriteTerm(term->t, rewrite_map);
        auto f = _rewriteTerm(term->f, rewrite_map);
        return std::make_shared<TmIf>(c, t, f);
    }
    RewriteHead(Fix) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmFix>(content);
    }
    RewriteHead(Abs) {
        auto content = _rewriteTerm(term->content, rewrite_map);
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }

    Term _rewriteTerm(const Term& term, const std::unordered_map<TermData*, std::string>& rewrite_map) {
        auto it = rewrite_map.find(term.get());
        if (it != rewrite_map.end()) {
            return std::make_shared<TmVar>(it->second);
        }
        switch (term->getType()) {
            case TermType::VALUE:
            case TermType::VAR:
                return term;
            case TermType::WILDCARD:
                LOG(FATAL) << "Unexpected TermType: " << term->toString();
            case TermType::ABS: RewriteCase(Abs);
            case TermType::FIX: RewriteCase(Fix);
            case TermType::ALIGN: RewriteCase(Align);
            case TermType::LABEL: RewriteCase(Label);
            case TermType::UNLABEL: RewriteCase(UnLabel);
            case TermType::MATCH: RewriteCase(Match);
            case TermType::LET: RewriteCase(Let);
            case TermType::PROJ: RewriteCase(Proj);
            case TermType::TUPLE: RewriteCase(Tuple);
            case TermType::APP: RewriteCase(App);
            case TermType::IF: RewriteCase(If);
        }
    }

    Term _buildNewTerm(const Term& term) {
        auto* ta = dynamic_cast<TmAlign*>(term.get()); assert(ta);
        TermList movable_term; std::vector<std::string> tmp_names;
        _collectMovableTerms(ta->content, tmp_names, movable_term);
        assert(tmp_names.empty());
        if (movable_term.empty()) return term;
        std::unordered_map<TermData*, std::string> rewrite_map;
        for (int i = 0; i < movable_term.size(); ++i) {
            auto tmp_name = _getTmpName();
            tmp_names.push_back(tmp_name);
            rewrite_map[movable_term[i].get()] = tmp_name;
        }

        auto content = _rewriteTerm(term, rewrite_map);
        for (int i = int(movable_term.size()) - 1; i >= 0; --i) {
            content = std::make_shared<TmLet>(tmp_names[i], movable_term[i], content);
        }
        return content;
    }

    Term _eliminateNestedAlign(const Term& term);

#define EliminateHead(name) Term _eliminateNestedAlign(Tm ## name* term, const Term& _term)
#define EliminateCase(name) return _eliminateNestedAlign(dynamic_cast<Tm ## name*>(term.get()), term)

    EliminateHead(Align) {
        auto content = _eliminateNestedAlign(term->content);
        return _buildNewTerm(std::make_shared<TmAlign>(content));
    }
    EliminateHead(Label) {
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmLabel>(content);
    }
    EliminateHead(UnLabel) {
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmUnLabel>(content);
    }
    EliminateHead(If) {
        auto c = _eliminateNestedAlign(term->c);
        auto t = _eliminateNestedAlign(term->t);
        auto f = _eliminateNestedAlign(term->f);
        return std::make_shared<TmIf>(c, t, f);
    }
    EliminateHead(Proj) {
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmProj>(content, term->id);
    }
    EliminateHead(Tuple) {
        TermList fields;
        for (auto& field: term->fields) fields.push_back(_eliminateNestedAlign(field));
        return std::make_shared<TmTuple>(fields);
    }
    EliminateHead(Match) {
        auto def = _eliminateNestedAlign(term->def);
        std::vector<std::pair<Pattern, Term>> cases;
        for (auto& [name, sub_term]: term->cases) {
            cases.emplace_back(name, _eliminateNestedAlign(sub_term));
        }
        return std::make_shared<TmMatch>(def, cases);
    }
    EliminateHead(App) {
        auto func = _eliminateNestedAlign(term->func);
        auto param = _eliminateNestedAlign(term->param);
        return std::make_shared<TmApp>(func, param);
    }
    EliminateHead(Let) {
        auto def = _eliminateNestedAlign(term->def);
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmLet>(term->name, def, content);
    }
    EliminateHead(Fix) {
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmFix>(content);
    }
    EliminateHead(Abs) {
        auto content = _eliminateNestedAlign(term->content);
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }

    Term _eliminateNestedAlign(const Term& term) {
        switch (term->getType()) {
            case TermType::VALUE:
            case TermType::VAR:
                return term;
            case TermType::ALIGN: EliminateCase(Align);
            case TermType::LABEL: EliminateCase(Label);
            case TermType::UNLABEL: EliminateCase(UnLabel);
            case TermType::IF: EliminateCase(If);
            case TermType::PROJ: EliminateCase(Proj);
            case TermType::TUPLE: EliminateCase(Tuple);
            case TermType::MATCH: EliminateCase(Match);
            case TermType::APP: EliminateCase(App);
            case TermType::WILDCARD:
                LOG(FATAL) << "Unexpected WILDCARD: " << term->toString();
            case TermType::FIX: EliminateCase(Fix);
            case TermType::ABS: EliminateCase(Abs);
            case TermType::LET: EliminateCase(Let);
        }
    }
}

IncreProgram incre::eliminateNestedAlign(ProgramData *program) {
    CommandList commands;
    for (auto& command: program->commands) {
        if (command->getType() != CommandType::BIND) {
            commands.push_back(command); continue;
        }
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        if (cb->binding->getType() != BindingType::TERM) {
            commands.push_back(command); continue;
        }
        auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
        auto new_term = _eliminateNestedAlign(tb->term);
        auto new_bind = std::make_shared<TermBinding>(new_term);
        commands.push_back(std::make_shared<CommandBind>(cb->name, new_bind, cb->decorate_set));
    }
    return std::make_shared<ProgramData>(commands, program->config_map);
}
