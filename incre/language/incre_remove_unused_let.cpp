//
// Created by pro on 2023/3/5.
//

#include "istool/incre/language/incre.h"
#include "glog/logging.h"

using namespace incre;

namespace {
    Term _eliminateUnusedLet(const Term& term);

#define EliminateHead(name) Term _eliminateUnusedLet(Tm ## name* term)
#define EliminateCase(name) return _eliminateUnusedLet(dynamic_cast<Tm ## name*>(term.get()))

    EliminateHead(Let) {
        auto content = _eliminateUnusedLet(term->content);
        auto unbounded_vars = incre::getUnboundedVars(content.get());
        bool is_used = false;
        for (auto& var: unbounded_vars) if (var == term->name) is_used = true;
        if (is_used) {
            auto def = _eliminateUnusedLet(term->def);
            return std::make_shared<TmLet>(term->name, def, content);
        }
        return content;
    }
    EliminateHead(Proj) {
        return std::make_shared<TmProj>(_eliminateUnusedLet(term->content), term->id);
    }
    EliminateHead(Abs) {
        return std::make_shared<TmAbs>(term->name, term->type, _eliminateUnusedLet(term->content));
    }
    EliminateHead(Fix) {
        return std::make_shared<TmFix>(_eliminateUnusedLet(term->content));
    }
    EliminateHead(Tuple) {
        TermList fields;
        for (auto& field: term->fields) fields.push_back(_eliminateUnusedLet(field));
        return std::make_shared<TmTuple>(fields);
    }
    EliminateHead(If) {
        return std::make_shared<TmIf> (_eliminateUnusedLet(term->c), _eliminateUnusedLet(term->t),
                                       _eliminateUnusedLet(term->f));
    }
    EliminateHead(App) {
        return std::make_shared<TmApp>(_eliminateUnusedLet(term->func), _eliminateUnusedLet(term->param));
    }
    EliminateHead(Match) {
        std::vector<std::pair<Pattern, Term>> cases;
        for (auto& [pt, case_term]: term->cases) {
            cases.emplace_back(pt, _eliminateUnusedLet(case_term));
        }
        auto def = _eliminateUnusedLet(term->def);
        return std::make_shared<TmMatch>(def, cases);
    }
    EliminateHead(Align) {
        return std::make_shared<TmAlign>(_eliminateUnusedLet(term->content));
    }

    Term _eliminateUnusedLet(const Term& term) {
        switch (term->getType()) {
            case TermType::WILDCARD:
            case TermType::LABEL:
            case TermType::UNLABEL:
                LOG(FATAL) << "Unexpected TermType " << term->toString();
            case TermType::VALUE:
            case TermType::VAR: return term;
            case TermType::ALIGN: EliminateCase(Align);
            case TermType::LET: EliminateCase(Let);
            case TermType::PROJ: EliminateCase(Proj);
            case TermType::ABS: EliminateCase(Abs);
            case TermType::FIX: EliminateCase(Fix);
            case TermType::TUPLE: EliminateCase(Tuple);
            case TermType::IF: EliminateCase(If);
            case TermType::APP: EliminateCase(App);
            case TermType::MATCH: EliminateCase(Match);
        }
    }
}

IncreProgram incre::eliminateUnusedLet(ProgramData *program) {
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
        auto new_term = _eliminateUnusedLet(tb->term);
        auto new_bind = std::make_shared<TermBinding>(new_term);
        commands.push_back(std::make_shared<CommandBind>(cb->name, new_bind, cb->decorate_set));
    }
    return std::make_shared<ProgramData>(commands, program->config_map);
}