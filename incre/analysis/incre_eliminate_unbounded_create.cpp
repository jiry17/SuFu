//
// Created by pro on 2022/9/21.
//

#include "istool/incre/analysis/incre_instru_info.h"
#include "glog/logging.h"
#include <unordered_set>

using namespace incre;
/*
namespace {
#define EliminateCreateCase(name) return _eliminateUnboundedCreate(dynamic_cast<Tm ## name*>(term.get()), term, is_bounded)
#define EliminateCreateHead(name) Term _eliminateUnboundedCreate(Tm ## name* term, const Term& _term, bool is_bounded)
#define EliminateSub(name) auto name = _eliminateUnboundedCreate(term->name, is_bounded)

    Term _eliminateUnboundedCreate(const Term&, bool);

    EliminateCreateHead(Var) {return _term;}
    EliminateCreateHead(Create) {
        if (is_bounded) return _term;
        return std::make_shared<TmPass>(std::vector<std::string>(), TermList(), _term);
    }
    EliminateCreateHead(Tuple) {
        TermList fields;
        for (const auto& field: term->fields) {
            fields.push_back(_eliminateUnboundedCreate(field, is_bounded));
        }
        return std::make_shared<TmTuple>(fields);
    }
    EliminateCreateHead(App) {
        EliminateSub(func); EliminateSub(param);
        return std::make_shared<TmApp>(func, param);
    }
    EliminateCreateHead(Abs) {
        EliminateSub(content);
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }
    EliminateCreateHead(Value) {
        return _term;
    }
    EliminateCreateHead(If) {
        EliminateSub(c); EliminateSub(t); EliminateSub(f);
        return std::make_shared<TmIf>(c, t, f);
    }
    EliminateCreateHead(Let) {
        EliminateSub(def); EliminateSub(content);
        return std::make_shared<TmLet>(term->name, def, content);
    }
    EliminateCreateHead(Pass) {
        if (is_bounded) LOG(FATAL) << "Nested usage of pass is invalid";
        TermList defs;
        for (const auto& def: term->defs) defs.push_back(_eliminateUnboundedCreate(def, is_bounded));
        auto content = _eliminateUnboundedCreate(term->content, true);
        return std::make_shared<TmPass>(term->names, defs, content);
    }
    EliminateCreateHead(Match) {
        EliminateSub(def);
        std::vector<std::pair<Pattern, Term>> cases;
        for (const auto& [pt, term]: term->cases) {
            cases.emplace_back(pt, _eliminateUnboundedCreate(term, is_bounded));
        }
        return std::make_shared<TmMatch>(def,cases);
    }
    EliminateCreateHead(Fix) {
        EliminateSub(content);
        return std::make_shared<TmFix>(content);
    }
    EliminateCreateHead(Proj) {
        EliminateSub(content);
        return std::make_shared<TmProj>(content, term->id);
    }

    Term _eliminateUnboundedCreate(const Term& term, bool is_bounded) {
        switch (term->getType()) {
            case TermType::VAR: EliminateCreateCase(Var);
            case TermType::CREATE: EliminateCreateCase(Create);
            case TermType::TUPLE: EliminateCreateCase(Tuple);
            case TermType::VALUE: EliminateCreateCase(Value);
            case TermType::ABS: EliminateCreateCase(Abs);
            case TermType::APP: EliminateCreateCase(App);
            case TermType::IF: EliminateCreateCase(If);
            case TermType::LET: EliminateCreateCase(Let);
            case TermType::PASS: EliminateCreateCase(Pass);
            case TermType::MATCH: EliminateCreateCase(Match);
            case TermType::PROJ: EliminateCreateCase(Proj);
            case TermType::FIX: EliminateCreateCase(Fix);
        }
    }
}

IncreProgram incre::eliminateUnboundedCreate(const IncreProgram& prog) {
    CommandList commands;
    for (const auto& command: prog->commands) {
        switch (command->getType()) {
            case CommandType::IMPORT:
            case CommandType::DEF_IND: {
                commands.push_back(command); break;
            }
            case CommandType::BIND: {
                auto* bind = dynamic_cast<CommandBind*>(command.get());
                assert(bind);
                if (bind->binding->getType() == BindingType::TERM) {
                    auto* term_bind = dynamic_cast<TermBinding*>(bind->binding.get());
                    assert(term_bind);
                    auto term = _eliminateUnboundedCreate(term_bind->term, false);
                    auto new_bind = std::make_shared<TermBinding>(term);
                    commands.push_back(std::make_shared<CommandBind>(bind->name, new_bind));
                } else {
                    commands.push_back(command);
                }
                break;
            }
        }
    }
    return std::make_shared<ProgramData>(commands);
}*/

namespace {

    void _checkCovered(const Term& term) {
        switch (term->getType()) {
            case TermType::ALIGN:
                return;
            case TermType::LABEL:
            case TermType::UNLABEL: {
                LOG(FATAL) << "Term " << term->toString() << " is not covered by TmAlign";
            }
            default: {
                auto sub_terms = incre::getSubTerms(term.get());
                for (auto& sub_term: sub_terms) _checkCovered(sub_term);
                return;
            }
        }
    }
}

// TODO: This check is a temporary syntax check.
void incre::checkAllLabelBounded(ProgramData *program) {
    for (auto& command: program->commands) {
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        if (!cb) continue;
        auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
        if (!tb) continue;
        _checkCovered(tb->term);
    }
}