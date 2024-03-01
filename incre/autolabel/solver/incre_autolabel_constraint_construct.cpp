//
// Created by pro on 2023/1/28.
//

#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::autolabel;

namespace {
    bool _getBool(const z3::expr& expr, const z3::model& model) {
        auto res = model.eval(expr);
        if (res.bool_value() == Z3_L_TRUE) return true;
        return false;
    }

    std::pair<z3::expr, Ty> _unfoldCompress(const Ty& type, z3::context& ctx) {
        if (type->getType() != TyType::COMPRESS) {
            return {ctx.bool_val(false), type};
        }
        auto* tlc = dynamic_cast<TyZ3LabeledCompress*>(type.get());
        if (tlc) return {tlc->label, tlc->content};
        auto* tc = dynamic_cast<TyCompress*>(type.get());
        return {ctx.bool_val(true), tc->content};
    }

    Term _constructLabel(const Term& term, const z3::model& model, Z3Context* ctx);

#define ConstructHead(name) Term _constructLabel(Tm ## name* term, const z3::model& model, Z3Context* ctx)
#define ConstructCase(name) {res = _constructLabel(dynamic_cast<Tm ## name*>(term.get()), model, ctx); break;}

    ConstructHead(Match) {
        auto def = _constructLabel(term->def, model, ctx);
        std::vector<std::pair<Pattern, Term>> cases;
        for (auto& [cname, sub_term]: term->cases) {
            cases.emplace_back(cname, _constructLabel(sub_term, model, ctx));
        }
        return std::make_shared<TmMatch>(def, cases);
    }
    ConstructHead(Let) {
        auto def = _constructLabel(term->def, model, ctx);
        auto content = _constructLabel(term->content, model, ctx);
        return std::make_shared<TmLet>(term->name, def, content);
    }
    ConstructHead(App) {
        auto func = _constructLabel(term->func, model, ctx);
        auto param = _constructLabel(term->param, model, ctx);
        return std::make_shared<TmApp>(func, param);
    }
    ConstructHead(Fix) {
        auto content = _constructLabel(term->content, model, ctx);
        return std::make_shared<TmFix>(content);
    }
    ConstructHead(Abs) {
        auto content = _constructLabel(term->content, model, ctx);
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }
    ConstructHead(If) {
        auto c = _constructLabel(term->c, model, ctx);
        auto t = _constructLabel(term->t, model, ctx);
        auto f = _constructLabel(term->f, model, ctx);
        return std::make_shared<TmIf>(c, t, f);
    }
    ConstructHead(Proj) {
        auto content = _constructLabel(term->content, model, ctx);
        return std::make_shared<TmProj>(content, term->id);
    }
    ConstructHead(Tuple) {
        TermList fields;
        for (auto& field: term->fields) {
            fields.push_back(_constructLabel(field, model, ctx));
        }
        return std::make_shared<TmTuple>(fields);
    }

    Term _constructLabel(const Term& term, const z3::model& model, Z3Context* ctx) {
        Term res;
        switch (term->getType()) {
            case TermType::VALUE:
            case TermType::VAR: {
                res = term; break;
            }
            case TermType::ALIGN:
            case TermType::LABEL:
            case TermType::UNLABEL:
            case TermType::WILDCARD:
                LOG(FATAL) << "Unexpected TermType: " << term->toString();
            case TermType::FIX: ConstructCase(Fix);
            case TermType::ABS: ConstructCase(Abs);
            case TermType::MATCH: ConstructCase(Match);
            case TermType::APP: ConstructCase(App);
            case TermType::IF: ConstructCase(If);
            case TermType::PROJ: ConstructCase(Proj);
            case TermType::TUPLE: ConstructCase(Tuple);
            case TermType::LET: ConstructCase(Let);
        }
        auto flip_it = ctx->flip_map.find(term.get());
        auto type_it = ctx->type_map.find(term.get());
        auto align_it = ctx->align_map.find(term.get());

        if (flip_it != ctx->flip_map.end() && _getBool(flip_it->second, model)) {
            auto [label, _] = _unfoldCompress(type_it->second, ctx->ctx);
            if (_getBool(label, model)) {
                res = std::make_shared<TmLabel>(res);
            } else res = std::make_shared<TmUnLabel>(res);
        }

        if (align_it != ctx->align_map.end() && _getBool(align_it->second, model)) {
            // LOG(INFO) << "Align " << res->toString();
            res = std::make_shared<TmAlign>(res);
        }
        return res;
    }
}

IncreProgram autolabel::constructLabel(ProgramData *program, const z3::model &model, Z3Context *ctx) {
    CommandList commands;
    for (auto& command: program->commands) {
        if (command->getType() != CommandType::BIND) {
            commands.push_back(command); continue;
        }
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        auto& bind = cb->binding;
        if (bind->getType() != BindingType::TERM) {
            commands.push_back(command); continue;
        }
        auto* bt = dynamic_cast<TermBinding*>(bind.get());
        auto new_term = _constructLabel(bt->term, model, ctx);
        auto new_bind = std::make_shared<TermBinding>(new_term);
        commands.push_back(std::make_shared<CommandBind>(cb->name, new_bind, cb->decorate_set));
    }
    return std::make_shared<ProgramData>(commands, program->config_map);
}