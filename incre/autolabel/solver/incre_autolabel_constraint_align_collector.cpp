//
// Created by pro on 2023/1/27.
//

#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::autolabel;

namespace {

    bool is_print = false;

    void _collectCons(const Term& term, Z3Context* ctx, const z3::expr& path) {
        TermList sub_list = incre::getSubTerms(term.get());

        auto align_it = ctx->align_map.find(term.get());
        auto flip_it = ctx->flip_map.find(term.get());

        auto new_path = (align_it == ctx->align_map.end()) ? path : (path | align_it->second);
        if (flip_it != ctx->flip_map.end()) {
            ctx->addCons(z3::implies(flip_it->second, new_path));
        }

        for (auto& sub: sub_list) {
            _collectCons(sub, ctx, new_path);
        }
    }
}

void autolabel::collectAlignConstraint(ProgramData *program, Z3Context *ctx) {
    for (auto& command: program->commands) {
        if (command->getType() != CommandType::BIND) continue;
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        if (cb->binding->getType() != BindingType::TERM) continue;
        auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
        _collectCons(tb->term, ctx, ctx->ctx.bool_val(false));
    }
}

namespace {
    z3::expr _initFreeMap(const Term& term, Z3Context* ctx) {
        auto free_var = ctx->getVar();
        ctx->free_map.insert({term.get(), free_var});

        auto sub_list = incre::getSubTerms(term.get());
        z3::expr_vector sub_cons(ctx->ctx);
        for (auto& sub_term: sub_list) {
            sub_cons.push_back(_initFreeMap(sub_term, ctx));
        }
        ctx->addCons(free_var == z3::mk_and(sub_cons));

        auto res = free_var;
        auto flip_it = ctx->flip_map.find(term.get());
        auto align_it = ctx->align_map.find(term.get());
        if (flip_it != ctx->flip_map.end()) res = res & (!flip_it->second);
        if (align_it != ctx->align_map.end()) res = res | (align_it->second);
        return res;
    }

    void _collectObjective(const Term& term, Z3Context* ctx, z3::expr_vector& objs, const z3::expr& unmovable_path,
                           const z3::expr& pre_movable_path) {
        auto movable_path = pre_movable_path;
        auto align_it = ctx->align_map.find(term.get());
        auto free_it = ctx->free_map.find(term.get());
        if (align_it != ctx->align_map.end()) movable_path = pre_movable_path | align_it->second;

        auto obj_var = ctx->getVar();
        ctx->obj_map.insert({term.get(), obj_var});
        ctx->addCons(obj_var == (unmovable_path | (movable_path & !free_it->second)));
        objs.push_back(obj_var);

        switch (term->getType()) {
            case TermType::ALIGN:
            case TermType::LABEL:
            case TermType::UNLABEL:
            case TermType::WILDCARD:
                LOG(FATAL) << "Unexpected TermType: " << term->toString();
            case TermType::TUPLE:
            case TermType::PROJ:
            case TermType::VALUE:
            case TermType::VAR:
            case TermType::APP: {
                for (auto& sub_term: incre::getSubTerms(term.get())) {
                    _collectObjective(sub_term, ctx, objs, unmovable_path, movable_path);
                }
                return;
            }
            case TermType::ABS: {
                auto* ta = dynamic_cast<TmAbs*>(term.get());
                _collectObjective(ta->content, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                return;
            }
            case TermType::FIX: {
                auto* tf = dynamic_cast<TmFix*>(term.get());
                _collectObjective(tf->content, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                return;
            }
            case TermType::LET: {
                auto* tl = dynamic_cast<TmLet*>(term.get());
                _collectObjective(tl->def, ctx, objs, unmovable_path, movable_path);
                _collectObjective(tl->content, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                return;
            }
            case TermType::MATCH: {
                auto* tm = dynamic_cast<TmMatch*>(term.get());
                _collectObjective(tm->def, ctx, objs, unmovable_path, movable_path);
                for (auto& [name, sub_term]: tm->cases) {
                    _collectObjective(sub_term, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                }
                return;
            }
            case TermType::IF: {
                auto* ti = dynamic_cast<TmIf*>(term.get());
                _collectObjective(ti->c, ctx, objs, unmovable_path, movable_path);
                _collectObjective(ti->f, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                _collectObjective(ti->t, ctx, objs, unmovable_path | movable_path, ctx->ctx.bool_val(false));
                return;
            }
        }
    }
}

z3::expr autolabel::collectMinimalAlignConstraint(ProgramData *program, Z3Context *ctx) {
    z3::expr_vector objs(ctx->ctx);
    for (auto& command: program->commands) {
        if (command->getType() != CommandType::BIND) continue;
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        if (cb->binding->getType() != BindingType::TERM) continue;
        auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
        _initFreeMap(tb->term, ctx);
        _collectObjective(tb->term, ctx, objs, ctx->ctx.bool_val(false), ctx->ctx.bool_val(false));
    }
    // for (auto [term, align_var]: ctx->align_map) objs.push_back(align_var);

    z3::expr obj = ctx->ctx.int_val(0);
    for (int i = 0; i < objs.size(); ++i) {
        obj = obj + z3::ite(objs[i], ctx->ctx.int_val(1), ctx->ctx.int_val(0));
    }
    return obj;
}