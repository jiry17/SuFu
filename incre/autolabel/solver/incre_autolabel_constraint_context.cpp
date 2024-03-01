//
// Created by pro on 2023/1/27.
//
#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "istool/incre/language/incre_lookup.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::autolabel;

std::string TyZ3LabeledCompress::toString() const {
    return "Compress[" + label.to_string() + "] " + content->toString();
}
TyZ3LabeledCompress::TyZ3LabeledCompress(const Ty &content, const z3::expr &_label):
    TyCompress(content), label(_label) {
}

void autolabel::Z3Context::addCons(const z3::expr &cons) {
    cons_list.push_back(cons);
}

z3::expr Z3Context::getVar() {
    auto name = "x" + std::to_string(++tmp_id);
    return ctx.bool_const(name.c_str());
}
autolabel::Z3Context::Z3Context(): ctx(), cons_list(ctx), tmp_id(0) {
}

Ty autolabel::unfoldTypeWithZ3Label(const Ty &type, TypeContext *ctx) {
    ExternalUnfoldRule rule;
    rule.func = [](const Ty& type, TypeContext* ctx, std::vector<std::string>& tmp_names, const ExternalUnfoldMap& map) -> Ty {
        auto* tc = dynamic_cast<TyCompress*>(type.get()); assert(tc);
        auto content = incre::unfoldTypeAll(tc->content, ctx, tmp_names, map);
        auto* tlc = dynamic_cast<TyZ3LabeledCompress*>(type.get());
        if (tlc) return std::make_shared<TyZ3LabeledCompress>(content, tlc->label);
        return std::make_shared<TyCompress>(content);
    };
    ExternalUnfoldMap map = {{TyType::COMPRESS, rule}};
    std::vector<std::string> tmp_names;
    return incre::unfoldTypeAll(type, ctx, tmp_names, map);
}

namespace {
    bool is_print = false;

    std::pair<z3::expr, Ty> _unfoldCompress(const Ty& type, z3::context& ctx) {
        if (type->getType() != TyType::COMPRESS) {
            return {ctx.bool_val(false), type};
        }
        auto* tlc = dynamic_cast<TyZ3LabeledCompress*>(type.get());
        if (tlc) return {tlc->label, tlc->content};
        auto* tc = dynamic_cast<TyCompress*>(type.get());
        return {ctx.bool_val(true), tc->content};
    }

    void __align(const Ty& x, const Ty& y, Z3Context* ctx) {
        auto [x_label, x_content] = _unfoldCompress(x, ctx->ctx);
        auto [y_label, y_content] = _unfoldCompress(y, ctx->ctx);
        if (x->getType() == TyType::COMPRESS || y->getType() == TyType::COMPRESS) {
            ctx->addCons(x_label == y_label);
        }
        // LOG(INFO) << "Try align " << x->toString() << " " << y->toString();
        assert(x_content->getType() == y_content->getType());
        switch (x_content->getType()) {
            case TyType::COMPRESS: assert(0);
            case TyType::INT:
            case TyType::BOOL:
            case TyType::VAR:
            case TyType::UNIT: return;
            case TyType::ARROW: {
                auto* xt = dynamic_cast<TyArrow*>(x_content.get());
                auto* yt = dynamic_cast<TyArrow*>(y_content.get());
                __align(xt->source, yt->source, ctx);
                __align(xt->target, yt->target, ctx);
                return;
            }
            case TyType::TUPLE: {
                auto* xt = dynamic_cast<TyTuple*>(x_content.get());
                auto* yt = dynamic_cast<TyTuple*>(y_content.get());
                assert(xt->fields.size() == yt->fields.size());
                for (int i = 0; i < xt->fields.size(); ++i) {
                    __align(xt->fields[i], yt->fields[i], ctx);
                }
                return;
            }
            case TyType::IND: {
                std::unordered_map<std::string, Ty> sub_map;
                auto* xt = dynamic_cast<TyInductive*>(x_content.get());
                auto* yt = dynamic_cast<TyInductive*>(y_content.get());
                assert(xt->constructors.size() == yt->constructors.size());
                for (auto& [cons_name, cons_type]: xt->constructors) {
                    sub_map[cons_name] = cons_type;
                }
                for (auto& [cons_name, cons_type]: yt->constructors) {
                    assert(sub_map.count(cons_name));
                    __align(sub_map[cons_name], cons_type, ctx);
                }
                return;
            }
        }
    }

    void _align(const Ty& x, const Ty& y, Z3Context* ctx) {
        auto full_x = unfoldTypeWithZ3Label(x, ctx);
        auto full_y = unfoldTypeWithZ3Label(y, ctx);
        __align(full_x, full_y, ctx);
    }

    Ty _labelTerm(const Term& term, Z3Context* ctx, bool is_scalar_only);

#define LabelHead(name) Ty _labelTerm(Tm ## name* term, Z3Context* ctx, bool is_scalar_only)
#define LabelCase(name) {raw_type = _labelTerm(dynamic_cast<Tm ## name*>(term.get()), ctx, is_scalar_only); break;}

    LabelHead(If) {
        auto c = _labelTerm(term->c, ctx, is_scalar_only);
        auto t = _labelTerm(term->t, ctx, is_scalar_only);
        auto f = _labelTerm(term->f, ctx, is_scalar_only);
        LOG(INFO) << "Align term " << term->toString();
        _align(c, std::make_shared<TyBool>(), ctx);
        _align(t, f, ctx);
        return t;
    }
    LabelHead(Var) {
        return ctx->lookup(term->name);
    }
    LabelHead(Let) {
        auto type = _labelTerm(term->def, ctx, is_scalar_only);
        auto log = ctx->bind(term->name, type);
        auto res = _labelTerm(term->content, ctx, is_scalar_only);
        ctx->cancelBind(log);
        return res;
    }
    LabelHead(Proj) {
        auto res = autolabel::unfoldTypeWithZ3Label(_labelTerm(term->content, ctx, is_scalar_only), ctx);
        auto [label, content] = _unfoldCompress(res, ctx->ctx);
        ctx->addCons(!label);
        auto* tt = dynamic_cast<TyTuple*>(content.get());
        assert(tt && tt->fields.size() >= term->id);
        return tt->fields[term->id - 1];
    }
    LabelHead(Tuple) {
        TyList sub_list;
        for (auto& field: term->fields) sub_list.push_back(_labelTerm(field, ctx, is_scalar_only));
        return std::make_shared<TyTuple>(sub_list);
    }
    LabelHead(Fix) {
        auto res = _labelTerm(term->content, ctx, is_scalar_only);
        auto* at = dynamic_cast<TyArrow*>(res.get());
        assert(at);
        _align(at->source, at->target, ctx);
        return at->source;
    }
    LabelHead(Abs) {
        auto log = ctx->bind(term->name, term->type);
        auto res = _labelTerm(term->content, ctx, is_scalar_only);
        ctx->cancelBind(log);
        return std::make_shared<TyArrow>(term->type, res);
    }
    LabelHead(Value) {
        return incre::getValueType(term->data.get());
    }
    LabelHead(App) {
        auto func = _labelTerm(term->func, ctx, is_scalar_only);
        func = autolabel::unfoldTypeWithZ3Label(func, ctx);
        auto param = _labelTerm(term->param, ctx, is_scalar_only);
        auto* at = dynamic_cast<TyArrow*>(func.get());
        // LOG(INFO) << "get type " << term->toString() << " " << at->toString() << " " << param->toString();
        assert(at); _align(at->source, param, ctx);
        return at->target;
    }
    void _bind(const Pattern& pattern, const Ty& init_type, Z3Context* ctx, std::vector<TypeContext::BindLog>& log_list) {
        auto type = autolabel::unfoldTypeWithZ3Label(init_type, ctx);
        // LOG(INFO) << "bind " << pattern->toString() << " " << type->toString();
        switch (pattern->getType()) {
            case PatternType::VAR: {
                auto* pv = dynamic_cast<PtVar*>(pattern.get());
                log_list.push_back(ctx->bind(pv->name, type));
                return;
            }
            case PatternType::UNDER_SCORE: return;
            case PatternType::TUPLE: {
                auto* pt = dynamic_cast<PtTuple*>(pattern.get());
                auto [label, content] = _unfoldCompress(type, ctx->ctx);
                ctx->addCons(!label);
                auto* tt = dynamic_cast<TyTuple*>(content.get());
                assert(tt && tt->fields.size() == pt->pattern_list.size());
                for (int i = 0; i < tt->fields.size(); ++i) {
                    _bind(pt->pattern_list[i], tt->fields[i], ctx, log_list);
                }
                return;
            }
            case PatternType::CONSTRUCTOR: {
                auto* pc = dynamic_cast<PtConstructor*>(pattern.get());
                auto [label, content] = _unfoldCompress(type, ctx->ctx);
                ctx->addCons(!label);
                auto* ti = dynamic_cast<TyInductive*>(content.get());
                for (auto& [cname, ctype]: ti->constructors) {
                    if (cname == pc->name) {
                        _bind(pc->pattern, ctype, ctx, log_list);
                        return;
                    }
                }
                assert(0);
            }
        }
    }
    LabelHead(Match) {
        auto content = autolabel::unfoldTypeWithZ3Label(_labelTerm(term->def, ctx, is_scalar_only), ctx);
        TyList case_types;
        for (auto& [pattern, sub_term]: term->cases) {
            std::vector<TypeContext::BindLog> log_list;
            _bind(pattern, content, ctx, log_list);
            case_types.push_back(_labelTerm(sub_term, ctx, is_scalar_only));
            for (int i = int(log_list.size()) - 1; i >= 0; --i) {
                ctx->cancelBind(log_list[i]);
            }
        }
        LOG(INFO) << "Align term " << term->toString() << " " << content->toString();
        for (int i = 1; i < case_types.size(); ++i) {
            _align(case_types[0], case_types[i], ctx);
        }
        return case_types[0];
    }

    bool _isFirstOrder(const Ty& type) {
        match::MatchTask task;
        task.type_matcher = [](TyData* type, const match::MatchContext& ctx) -> bool {
            return type->getType() == TyType::ARROW;
        };
        return !match::match(type.get(), task);
    }

    bool _isCanFlip(const Ty& type, TypeContext* ctx) {
        auto full_type = autolabel::unfoldTypeWithZ3Label(type, ctx);
        return _isFirstOrder(full_type) && (full_type->getType() == TyType::TUPLE || full_type->getType() == TyType::IND || full_type->getType() == TyType::COMPRESS);
    }
    bool _isCanAlign(const Ty& type, TypeContext* ctx) {
        auto full_type = autolabel::unfoldTypeWithZ3Label(type, ctx);
        return _isFirstOrder(full_type);
    }

    std::pair<bool, z3::expr> __getScalarOnlyAlignCons(const Ty& type, z3::context& ctx) {
        switch (type->getType()) {
            case TyType::VAR:
            case TyType::ARROW:
                LOG(FATAL) << "Unexpected TyType: " << type->toString();
            case TyType::INT:
            case TyType::BOOL:
            case TyType::UNIT:
                return {false, ctx.bool_val(true)};
            case TyType::IND:
                return {true, ctx.bool_val(false)};
            case TyType::COMPRESS: {
                auto [label, content] = _unfoldCompress(type, ctx);
                auto [is_inductive, sub_cons] = __getScalarOnlyAlignCons(content, ctx);
                if (is_inductive) return {true, label | sub_cons};
                return {false, ctx.bool_val(true)};
            }
            case TyType::TUPLE: {
                auto* tt = dynamic_cast<TyTuple*>(type.get());
                bool is_inductive = false; z3::expr cons = ctx.bool_val(true);
                for (auto& field: tt->fields) {
                    auto sub_res = __getScalarOnlyAlignCons(field, ctx);
                    is_inductive |= sub_res.first;
                    cons = cons & sub_res.second;
                }
                if (is_inductive) return {true, cons};
                return {false, ctx.bool_val(true)};
            }
        }
    }

    z3::expr _getScalarOnlyAlignCons(const Ty& type, Z3Context* ctx) {
        auto full_type = autolabel::unfoldTypeWithZ3Label(type, ctx);
        return __getScalarOnlyAlignCons(full_type, ctx->ctx).second;
    }

    Ty _labelTerm(const Term& term, Z3Context* ctx, bool is_scalar_only) {
        Ty raw_type;
        switch (term->getType()) {
            case TermType::ALIGN:
            case TermType::LABEL:
            case TermType::UNLABEL:
            case TermType::WILDCARD:
                LOG(FATAL) << "Unexpected TermType: " << term->toString();
            case TermType::IF: LabelCase(If);
            case TermType::VAR: LabelCase(Var);
            case TermType::LET: LabelCase(Let);
            case TermType::MATCH: LabelCase(Match);
            case TermType::FIX: LabelCase(Fix);
            case TermType::ABS: LabelCase(Abs);
            case TermType::APP: LabelCase(App);
            case TermType::VALUE: LabelCase(Value);
            case TermType::TUPLE: LabelCase(Tuple);
            case TermType::PROJ: LabelCase(Proj);
        }

        raw_type = unfoldTypeWithZ3Label(raw_type, ctx);
        Ty res;
        if (_isCanFlip(raw_type, ctx)) {
            auto flip_var = ctx->getVar();
            ctx->flip_map.insert({term.get(), flip_var});
            if (raw_type->getType() == TyType::COMPRESS) {
                auto [label, content] = _unfoldCompress(raw_type, ctx->ctx);
                res = std::make_shared<TyZ3LabeledCompress>(content, label ^ flip_var);
            } else res = std::make_shared<TyZ3LabeledCompress>(raw_type, flip_var);
        } else res = raw_type;

        // LOG(INFO) << "Full type " << term->toString() << " " << res->toString() << " " << _isCanFlip(raw_type, ctx);

        if (_isCanAlign(raw_type, ctx)) {
            // LOG(INFO) << "Can align " << term->toString();
            auto align_var = ctx->getVar();
            ctx->align_map.insert({term.get(), align_var});
            if (is_scalar_only) ctx->addCons(z3::implies(align_var, _getScalarOnlyAlignCons(res, ctx)));
            else {
                auto flip_it = ctx->flip_map.find(term.get());
                if (flip_it != ctx->flip_map.end()) {
                    auto flip_var = flip_it->second;
                    if (raw_type->getType() == TyType::COMPRESS) {
                        auto [label, content] = _unfoldCompress(raw_type, ctx->ctx);
                        ctx->addCons(z3::implies(align_var && flip_var, !label));
                    }
                }
            }
        }

        return ctx->type_map[term.get()] = res;
    }
}

void autolabel::initZ3Context(ProgramData* init_program, Z3Context* ctx, bool is_scalar_only) {
    for (auto& command: init_program->commands) {
        switch (command->getType()) {
            case CommandType::DEF_IND: {
                auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
                ctx->bind(cd->type->name, cd->_type);
                for (auto& [cname, ctype]: cd->type->constructors) {
                    auto inp_type = incre::subst(ctype, cd->type->name, cd->_type);
                    auto full_type = std::make_shared<TyArrow>(inp_type, cd->_type);
                    ctx->bind(cname, full_type);
                }
                break;
            }
            case CommandType::IMPORT: break;
            case CommandType::BIND: {
                auto* cb = dynamic_cast<CommandBind*>(command.get());
                auto& bind = cb->binding;
                switch (bind->getType()) {
                    case BindingType::VAR: {
                        auto* bt = dynamic_cast<VarTypeBinding*>(bind.get());
                        ctx->bind(cb->name, bt->type); break;
                    }
                    case BindingType::TYPE: {
                        auto* bt = dynamic_cast<TypeBinding*>(bind.get());
                        ctx->bind(cb->name, bt->type); break;
                    }
                    case BindingType::TERM: {
                        auto* bt = dynamic_cast<TermBinding*>(bind.get());

                        auto type = _labelTerm(bt->term, ctx, is_scalar_only);
                        ctx->bind(cb->name, type);
                        break;
                    }
                }
            }
        }
    }
}