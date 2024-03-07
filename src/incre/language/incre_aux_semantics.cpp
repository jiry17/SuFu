//
// Created by pro on 2022/9/17.
//

#include "istool/incre/language/incre.h"
#include "glog/logging.h"

using namespace incre;

bool incre::isUsed(const Pattern& pt, const std::string& name) {
    switch (pt->getType()) {
        case PatternType::UNDER_SCORE: return false;
        case PatternType::VAR: {
            auto *x = dynamic_cast<PtVar *>(pt.get());
            return x->name == name;
        }
        case PatternType::CONSTRUCTOR: {
            auto *x = dynamic_cast<PtConstructor *>(pt.get());
            return isUsed(x->pattern, name);
        }
        case PatternType::TUPLE: {
            auto* x = dynamic_cast<PtTuple*>(pt.get());
            for (const auto& sub_pattern: x->pattern_list) {
                if (isUsed(sub_pattern, name)) return true;
            }
            return false;
        }
    }
}

namespace {
    std::pair<Term, bool> _subst(const Term& x, const std::string& name, const Term& y);

#define SubstHead(ty) std::pair<Term, bool> _subst(Tm ## ty *x, const Term& _x, const std::string& name, const Term& y)
#define SubstCase(ty) return _subst(dynamic_cast<Tm ## ty *>(x.get()), x, name, y)
#define SubstRes(field) auto [res_ ## field, flag_ ## field] = _subst(x-> field, name, y)

    SubstHead(Var) {
        if (x->name == name) return {y, true};
        return {_x, false};
    }

    SubstHead(Match) {
        auto def = _subst(x->def, name, y);
        bool is_changed = def.second;
        std::vector<std::pair<Pattern, Term>> cases;
        for (const auto& [pt, branch]: x->cases) {
            if (isUsed(pt, name)) {
                cases.emplace_back(pt, branch);
            } else {
                auto [res, flag] = _subst(branch, name, y);
                is_changed |= flag;
                cases.emplace_back(pt, res);
            }
        }
        if (!is_changed) return {_x, false};
        return {std::make_shared<TmMatch>(def.first, cases), true};
    }

    SubstHead(Tuple) {
        bool is_changed = false;
        TermList fields;
        for (const auto& field: x->fields) {
            auto [res, flag] = _subst(field, name, y);
            is_changed |= flag;
            fields.emplace_back(res);
        }
        if (!is_changed) return {_x, false};
        return {std::make_shared<TmTuple>(fields), true};
    }

    SubstHead(Label) {
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmLabel>(res_content), true};
    }

    SubstHead(UnLabel) {
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmUnLabel>(res_content), true};
    }

    SubstHead(Align) {
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmAlign>(res_content), true};
    }

    SubstHead(Abs) {
        if (x->name == name) return {_x, false};
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmAbs>(x->name, x->type, res_content), true};
    }

    SubstHead(App) {
        SubstRes(func); SubstRes(param);
        if (!flag_func && !flag_param) return {_x, false};
        return {std::make_shared<TmApp>(res_func, res_param), true};
    }

    SubstHead(Fix) {
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmFix>(res_content), true};
    }

    SubstHead(If) {
        SubstRes(c); SubstRes(t); SubstRes(f);
        if (!flag_c && !flag_t && !flag_f) return {_x, false};
        return {std::make_shared<TmIf>(res_c, res_t, res_f), true};
    }

    SubstHead(Let) {
        SubstRes(def);
        if (x->name == name) {
            if (!flag_def) return {_x, false};
            return {std::make_shared<TmLet>(x->name, res_def, x->content), true};
        }
        SubstRes(content);
        if (!flag_def && !flag_content) return {_x, false};
        return {std::make_shared<TmLet>(x->name, res_def, res_content), true};
    }

    SubstHead(Proj) {
        SubstRes(content);
        if (!flag_content) return {_x, false};
        return {std::make_shared<TmProj>(res_content, x->id), true};
    }

    std::pair<Term, bool> _subst(const Term& x, const std::string& name, const Term& y) {
        switch (x->getType()) {
            case TermType::VALUE: return {x, false};
            case TermType::VAR: SubstCase(Var);
            case TermType::MATCH: SubstCase(Match);
            case TermType::TUPLE: SubstCase(Tuple);
            case TermType::ABS: SubstCase(Abs);
            case TermType::APP: SubstCase(App);
            case TermType::FIX: SubstCase(Fix);
            case TermType::IF: SubstCase(If);
            case TermType::LET: SubstCase(Let);
            case TermType::PROJ: SubstCase(Proj);
            case TermType::LABEL: SubstCase(Label);
            case TermType::UNLABEL: SubstCase(UnLabel);
            case TermType::ALIGN: SubstCase(Align);
            case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << x->toString();
        }
    }
}

Term incre::subst(const Term &x, const std::string &name, const Term &y) {
    auto [res, _] = _subst(x, name, y);
    /*std::cout << "subst " << x->toString() << std::endl;
    std::cout << "name " << name << " " << y->toString() << std::endl;
    std::cout << "res " << res->toString() << std::endl;
    int kk; std::cin >> kk;*/
    return res;
}

namespace {
    bool _isMatch(const Data& data, PtConstructor* pt) {
        auto* iv = dynamic_cast<VInductive*>(data.get());
        if (!iv || iv->name != pt->name) return false;
        return incre::isMatch(iv->content, pt->pattern);
    }

    bool _isMatch(const Data& data, PtTuple* pt) {
        auto* it = dynamic_cast<VTuple*>(data.get());
        if (!it || it->elements.size() != pt->pattern_list.size()) return false;
        for (int i = 0; i < it->elements.size(); ++i) {
            if (!incre::isMatch(it->elements[i], pt->pattern_list[i])) return false;
        }
        return true;
    }
}

bool incre::isMatch(const Data &data, const Pattern &pt) {
    switch (pt->getType()) {
        case PatternType::UNDER_SCORE:
        case PatternType::VAR:
            return true;
        case PatternType::CONSTRUCTOR:
            return _isMatch(data, dynamic_cast<PtConstructor*>(pt.get()));
        case PatternType::TUPLE:
            return _isMatch(data, dynamic_cast<PtTuple*>(pt.get()));
    }
}

namespace {
    void _bindPattern(const Data& data, const Pattern& pt, std::vector<std::pair<std::string, Term>>& bind_list);

    void _bindPattern(const Data& data, PtVar* pt, std::vector<std::pair<std::string, Term>>& bind_list) {
        bind_list.emplace_back(pt->name, std::make_shared<TmValue>(data));
    }

    void _bindPattern(const Data& data, PtConstructor* pt, std::vector<std::pair<std::string, Term>>& bind_list) {
        auto* iv = dynamic_cast<VInductive*>(data.get());
        if (!iv || pt->name != iv->name) {
            LOG(FATAL) << "Match failed: expected VInductive but get " << data.toString();
        }
        _bindPattern(iv->content, pt->pattern, bind_list);
    }

    void _bindPattern(const Data& data, PtTuple* pt, std::vector<std::pair<std::string, Term>>& bind_list) {
        auto* it = dynamic_cast<VTuple*>(data.get());
        if (!it || it->elements.size() != pt->pattern_list.size()) {
            LOG(FATAL) << "Match failed: expected VTuple with " << pt->pattern_list.size() << " fields but get " <<data.toString();
        }
        for (int i = 0; i < it->elements.size(); ++i) {
            _bindPattern(it->elements[i], pt->pattern_list[i], bind_list);
        }
    }

    void _bindPattern(const Data& data, const Pattern& pt, std::vector<std::pair<std::string, Term>>& bind_list) {
        switch (pt->getType()) {
            case PatternType::UNDER_SCORE: return;
            case PatternType::VAR:
                _bindPattern(data, dynamic_cast<PtVar*>(pt.get()), bind_list);
                return;
            case PatternType::CONSTRUCTOR:
                _bindPattern(data, dynamic_cast<PtConstructor*>(pt.get()), bind_list);
                return;
            case PatternType::TUPLE:
                _bindPattern(data, dynamic_cast<PtTuple*>(pt.get()), bind_list);
                return;
        }
    }
}

std::vector<std::pair<std::string, Term>> incre::bindPattern(const Data &data, const Pattern &pt) {
    std::vector<std::pair<std::string, Term>> bind_list;
    _bindPattern(data, pt, bind_list);
    return bind_list;
}

namespace {
    void _getPatternVars(const Pattern& pattern, std::vector<std::string>& res) {
        switch (pattern->getType()) {
            case PatternType::UNDER_SCORE: return;
            case PatternType::VAR: {
                auto* pv = dynamic_cast<PtVar*>(pattern.get());
                res.push_back(pv->name);
                return;
            }
            case PatternType::CONSTRUCTOR: {
                auto* pc = dynamic_cast<PtConstructor*>(pattern.get());
                _getPatternVars(pc->pattern, res);
                return;
            }
            case PatternType::TUPLE: {
                auto* pt = dynamic_cast<PtTuple*>(pattern.get());
                for (auto& sub_pattern: pt->pattern_list) {
                    _getPatternVars(sub_pattern, res);
                }
                return;
            }
        }
    }
}

std::vector<std::string> incre::getPatternVars(const Pattern &pt) {
    std::vector<std::string> res;
    _getPatternVars(pt, res);
    return res;
}

namespace {
#define RunHead(type) Data _run(Tm ## type* term, const Term& _term, Context* ctx)
#define RunCase(type) return _run(dynamic_cast<Tm ## type*>(term.get()), term, ctx)

    RunHead(Value) {
        return term->data;
    }

    RunHead(Var) {
        auto name = term->name;
        return incre::run(ctx->getTerm(name), ctx);
    }

    RunHead(Match) {
        auto v = incre::run(term->def, ctx);
        for (const auto& [pt, sub_term]: term->cases) {
            if (incre::isMatch(v, pt)) {
                auto binds = incre::bindPattern(v, pt);
                auto res = sub_term;
                for (int i = int(binds.size()) - 1; i >= 0; --i) {
                    auto& [name, bind] = binds[i];
                    res = incre::subst(res, name, bind);
                }
                return incre::run(res, ctx);
            }
        }
        throw SemanticsError();
    }

    RunHead(Tuple) {
        DataList res;
        for (const auto& sub_term: term->fields) {
            res.push_back(incre::run(sub_term, ctx));
        }
        return Data(std::make_shared<VTuple>(res));
    }

    RunHead(Label) {
        auto res = incre::run(term->content, ctx);
        return Data(std::make_shared<VCompress>(res));
    }

    RunHead(UnLabel) {
        auto res = incre::run(term->content, ctx);
        auto* cv = dynamic_cast<VCompress*>(res.get());
        if (!cv) {
            LOG(FATAL) << term->content->toString() << " does not return a compressed value";
        }
        return cv->content;
    }

    RunHead(Align) {
        return incre::run(term->content, ctx);
    }

    RunHead(Abs) {
        return Data(std::make_shared<VAbsFunction>(_term));
    }

    RunHead(App) {
        auto func = incre::run(term->func, ctx);
        auto* fv = dynamic_cast<VFunction*>(func.get());
        if (!fv) {
            LOG(FATAL) << term->func->toString() << " is not a function.";
        }
        auto param = incre::run(term->param, ctx);
        return fv->run(std::make_shared<TmValue>(param), ctx);
    }

    RunHead(Fix) {
        auto f = incre::run(term->content, ctx);
        auto* fv = dynamic_cast<VFunction*>(f.get());
        if (!fv) {
            LOG(FATAL) << term->content->toString() << " is not a function.";
        }
        return fv->run(_term, ctx);
    }

    RunHead(If) {
        auto c = incre::run(term->c, ctx);
        auto* bv = dynamic_cast<VBool*>(c.get());
        if (!bv) {
            LOG(FATAL) << term->c->toString() << " is not a boolean value.";
        }
        if (bv->w) return incre::run(term->t, ctx);
        return incre::run(term->f, ctx);
    }

    RunHead(Let) {
        auto def = incre::run(term->def, ctx);
        auto res = incre::subst(term->content, term->name, std::make_shared<TmValue>(def));
        return incre::run(res, ctx);
    }

    RunHead(Proj) {
        auto content = incre::run(term->content, ctx);
        auto* vt = dynamic_cast<VTuple*>(content.get());
        if (!vt) {
            LOG(FATAL) << term->content->toString() << " is not a tuple.";
        }
        if (term->id <= 0 || term->id > vt->elements.size()) {
            LOG(FATAL) << "Invalid index " << term->id << " for " << term->content->toString();
        }
        return vt->elements[term->id - 1];
    }
}

Data incre::run(const Term &term, Context* ctx) {
    // std::cout << "run " << term->toString() << std::endl;
    switch (term->getType()) {
        case TermType::VALUE: RunCase(Value);
        case TermType::VAR: RunCase(Var);
        case TermType::MATCH: RunCase(Match);
        case TermType::TUPLE: RunCase(Tuple);
        case TermType::LABEL: RunCase(Label);
        case TermType::UNLABEL: RunCase(UnLabel);
        case TermType::ALIGN: RunCase(Align);
        case TermType::ABS: RunCase(Abs);
        case TermType::APP: RunCase(App);
        case TermType::FIX: RunCase(Fix);
        case TermType::IF: RunCase(If);
        case TermType::LET: RunCase(Let);
        case TermType::PROJ: RunCase(Proj);
        case TermType::WILDCARD: LOG(FATAL) << "Unknown WildCard: " << term->toString();
    }
}

namespace {
    void _run(CommandImport* command, Context* ctx) {
        for (auto& sub_command: command->commands) {
            incre::run(sub_command, ctx);
        }
    }

    void _run(CommandBind* command, Context* ctx) {
        switch (command->binding->getType()) {
            case BindingType::TYPE: {
                auto* ty_bind = dynamic_cast<TypeBinding*>(command->binding.get());
                ctx->addBinding(command->name, ty_bind->type);
                return;
            }
            case BindingType::TERM: {
                auto* term_bind = dynamic_cast<TermBinding*>(command->binding.get());
                auto ty = incre::getType(term_bind->term, ctx);
                Data res = run(term_bind->term, ctx);
                ctx->addBinding(command->name, std::make_shared<TmValue>(res), ty);
                return;
            }
            case BindingType::VAR: {
                auto* var_bind = dynamic_cast<VarTypeBinding*>(command->binding.get());
                ctx->addBinding(command->name, var_bind->type);
                return;
            }
        }
    }

    Term _buildConstructor(const std::string& name, const Ty& type) {
        auto func = [name](const DataList& inps) {
            return Data(std::make_shared<VInductive>(name, inps[0]));
        };
        Data data(std::make_shared<VOpFunction>(name, 1, func, type));
        return std::make_shared<TmValue>(data);
    }

    void _run(CommandDefInductive* command, Context* ctx) {
        ctx->addBinding(command->type->name, command->_type);
        for (const auto& [name, sub_ty]: command->type->constructors) {
            auto ity = incre::subst(sub_ty, command->type->name, command->_type);
            auto ty = std::make_shared<TyArrow>(ity, command->_type);
            ctx->addBinding(name, _buildConstructor(name, ty), ty);
        }
    }
}

void incre::run(const Command &command, Context *ctx) {
    switch (command->getType()) {
        case CommandType::IMPORT:
            _run(dynamic_cast<CommandImport*>(command.get()), ctx);
            return;
        case CommandType::BIND:
            _run(dynamic_cast<CommandBind*>(command.get()), ctx);
            return;
        case CommandType::DEF_IND:
            _run(dynamic_cast<CommandDefInductive*>(command.get()), ctx);
            return;
    }
}

void incre::run(const IncreProgram &program, Context *ctx) {
    for (auto& command: program->commands) {
        incre::run(command, ctx);
    }
}

Context * incre::run(const IncreProgram &program) {
    auto* ctx = new Context();
    incre::run(program, ctx);
    return ctx;
}