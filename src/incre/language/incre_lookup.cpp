//
// Created by pro on 2022/11/22.
//

#include "istool/incre/language/incre_lookup.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::match;

MatchTask::MatchTask():
    term_matcher([](TermData*, const MatchContext&){return false;}),
    type_matcher([](TyData*, const MatchContext&){return false;}) {
}

namespace {
    bool _isTmp(const std::string& name, const std::vector<std::string>& tmp_list) {
        for (auto& tmp_name: tmp_list) if (name == tmp_name) return true;
        return false;
    }

    bool _isTypeMatch(TyData* type, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names);

#define TypeHead(name) bool _isTypeMatchCase(Ty ## name* type, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names)
#define TypeCase(name) return _isTypeMatchCase(dynamic_cast<Ty ## name*>(type), task, ctx, tmp_names)

    TypeHead(Var) {
        return !_isTmp(type->name, tmp_names) && ctx.find(type->name)->second;
    }
    TypeHead(Tuple) {
        for (auto& field: type->fields) {
            if (_isTypeMatch(field.get(), task, ctx, tmp_names)) return true;
        }
        return false;
    }
    TypeHead(Arrow) {
        return _isTypeMatch(type->source.get(), task, ctx, tmp_names) || _isTypeMatch(type->target.get(), task, ctx, tmp_names);
    }
    TypeHead(Inductive) {
        tmp_names.push_back(type->name);
        for (auto& [name, ctype]: type->constructors) {
            if (_isTypeMatch(ctype.get(), task, ctx, tmp_names)) {
                tmp_names.pop_back(); return true;
            }
        }
        tmp_names.pop_back(); return false;
    }
    TypeHead(Compress) {
        return _isTypeMatch(type->content.get(), task, ctx, tmp_names);
    }

    bool _isTypeMatch(TyData* type, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names) {
        if (task.type_matcher(type, ctx)) return true;
        switch (type->getType()) {
            case TyType::INT:
            case TyType::UNIT:
            case TyType::BOOL: return false;
            case TyType::VAR: TypeCase(Var);
            case TyType::TUPLE: TypeCase(Tuple);
            case TyType::ARROW: TypeCase(Arrow);
            case TyType::IND: TypeCase(Inductive);
            case TyType::COMPRESS: TypeCase(Compress);
        }
    }
    bool _isTypeMatch(TyData* type, const MatchTask& task, const MatchContext& ctx) {
        std::vector<std::string> tmp_names;
        return _isTypeMatch(type, task, ctx, tmp_names);
    }

    void _patternNames(PatternData* pt, std::vector<std::string>& res) {
        switch (pt->getType()) {
            case PatternType::TUPLE: {
                auto* tp = dynamic_cast<PtTuple*>(pt);
                for (auto& sub: tp->pattern_list) {
                    _patternNames(sub.get(), res);
                }
                return;
            }
            case PatternType::VAR: {
                auto* vp = dynamic_cast<PtVar*>(pt);
                res.push_back(vp->name);
                return;
            }
            case PatternType::CONSTRUCTOR: {
                auto* cp = dynamic_cast<PtConstructor*>(pt);
                _patternNames(cp->pattern.get(), res);
                return;
            }
            case PatternType::UNDER_SCORE: {
                return;
            }
        }
    }

    std::vector<std::string> _patternNames(PatternData* pt) {
        std::vector<std::string> res;
        _patternNames(pt, res);
        return res;
    }

    bool _isTermMatch(TermData* term, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names);

#define TermHead(name) bool _isTermMatch(Tm ## name* term, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names)
#define TermCase(name) return _isTermMatch(dynamic_cast<Tm ## name*>(term), task, ctx, tmp_names)

    TermHead(Tuple) {
        for (auto& field: term->fields) {
            if (_isTermMatch(field.get(), task, ctx, tmp_names)) return true;
        }
        return false;
    }
    TermHead(Var) {
        return !_isTmp(term->name, tmp_names) && ctx.find(term->name)->second;
    }
    TermHead(Match) {
        if (_isTermMatch(term->def.get(), task, ctx, tmp_names)) {
            return true;
        }
        auto res = false;
        for (auto& [pt, sub_term]: term->cases) {
            std::vector<std::string> names = _patternNames(pt.get());
            for (const auto& name: names) tmp_names.push_back(name);
            res |= _isTermMatch(sub_term.get(), task, ctx, tmp_names);
            for (const auto& _: names) tmp_names.pop_back();
            if (res) return true;
        }
        return false;
    }
    TermHead(Abs) {
        if (_isTypeMatch(term->type.get(), task, ctx, tmp_names)) return true;
        tmp_names.push_back(term->name);
        auto res = _isTermMatch(term->content.get(), task, ctx, tmp_names);
        tmp_names.pop_back();
        return res;
    }
    TermHead(Proj) {
        return _isTermMatch(term->content.get(), task, ctx, tmp_names);
    }
    TermHead(App) {
        return _isTermMatch(term->func.get(), task, ctx, tmp_names) || _isTermMatch(term->param.get(), task, ctx, tmp_names);
    }
    TermHead(Let) {
        if (_isTermMatch(term->def.get(), task, ctx, tmp_names)) return true;
        tmp_names.push_back(term->name);
        auto res = _isTermMatch(term->content.get(), task, ctx, tmp_names);
        tmp_names.pop_back();
        return res;
    }
    TermHead(Fix) {
        return _isTermMatch(term->content.get(), task, ctx, tmp_names);
    }
    TermHead(If) {
        for (auto& sub_term: {term->t, term->c, term->f}) {
            if (_isTermMatch(sub_term.get(), task, ctx, tmp_names)) return true;
        }
        return false;
    }
    TermHead(Label) {
        return _isTermMatch(term->content.get(), task, ctx, tmp_names);
    }
    TermHead(UnLabel) {
        return _isTermMatch(term->content.get(), task, ctx, tmp_names);
    }
    TermHead(Align) {
        return _isTermMatch(term->content.get(), task, ctx, tmp_names);
    }
    bool _isTermMatch(TermData* term, const MatchTask& task, const MatchContext& ctx, std::vector<std::string>& tmp_names) {
        if (task.term_matcher(term, ctx)) return true;
        switch (term->getType()) {
            case TermType::VALUE: return false;
            case TermType::LABEL: TermCase(Label);
            case TermType::UNLABEL: TermCase(UnLabel);
            case TermType::ALIGN: TermCase(Align);
            case TermType::TUPLE: TermCase(Tuple);
            case TermType::VAR: TermCase(Var);
            case TermType::MATCH: TermCase(Match);
            case TermType::ABS: TermCase(Abs);
            case TermType::APP: TermCase(App);
            case TermType::PROJ: TermCase(Proj);
            case TermType::LET: TermCase(Let);
            case TermType::FIX: TermCase(Fix);
            case TermType::IF: TermCase(If);
            case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << term->toString();
        }
    }

    bool _isTermMatch(TermData* term, const MatchTask& task, const MatchContext& ctx) {
        std::vector<std::string> tmp_names;
        return _isTermMatch(term, task, ctx, tmp_names);
    }

    bool _processBindingCompress(BindingData* binding, const MatchTask& task, const MatchContext& ctx) {
        switch (binding->getType()) {
            case BindingType::TYPE: {
                auto* tb = dynamic_cast<TypeBinding*>(binding);
                return _isTypeMatch(tb->type.get(), task, ctx);
            }
            case BindingType::TERM: {
                auto* tb = dynamic_cast<TermBinding*>(binding);
                return _isTermMatch(tb->term.get(), task, ctx);
            }
            case BindingType::VAR: {
                auto* vb = dynamic_cast<VarTypeBinding*>(binding);
                return _isTypeMatch(vb->type.get(), task, ctx);
            }
        }
    }
}

bool incre::match::match(TyData *type, const MatchTask &task, const MatchContext& ctx) {
    return _isTypeMatch(type, task, ctx);
}
bool incre::match::match(TermData* tm, const MatchTask& task, const MatchContext& ctx) {
    return _isTermMatch(tm, task, ctx);
}

MatchContext incre::match::match(ProgramData *program, const MatchTask &task) {
    MatchContext ctx;
    for (auto& command: program->commands) {
        switch (command->getType()) {
            case CommandType::IMPORT:
                LOG(FATAL) << "Unsupported command IMPORT";
            case CommandType::DEF_IND: {
                auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
                bool res = _isTypeMatch(cd->type, task, ctx);
                ctx[cd->type->name] = res;
                for (const auto& [name, _]: cd->type->constructors) ctx[name] = res;
                break;
            }
            case CommandType::BIND: {
                auto* cb = dynamic_cast<CommandBind*>(command.get());
                ctx[cb->name] = _processBindingCompress(cb->binding.get(), task, ctx);
                break;
            }
        }
    }
    return ctx;
}