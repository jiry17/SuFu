//
// Created by pro on 2023/1/22.
//

#include "istool/incre/language/incre.h"
#include "glog/logging.h"

using namespace incre;

namespace {
#define ClearCompressHead(name) Ty _clearCompress(Ty ## name* type, const Ty& _type)
#define ClearCompressCase(name) return _clearCompress(dynamic_cast<Ty ## name*>(type.get()), type)

    ClearCompressHead(Arrow) {
        auto source = clearCompress(type->source), target = clearCompress(type->target);
        return std::make_shared<TyArrow>(source, target);
    }

    ClearCompressHead(Tuple) {
        TyList fields;
        for (auto& field: type->fields) {
            fields.push_back(clearCompress(field));
        }
        return std::make_shared<TyTuple>(fields);
    }

    ClearCompressHead(Compress) {
        return clearCompress(type->content);
    }

    ClearCompressHead(Inductive) {
        std::vector<std::pair<std::string, Ty>> cons_list;
        for (auto& [cons_name, cons_type]: type->constructors) {
            cons_list.emplace_back(cons_name, clearCompress(cons_type));
        }
        return std::make_shared<TyInductive>(type->name, cons_list);
    }

}

Ty incre::clearCompress(const Ty &type) {
    switch (type->getType()) {
        case TyType::BOOL:
        case TyType::INT:
        case TyType::UNIT:
        case TyType::VAR: return type;
        case TyType::ARROW: ClearCompressCase(Arrow);
        case TyType::TUPLE: ClearCompressCase(Tuple);
        case TyType::COMPRESS: ClearCompressCase(Compress);
        case TyType::IND: ClearCompressCase(Inductive);
    }
}


int incre::getTypeSize(TyData *type) {
    switch (type->getType()) {
        case TyType::VAR:
        case TyType::INT:
        case TyType::BOOL:
        case TyType::UNIT: return 1;
        case TyType::COMPRESS: {
            auto* ct = dynamic_cast<TyCompress*>(type);
            return 1 + incre::getTypeSize(ct->content.get());
        }
        case TyType::ARROW: {
            auto* ta = dynamic_cast<TyArrow*>(type);
            return 1 + incre::getTypeSize(ta->source.get()) + incre::getTypeSize(ta->target.get());
        }
        case TyType::TUPLE: {
            auto* tt = dynamic_cast<TyTuple*>(type);
            int res = 1;
            for (auto& sub_type: tt->fields) {
                res += incre::getTypeSize(sub_type.get());;
            }
            return res;
        }
        case TyType::IND: {
            auto* ti = dynamic_cast<TyInductive*>(type);
            int res = 1;
            for (auto& [name, sub_type]: ti->constructors) {
                res += incre::getTypeSize(sub_type.get());
            }
            return res;
        }
    }
}

int incre::getTermSize(TermData *term) {
    auto subs = getSubTerms(term);
    int res = 1;
    for (auto& sub: subs) {
        res += getTermSize(sub.get());
    }
    if (term->getType() == TermType::ABS) {
        auto* ta = dynamic_cast<TmAbs*>(term);
        res += incre::getTypeSize(ta->type.get());
    }
    return res;
}

int incre::getCommandSize(CommandData *command) {
    switch (command->getType()) {
        case CommandType::IMPORT: return 0;
        case CommandType::BIND: {
            auto* cb = dynamic_cast<CommandBind*>(command);
            switch (cb->binding->getType()) {
                case BindingType::VAR: return 0;
                case BindingType::TERM: {
                    auto* bt = dynamic_cast<TermBinding*>(cb->binding.get());
                    return incre::getTermSize(bt->term.get());
                }
                case BindingType::TYPE: {
                    auto* bt = dynamic_cast<TypeBinding*>(cb->binding.get());
                    return incre::getTypeSize(bt->type.get());
                }
            }
        }
        case CommandType::DEF_IND: {
            auto* cd = dynamic_cast<CommandDefInductive*>(command);
            return incre::getTypeSize(cd->type);
        }
    }
}

int incre::getProgramSize(ProgramData *program) {
    int res = 0;
    for (auto& command: program->commands) {
        res += getCommandSize(command.get());
    }
    return res;
}

TermList incre::getSubTerms(TermData *term) {
    switch (term->getType()) {
        case TermType::VALUE:
        case TermType::VAR: return {};
        case TermType::ALIGN: {
            auto* ta = dynamic_cast<TmAlign*>(term);
            return {ta->content};
        }
        case TermType::LABEL: {
            auto* tl = dynamic_cast<TmLabel*>(term);
            return {tl->content};
        }
        case TermType::UNLABEL: {
            auto* tu = dynamic_cast<TmUnLabel*>(term);
            return {tu->content};
        }
        case TermType::APP: {
            auto* ta = dynamic_cast<TmApp*>(term);
            return {ta->func, ta->param};
        }
        case TermType::ABS: {
            auto* ta = dynamic_cast<TmAbs*>(term);
            return {ta->content};
        }
        case TermType::LET: {
            auto* tl = dynamic_cast<TmLet*>(term);
            return {tl->def, tl->content};
        }
        case TermType::MATCH: {
            auto* tm = dynamic_cast<TmMatch*>(term);
            TermList res = {tm->def};
            for (auto& [_, sub]: tm->cases) res.push_back(sub);
            return res;
        }
        case TermType::TUPLE: {
            auto* tt = dynamic_cast<TmTuple*>(term);
            return tt->fields;
        }
        case TermType::PROJ: {
            auto* tp = dynamic_cast<TmProj*>(term);
            return {tp->content};
        }
        case TermType::IF: {
            auto* ti = dynamic_cast<TmIf*>(term);
            return {ti->c, ti->t, ti->f};
        }
        case TermType::FIX: {
            auto* tf = dynamic_cast<TmFix*>(term);
            return {tf->content};
        }
        case TermType::WILDCARD: {
            LOG(FATAL) << "Unknown WILDCARD: " << term->toString();
        }
    }
}