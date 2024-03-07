//
// Created by pro on 2023/6/22.
//

#include "istool/incre/language/incre.h"
#include "glog/logging.h"

using namespace incre;

namespace {
    Data _run(const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& ext_map);
    Data _invokeClosure(VClosure* vc, const Data& param, AddressHolder* holder, const ExternalEnvRuleMap& ext_map) {
        auto* new_env = holder->extend(vc->env, vc->name, param);
        return _run(vc->term, new_env, holder, ext_map);
    }

#define RunCase(name) return _run(dynamic_cast<Tm ## name*>(term.get()), env, holder, ext_map)
#define RunHead(name) Data _run(Tm ## name* term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& ext_map)
#define VRec(name) _run(term->name, env, holder, ext_map)
#define NRec(name) auto name = VRec(name)

    RunHead(Var) {
        return holder->lookup(env, term->name);
    }
    RunHead(If) {
        NRec(c);
        if (c.isTrue()) return VRec(t); else return VRec(f);
    }
    RunHead(Tuple) {
        DataList fields;
        for (auto& sub_term: term->fields) {
            fields.push_back(_run(sub_term, env, holder, ext_map));
        }
        return BuildData(Product, fields);
    }
    RunHead(Label) {
        NRec(content);
        return Data(std::make_shared<VCompress>(content));
    }
    RunHead(UnLabel) {
        NRec(content); auto* v = dynamic_cast<VCompress*>(content.get());
        assert(v); return v->content;
    }
    RunHead(Align) {
        return VRec(content);
    }
    RunHead(Value) {
        return term->data;
    }
    RunHead(App) {
        NRec(func); NRec(param);
        return incre::runApp(func, param, holder, ext_map);
    }
    RunHead(Proj) {
        NRec(content);
        auto* tv = dynamic_cast<VTuple*>(content.get());
        return tv->get(term->id - 1);
    }
    RunHead(Abs) {
        return Data(std::make_shared<VClosure>(env, term->name, term->content));
    }
    RunHead(Fix) {
        NRec(content);
        auto* vc = dynamic_cast<VClosure*>(content.get()); assert(vc);
        auto* new_env = holder->extend(env, vc->name, Data());
        auto res = _run(vc->term, new_env, holder, ext_map);
        new_env->v = res;
        return res;
    }
    RunHead(Let) {
        NRec(def); auto* new_env = holder->extend(env, term->name, def);
        return _run(term->content, new_env, holder, ext_map);
    }
    RunHead(Match) {
        NRec(def);
        for (auto& [pt, branch]: term->cases) {
            if (isMatch(def, pt)) {
                auto binds = bindPattern(def, pt);
                auto* new_env = env;
                for (auto& [name, v]: binds) {
                    auto* tv = dynamic_cast<TmValue*>(v.get()); assert(tv);
                    new_env = holder->extend(new_env, name, tv->data);
                }
                return _run(branch, new_env, holder, ext_map);
            }
        }
        throw SemanticsError();
    }

    Data _run(const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& ext_map) {
        auto it = ext_map.find(term->getType());
        if (it != ext_map.end()) {
            return it->second.func(term, env, holder, ext_map);
        }
        switch (term->getType()) {
            case TermType::VAR: RunCase(Var);
            case TermType::IF: RunCase(If);
            case TermType::TUPLE: RunCase(Tuple);
            case TermType::LABEL: RunCase(Label);
            case TermType::UNLABEL: RunCase(UnLabel);
            case TermType::WILDCARD: LOG(FATAL) << "Unexpected term " << term->toString();
            case TermType::ALIGN: RunCase(Align);
            case TermType::VALUE: RunCase(Value);
            case TermType::APP: RunCase(App);
            case TermType::PROJ: RunCase(Proj);
            case TermType::ABS: RunCase(Abs);
            case TermType::FIX: RunCase(Fix);
            case TermType::LET: RunCase(Let);
            case TermType::MATCH: RunCase(Match);
        }
    }
}

Data incre::envRun(const Term &term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap &map) {
    return _run(term, env, holder, map);
}

Data incre::runApp(const Data &func, const Data &param, AddressHolder *holder, const ExternalEnvRuleMap &map) {
    auto* cv = dynamic_cast<VClosure*>(func.get());
    if (cv) {
        return _invokeClosure(cv, param, holder, map);
    }
    assert(dynamic_cast<VOpFunction*>(func.get()) || dynamic_cast<VPartialOpFunction*>(func.get()));
    auto* vf = dynamic_cast<VFunction*>(func.get());
    return vf->run(std::make_shared<TmValue>(param), nullptr);
}


Data _buildConstructor(const std::string& name, const Ty& type) {
    auto func = [name](const DataList& inps) {
        return Data(std::make_shared<VInductive>(name, inps[0]));
    };
    return Data(std::make_shared<VOpFunction>(name, 1, func, type));
}

void incre::envRun(const Command &command, EnvContext* ctx, const ExternalEnvRuleMap &map) {
    auto* holder = ctx->holder;
    switch (command->getType()) {
        case CommandType::IMPORT: LOG(FATAL) << "Unexpected";
        case CommandType::BIND: {
            auto* cb = dynamic_cast<CommandBind*>(command.get()); assert(cb);
            switch (cb->binding->getType()) {
                case BindingType::TYPE: break;
                case BindingType::VAR: {
                    auto* bv = dynamic_cast<VarTypeBinding*>(cb->binding.get());
                    ctx->start = holder->extend(ctx->start, cb->name, {});
                    ctx->hole_map[cb->name] = ctx->start;
                    break;
                }
                case BindingType::TERM: {
                    auto* bt = dynamic_cast<TermBinding*>(cb->binding.get());
                    auto data = _run(bt->term, ctx->start, holder, map);
                    auto it = ctx->hole_map.find(cb->name);
                    if (it == ctx->hole_map.end()) {
                        ctx->start = holder->extend(ctx->start, cb->name, data);
                    } else {
                        it->second->v = data; ctx->hole_map.erase(it);
                    }
                }
            }
            break;
        }
        case CommandType::DEF_IND: {
            auto* ci = dynamic_cast<CommandDefInductive*>(command.get());
            for (const auto& [name, sub_ty]: ci->type->constructors) {
                auto ity = incre::subst(sub_ty, ci->type->name, ci->_type);
                auto ty = std::make_shared<TyArrow>(ity, ci->_type);
                ctx->start = holder->extend(ctx->start, name, _buildConstructor(name, ty));
            }
            break;
        }
    }
}

void incre::envRun(ProgramData* program, EnvContext *ctx, const ExternalEnvRuleMap &map) {
    for (auto& command: program->commands) {
        envRun(command, ctx, map);
    }
}

EnvContext *incre::envRun(ProgramData* program, const ExternalEnvRuleMap &map) {
    auto* holder = new AddressHolder();
    auto* ctx = new EnvContext(holder);
    envRun(program, ctx, map);
    return ctx;
}