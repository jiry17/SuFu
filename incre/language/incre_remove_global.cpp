//
// Created by pro on 2023/6/27.
//

#include "istool/incre/language/incre.h"
#include "istool/incre/language/incre_lookup.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::match;

namespace {
    typedef std::pair<std::string, Ty> GlobalInfo;
    typedef std::vector<GlobalInfo> GlobalInfoList;

    Term _buildTerm(const Term& term, const GlobalInfoList &info_list, const MatchContext& ctx, std::vector<std::string>& tmp_list);

#define BuildHead(name) Term _buildTerm(Tm ## name* term, const Term& _term, const GlobalInfoList &info_list, const MatchContext& ctx, std::vector<std::string>& tmp_list)
#define BuildCase(name) return _buildTerm(dynamic_cast<Tm ## name*>(term.get()), term, info_list, ctx, tmp_list)

    bool _isTmp(const std::string& name, const std::vector<std::string>& tmp_list) {
        for (auto& tmp_name: tmp_list) if (tmp_name == name) return true;
        return false;
    }

    BuildHead(Var) {
        if (_isTmp(term->name, tmp_list) || !ctx.find(term->name)->second) return _term;
        for (int i = 0; i < info_list.size(); ++i) {
            if (term->name == info_list[i].first) {
                return std::make_shared<TmVar>("global" + std::to_string(i));
            }
        }
        auto res = _term;
        for (int i = 0; i < info_list.size(); ++i) {
            auto param = std::make_shared<TmVar>("global" + std::to_string(i));
            res = std::make_shared<TmApp>(res, param);
        }
        return res;
    }

    BuildHead(If) {
        auto c = _buildTerm(term->c, info_list, ctx, tmp_list);
        auto t = _buildTerm(term->t, info_list, ctx, tmp_list);
        auto f = _buildTerm(term->f, info_list, ctx, tmp_list);
        return std::make_shared<TmIf>(c, t, f);
    }
    BuildHead(Proj) {
        auto content = _buildTerm(term->content, info_list, ctx, tmp_list);
        return std::make_shared<TmProj>(content, term->id);
    }
    BuildHead(Tuple) {
        TermList fields;
        for (auto& sub_term: term->fields) {
            fields.push_back(_buildTerm(sub_term, info_list, ctx, tmp_list));
        }
        return std::make_shared<TmTuple>(fields);
    }
    BuildHead(Let) {
        auto def = _buildTerm(term->def, info_list, ctx, tmp_list);
        tmp_list.push_back(term->name);
        auto content = _buildTerm(term->content, info_list, ctx, tmp_list);
        tmp_list.pop_back();
        return std::make_shared<TmLet>(term->name, def, content);
    }
    BuildHead(App) {
        auto func = _buildTerm(term->func, info_list, ctx, tmp_list);
        auto param = _buildTerm(term->param, info_list, ctx, tmp_list);
        return std::make_shared<TmApp>(func, param);
    }
    BuildHead(Abs) {
        tmp_list.push_back(term->name);
        auto content = _buildTerm(term->content, info_list, ctx, tmp_list);
        tmp_list.pop_back();
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }
    BuildHead(Match) {
        std::vector<std::pair<Pattern, Term>> cases;
        auto def = _buildTerm(term->def, info_list, ctx, tmp_list);
        for (auto& [pt, sub_term]: term->cases) {
            auto names = incre::getPatternVars(pt);
            for (auto& name: names) tmp_list.push_back(name);
            cases.emplace_back(pt, _buildTerm(sub_term, info_list, ctx, tmp_list));
            for (auto& _: names) tmp_list.pop_back();
        }
        return std::make_shared<TmMatch>(def, cases);
    }
    BuildHead(Fix) {
        auto content = _buildTerm(term->content, info_list, ctx, tmp_list);
        return std::make_shared<TmFix>(content);
    }

    Term _buildTerm(const Term& term, const GlobalInfoList &info_list, const MatchContext& ctx, std::vector<std::string>& tmp_list) {
        switch (term->getType()) {
            case TermType::VAR: BuildCase(Var);
            case TermType::VALUE: return term;
            case TermType::IF: BuildCase(If);
            case TermType::PROJ: BuildCase(Proj);
            case TermType::MATCH: BuildCase(Match);
            case TermType::ABS: BuildCase(Abs);
            case TermType::APP: BuildCase(App);
            case TermType::TUPLE: BuildCase(Tuple);
            case TermType::LABEL:
            case TermType::UNLABEL:
            case TermType::ALIGN:
            case TermType::WILDCARD: LOG(FATAL) << "Unexpected";
            case TermType::FIX: BuildCase(Fix);
            case TermType::LET: BuildCase(Let);
        }
    }
}

IncreProgram incre::removeGlobal(ProgramData *program) {
    GlobalInfoList info_list;
    for (auto& command: program->commands) {
        if (command->isDecoratedWith(CommandDecorate::INPUT)) {
            assert(command->getType() == CommandType::BIND);
            auto* cb = dynamic_cast<CommandBind*>(command.get());
            assert(cb->binding->getType() == BindingType::VAR);
            auto* bv = dynamic_cast<VarTypeBinding*>(cb->binding.get());
            info_list.emplace_back(cb->name, bv->type);
        }
    }

    MatchContext ctx;
    CommandList result;
    MatchTask task;
    for (auto& command: program->commands) {
        if (command->getType() == CommandType::IMPORT) {
            result.push_back(command); continue;
        }
        if (command->getType() == CommandType::DEF_IND) {
            auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
            result.push_back(command);
            ctx[cd->type->name] = false;
            for (auto& [cons_name, _]: cd->type->constructors) {
                ctx[cons_name] = false;
            }
            continue;
        }
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        switch (cb->binding->getType()) {
            case BindingType::VAR: {
                auto flag = cb->isDecoratedWith(CommandDecorate::INPUT);
                ctx[cb->name] = flag;
                if (!flag) result.push_back(command);
                break;
            }
            case BindingType::TYPE: {
                result.push_back(command);
                ctx[cb->name] = false;
                break;
            }
            case BindingType::TERM: {
                auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
                auto is_related = match::match(tb->term.get(), {}, ctx);
                if (!is_related) {
                    ctx[cb->name] = false; result.push_back(command); break;
                }
                std::vector<std::string> tmp_list;
                auto res = _buildTerm(tb->term, info_list, ctx, tmp_list);
                assert(tmp_list.size() == 0);
                for (int i = int(info_list.size()) - 1; i >= 0; --i) {
                    res = std::make_shared<TmAbs>("global" + std::to_string(i), info_list[i].second, res);
                }
                auto new_binding = std::make_shared<TermBinding>(res);
                result.push_back(std::make_shared<CommandBind>(cb->name, new_binding, cb->decorate_set));
                ctx[cb->name] = true;
                break;
            }
        }
    }
    return std::make_shared<ProgramData>(result, program->config_map);
}