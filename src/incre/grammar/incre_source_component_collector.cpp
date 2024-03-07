//
// Created by pro on 2023/4/5.
//

#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "glog/logging.h"

using namespace incre;
using namespace incre::grammar;

namespace {
    struct _UserProvidedComponentInfo {
        std::string name;
        bool is_compress_related, is_recursive, is_partial;
        int command_id;
        _UserProvidedComponentInfo(const std::string& _name, bool _is_compress_related, bool _is_recursive, bool _is_partial, int _command_id):
            name(_name), is_compress_related(_is_compress_related), is_recursive(_is_recursive), is_partial(_is_partial), command_id(_command_id) {
        }
        ~_UserProvidedComponentInfo() = default;
    };
    typedef std::unordered_map<std::string, _UserProvidedComponentInfo> _UserComponentInfoMap;

    _UserComponentInfoMap _constructInfoMap(ProgramData* program) {
        match::MatchTask compress_task, recursive_task;
        compress_task.term_matcher = [](TermData* term, const match::MatchContext& ctx) -> bool{
            return term->getType() == TermType::LABEL || term->getType() == TermType::UNLABEL || term->getType() == TermType::ALIGN;
        };
        compress_task.type_matcher = [](TyData* term, const match::MatchContext& ctx) -> bool {
            return term->getType() == TyType::COMPRESS;
        };
        recursive_task.term_matcher = [](TermData* term, const match::MatchContext& ctx) -> bool {
            return term->getType() == TermType::FIX;
        };
        auto compress_info = match::match(program, compress_task), recursive_info = match::match(program, recursive_task);

        // Collect command id infos
        std::unordered_map<std::string, int> id_map;
        std::unordered_map<std::string, bool> partial_map;
        for (int command_id = 0; command_id < program->commands.size(); ++command_id) {
            auto& command = program->commands[command_id];
            switch (command->getType()) {
                case CommandType::IMPORT: break;
                case CommandType::DEF_IND: {
                    auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
                    for (auto& [cname, _]: cd->type->constructors) {
                        id_map[cname] = command_id; partial_map[cname] = false;
                    }
                    break;
                }
                case CommandType::BIND: {
                    auto* cb = dynamic_cast<CommandBind*>(command.get());
                    id_map[cb->name] = command_id;
                    partial_map[cb->name] = !command->isDecoratedWith(CommandDecorate::SYN_NO_PARTIAL);
                }
            }
        }

        // Merge all infos
        _UserComponentInfoMap res;
        for (auto& [name, command_id]: id_map) {
            assert(compress_info.count(name) && recursive_info.count(name) && partial_map.count(name));
            res.insert({name, _UserProvidedComponentInfo(name, compress_info[name], recursive_info[name],
                                                         partial_map[name], command_id)});
        }
        return res;
    }
}

ComponentPool collector::collectComponentFromSource(EnvContext* env_ctx, TypeContext* type_ctx, ProgramData *program) {
    ComponentPool pool;
    auto info_map = _constructInfoMap(program);
    std::unordered_set<std::string> name_set;
    std::unordered_set<std::string> extract_wildcard, combine_wildcard;
    for (auto& command: program->commands) {
        switch (command->getType()) {
            case CommandType::DEF_IND: {
                auto* cd = dynamic_cast<CommandDefInductive*>(command.get());
                for (auto& [cons_name, _]: cd->type->constructors) {
                    name_set.insert(cons_name);
                }
                break;
            }
            case CommandType::BIND: {
                auto* cb = dynamic_cast<CommandBind*>(command.get());
                if (cb->binding->getType() != BindingType::TERM) continue;
                if (cb->isDecoratedWith(CommandDecorate::SYN_COMBINE)) combine_wildcard.insert(cb->name);
                if (cb->isDecoratedWith(CommandDecorate::SYN_COMPRESS)) extract_wildcard.insert(cb->name);
                name_set.insert(cb->name);
            }
            case CommandType::IMPORT: break;
        }
    }

    for (auto& name: name_set) {
        auto it = info_map.find(name); assert(it != info_map.end());
        auto component_info = it->second;

        auto full_type = incre::unfoldBasicType(type_ctx->lookup(name), type_ctx);
        auto component_type = incre::typeFromIncre(full_type);
        auto term = std::make_shared<TmVar>(name);
        auto component_value = incre::envRun(term, env_ctx->start, env_ctx->holder);
        auto normal_component = std::make_shared<IncreComponent>(name, component_type, component_value, term,
                                                          component_info.command_id, component_info.is_partial, false);
        auto parallel_component = std::make_shared<IncreComponent>(name, component_type, component_value, term,
                                                                 component_info.command_id, component_info.is_partial, true);

        if (component_info.is_compress_related) continue;
        if (!component_info.is_recursive) {
            pool.compress_list.push_back(normal_component);
            pool.comb_list.push_back(parallel_component);
        } else {
            if (extract_wildcard.find(name) != extract_wildcard.end()) {
                pool.compress_list.push_back(normal_component);
            }
            if (combine_wildcard.find(name) != extract_wildcard.end()) {
                pool.comb_list.push_back(parallel_component);
            }
        }
        pool.align_list.push_back(normal_component);
    }
    LOG(INFO) << "Print Component Pool";
    pool.print();
    return pool;
}