//
// Created by pro on 2023/4/11.
//

#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/incre/trans/incre_trans.h"
#include "glog/logging.h"

using namespace incre::grammar;

ComponentPool collector::collectComponentFromLabel(EnvContext *env_ctx, TypeContext* type_ctx, ProgramData *program) {
    ComponentPool res;

    for (int command_id = 0; command_id < program->commands.size(); ++command_id) {
        auto& command = program->commands[command_id];
        auto* cb = dynamic_cast<CommandBind*>(command.get());
        if (!cb) continue;
        auto* tb = dynamic_cast<TermBinding*>(cb->binding.get());
        if (!tb) continue;
        auto term = std::make_shared<TmVar>(cb->name);
        auto type = type_ctx->lookup(cb->name);
        auto full_type = incre::unfoldBasicType(type, type_ctx);

        auto val = incre::envRun(term, env_ctx->start, env_ctx->holder);

        auto normal_component = std::make_shared<IncreComponent>(cb->name, incre::typeFromIncre(full_type),
                                                          val, term, command_id, !command->isDecoratedWith(CommandDecorate::SYN_NO_PARTIAL), false);
        auto parallel_component = std::make_shared<IncreComponent>(cb->name, incre::typeFromIncre(full_type),
                                                                   val, term, command_id, !command->isDecoratedWith(CommandDecorate::SYN_NO_PARTIAL), true);
        if (command->isDecoratedWith(CommandDecorate::SYN_COMPRESS)) {
            res.compress_list.push_back(normal_component);
        }
        if (command->isDecoratedWith(CommandDecorate::SYN_COMBINE)) {
            res.comb_list.push_back(parallel_component);
        }
        if (command->isDecoratedWith(CommandDecorate::SYN_ALIGN)) {
            res.align_list.push_back(normal_component);
        }
    }
    return res;
}