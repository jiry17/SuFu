//
// Created by pro on 2022/9/18.
//


#include "istool/basic/config.h"
#include "istool/incre/io/incre_from_json.h"
#include "istool/incre/io/incre_printer.h"
#include "istool/solver/polygen/lia_solver.h"
#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/incre/autolifter/incre_autolifter_solver.h"
#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/string_value.h"
#include <iostream>
#include <glog/logging.h>
#include <gflags/gflags.h>

using namespace incre;

DEFINE_string(benchmark, "/home/jiry/2023A/ISTool/incre-tests/synduce/constraints/sorted_and_indexed/count_lt0.f", "The absolute path of the benchmark file (.sl)");
DEFINE_string(output, "", "The absolute path of the output file");
DEFINE_bool(is_gurobi, false, "Is use gurobi to synthesize sketch holes.");
DEFINE_string(stage_output_file, "", "Only used in online demo");

int main(int argc, char** argv) {
    gflags::ParseCommandLineFlags(&argc, &argv, true);

    std::string path = FLAGS_benchmark, target = FLAGS_output;
    bool is_gurobi = FLAGS_is_gurobi;
    global::KStageInfoPath = FLAGS_stage_output_file;

    TimeGuard* global_guard = new TimeGuard(1e9);

    auto env = std::make_shared<Env>();
    incre::prepareEnv(env.get());
    env->setConst(solver::lia::KIsGurobiName, BuildData(Bool, is_gurobi));

    auto input_program = incre::parseFromF(path, true);
    // init_program = incre::removeGlobal(init_program.get());

    IncreProgram labeled_program;

    global::printStageResult("Start stage 0/2: generating annotations.");
    global::recorder.start("label");
    auto *label_solver = new autolabel::AutoLabelZ3Solver(input_program, true);
    labeled_program = label_solver->label();
    global::recorder.end("label");
    global::printStageResult("Stage 0/2 Finished");
    incre::applyConfig(labeled_program.get(), env.get());

    labeled_program = incre::eliminateNestedAlign(labeled_program.get());
    incre::printProgram(labeled_program);
    env->setConst(incre::grammar::collector::KCollectMethodName, BuildData(Int, incre::grammar::ComponentCollectorType::SOURCE));
    env->setConst(theory::clia::KINFName, BuildData(Int, 50000));

    auto* info = incre::buildIncreInfo(labeled_program, env.get());

    for (auto& align_info: info->align_infos) {
        align_info->print();
        if (align_info->getId() >= info->example_pool->example_pool.size()) continue;
        auto& example_list = info->example_pool->example_pool[align_info->getId()];
        for (int i = 0; i < 10 && i < example_list.size(); ++i) {
            std::cout << "  " << example_list[i]->toString() << std::endl;
        }
    }

    IncreSolution solution;

    auto* solver = new incre::IncreAutoLifterSolver(info, env);
    solution = solver->solve();
    solution.print();
    // LOG(INFO) << "After execute time " << global::recorder.query("execute");

    auto full_res = incre::rewriteWithIncreSolution(info->program.get(), solution, env.get(), false);
    full_res = incre::eliminateUnusedLet(full_res.get());

    /* Final verification */
    global::recorder.start("final-verify");
    if (full_res->config_map.find(IncreConfig::EXTRA_GRAMMAR) != full_res->config_map.end()) {
        auto name_data = full_res->config_map[IncreConfig::EXTRA_GRAMMAR];
        auto *sv = dynamic_cast<StringValue *>(name_data.get());
        CommandList extra_component;
        if (sv) {
            extra_component = incre::grammar::collector::extractExtraComponentInResult(sv->s);
        }
        CommandList pre_commands;
        for (auto &command: full_res->commands) {
            if (command->isDecoratedWith(CommandDecorate::INPUT)) {
                extra_component.push_back(command);
            } else {
                pre_commands.push_back(command);
            }
        }
        full_res->commands.clear();
        bool is_inserted = false;
        for (auto &command: pre_commands) {
            if (command->getType() != CommandType::DEF_IND && !is_inserted) {
                is_inserted = true;
                for (auto &new_command: extra_component) full_res->commands.push_back(new_command);
            }
            full_res->commands.push_back(command);
        }
        if (!is_inserted) {
            for (auto &new_command: extra_component) {
                full_res->commands.push_back(new_command);
            }
        }
    }

    if (!target.empty()) incre::printProgram(full_res, target, false);
    incre::printProgram(full_res, {}, false);
    auto [start_name, params] = info->example_pool->start_list[0];
    auto possible_inputs = incre::constructAllPossibleInput(info->example_pool->input_list, params, start_name, input_program->config_map);

    LOG(INFO) << "Possible inputs " << possible_inputs.size();
    for (int i = 0; i < 10 && i < possible_inputs.size(); ++i) {
        std::cout << "#" << i << ": " << possible_inputs[i].first->toString() << std::endl;
        for (auto& [name, v]: possible_inputs[i].second) std::cout << " " << name << "@" << v.toString();
        std::cout << std::endl;
    }
    auto ref_result = evaluateAll(possible_inputs, input_program);
    auto total_result = evaluateAll(possible_inputs, full_res);
    for (int i = 0; i < 10 && i < possible_inputs.size(); ++i) {
        std::cout << "#" << i << ": " << ref_result[i].toString() << " " << total_result[i].toString() << std::endl;
    }
    for (int i = 0; i < ref_result.size(); ++i) {
        if (!(ref_result[i] == total_result[i])) {
            std::cout << "incorrect" << std::endl;
            std::cout << "#" << i << ": " << possible_inputs[i].first->toString() << std::endl;
            for (auto& [name, v]: possible_inputs[i].second) std::cout << " " << name << "@" << v.toString();
            std::cout << ref_result[i].toString() << " " << total_result[i].toString() << std::endl;
        }
        assert(ref_result[i] == total_result[i]);
    }
    global::recorder.end("final-verify");


    global::recorder.printAll();
    std::cout << global_guard->getPeriod() << std::endl;
    std::cout << "Success" << std::endl;
}