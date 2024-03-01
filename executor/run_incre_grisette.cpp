//
// Created by zyw on 2023/5/19.
//

#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/basic/config.h"
#include "istool/incre/io/incre_from_json.h"
#include "istool/incre/io/incre_printer.h"
#include "istool/incre/io/incre_to_haskell.h"
#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "istool/incre/analysis/incre_instru_info.h"
#include "istool/incre/autolifter/incre_autolifter_solver.h"
#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include <iostream>
#include "glog/logging.h"

using namespace incre;

int main(int argv, char** argc) {
    std::string path, label_path, result_path, haskell_path;
    if (argv <= 1) {
        std::string name = "synduce/constraints/bst/sum_between";
        path = config::KSourcePath + "incre-tests/" + name + ".f";
        // path for label result
        label_path = config::KSourcePath + "tests/incre/label-res/" + name + ".f";
        // path for synthesis result
        result_path = config::KSourcePath + "tests/incre/optimize-res/" + name + ".f";
        // path for file of haskell
        haskell_path = config::KSourcePath + "tests/incre/haskell-res/" + name + ".hs";
    } else {
        path = std::string(argc[1]);
        label_path = std::string(argc[2]);
        result_path = std::string(argc[3]);
        haskell_path = std::string(argc[4]);
    }
    // final_type_list: type to replace compress
    // Bool, Int, Int, Bool
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyBool>()})};
    // Bool, Int, Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // Bool, Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyInt>()})};
    // 2-(Bool, Int)
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyInt>()}), std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyInt>()})};
    // (4-Int, 1-Int)
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()}), std::make_shared<TyInt>()};
    // 6-Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 5-Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 4-Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 3-Int
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 2-Int
    TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 2-(2-Int)
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>()}), std::make_shared<TyTuple>((TyList){std::make_shared<TyInt>(), std::make_shared<TyInt>()})};
    // 3-(Int)
    // TyList final_type_list = {std::make_shared<TyInt>(), std::make_shared<TyInt>(), std::make_shared<TyInt>()};
    // 2-(Int)
    // TyList final_type_list = {std::make_shared<TyInt>(), std::make_shared<TyInt>()};
    // 1-Int
    // TyList final_type_list = {std::make_shared<TyInt>()};
    // 1-Unit
    // TyList final_type_list = {std::make_shared<TyUnit>()};
    // 2-(Bool)
    // TyList final_type_list = {std::make_shared<TyBool>(), std::make_shared<TyBool>()};
    // 1-Bool
    // TyList final_type_list = {std::make_shared<TyBool>()};
    // 3-Bool
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyBool>(), std::make_shared<TyBool>()})};
    // 4-Bool
    // TyList final_type_list = {std::make_shared<TyTuple>((TyList){std::make_shared<TyBool>(), std::make_shared<TyBool>(), std::make_shared<TyBool>(), std::make_shared<TyBool>()})};
    // 4-(Unit), 1-(Int)
    // TyList final_type_list = {std::make_shared<TyUnit>(), std::make_shared<TyUnit>(), std::make_shared<TyUnit>(), std::make_shared<TyUnit>(), std::make_shared<TyInt>()};
    // number of io examples
    int io_pair_num = 20;
    // The time limit is in minutes
    int time_limit = 10;

    TimeGuard* global_guard = new TimeGuard(1e9);

    auto env = std::make_shared<Env>();
    incre::prepareEnv(env.get());

    // input
    auto init_program = incre::parseFromF(path, true);
    init_program = incre::removeGlobal(init_program.get());
    incre::printProgram(init_program, label_path);

    // label
    global::recorder.start("label");
    auto* label_solver = new autolabel::AutoLabelZ3Solver(init_program);
    auto res = label_solver->label();
    global::recorder.end("label");
    incre::applyConfig(res.get(), env.get());

    res = incre::eliminateNestedAlign(res.get());
    // incre::printProgram(res, label_path);
    incre::printProgram(res);

    env->setConst(incre::grammar::collector::KCollectMethodName, BuildData(Int, incre::grammar::ComponentCollectorType::SOURCE));
    env->setConst(theory::clia::KINFName, BuildData(Int, 50000));

    auto* info = incre::buildIncreInfo(res, env.get());
    // set context in example_pool
    for (int i = 1; i <= 10; ++i) {
        info->example_pool->generateSingleExample();
    }

    // get result from AutoLabel
    auto* solver = new incre::IncreAutoLifterSolver(info, env);
    /*
    auto solution = solver->solve();
    std::cout << "zyw: solution.print() begin!" << std::endl;
    solution.print();
    std::cout << "zyw: solution.print() end!" << std::endl;
    // LOG(INFO) << "After execute time " << global::recorder.query("execute");

    auto full_res = incre::rewriteWithIncreSolution(info->program.get(), solution, env.get());
    full_res = incre::eliminateUnusedLet(full_res.get());

    incre::printProgram(full_res, result_path);
    incre::printProgram(full_res);
    */

    // get io pairs
    std::cout << "zyw: begin iopairs" << std::endl;
    std::vector<std::pair<Term, Data>> io_pairs;
    // while(io_pairs.size() <= io_pair_num){
    for (int i = 0; i < 100000; ++i) {
        std::pair<Term, std::unordered_map<std::string, Data>> example = info->example_pool->generateStart();
        info->example_pool->ctx->initGlobal(example.second);
        auto* start = info->example_pool->ctx->start;
        auto* holder = info->example_pool->ctx->holder;
        auto result = incre::envRun(example.first, start, holder);
        std::cout << result.value->toHaskell() << std::endl;
        if (example.first->toString().length() >= 10 && result.value->toHaskell() != "(0)") {
            io_pairs.push_back({example.first, result});
        }
    }
    std::sort(io_pairs.begin(), io_pairs.end(), compareInput);
    io_pairs.resize(io_pair_num);

    // get component pool
    std::cout << "zyw: print component_pool begin!" << std::endl;
    auto component_pool = info->component_pool;
    component_pool.print();
    std::cout << "zyw: print component_pool end!" << std::endl;

    // get align info
    std::vector<std::pair<std::vector<std::pair<std::string, Ty>>, Ty>> align_info_for_haskell;
    for (auto& align_info: info->align_infos) {
        std::cout << "zyw: align_info->print() begin!" << std::endl;
        align_info->print();
        align_info_for_haskell.push_back({align_info->inp_types, align_info->oup_type});
        std::cout << "zyw: align_info->print() end!" << std::endl;
    }

    // auto* solver = new incre::IncreAutoLifterSolver(info, env);

    // final output
    incre::programToHaskell(res, io_pairs, info, solver, haskell_path, final_type_list, time_limit);

    global::recorder.printAll();
    std::cout << global_guard->getPeriod() << std::endl;
    std::cout << "Success" << std::endl;
    
    return 0;
}