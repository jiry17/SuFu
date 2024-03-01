//
// Created by pro on 2021/12/30.
//

#include "istool/solver/vsa/vsa_solver.h"
#include "glog/logging.h"

VSARandomProgramSelector::VSARandomProgramSelector(Env *_env): env(_env) {}
PProgram VSARandomProgramSelector::select(VSANode *node) {
    std::uniform_int_distribution<int> d(0, int(node->edge_list.size()) - 1);
    int id = d(env->random_engine);
    ProgramList sub_list;
    for (auto* sub_node: node->edge_list[id].node_list) {
        sub_list.push_back(select(sub_node));
    }
    return std::make_shared<Program>(node->edge_list[id].semantics, sub_list);
}

VSAMinimalProgramSelector::VSAMinimalProgramSelector(TopDownModel *_model): model(_model) {}
PProgram VSAMinimalProgramSelector::select(VSANode *node) {
    return ext::vsa::getBestProgram(node, model);
}
VSAMinimalProgramSelector::~VSAMinimalProgramSelector() {
    delete model;
}

BasicVSASolver::BasicVSASolver(Specification *spec, const PVSABuilder &_builder, VSAProgramSelector *_selector):
    PBESolver(spec), builder(_builder), selector(_selector) {
    io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "VSA solver supports only IO examples";
    }
}
BasicVSASolver::~BasicVSASolver() {
    delete selector;
}

VSANode* BasicVSASolver::buildVSA(const ExampleList &example_list, TimeGuard *guard) {
    LOG(INFO) << "BuildVSA";
    for (const auto& example: example_list) LOG(INFO) << "  " << data::dataList2String(example) << std::endl;
    int pos = -1; VSANode* res = nullptr;
    std::string feature;
    for (int i = 0; i < example_list.size(); ++i) {
        feature += "@" + data::dataList2String(example_list[i]);
        if (cache.find(feature) != cache.end()) {
            pos = i; res = cache[feature];
        }
    }
    if (pos == -1) {
        if (example_list.empty()) {
            return cache[feature] = builder->buildFullVSA();
        }
        auto io_example = io_space->getIOExample(example_list[0]);
        res = builder->buildVSA(io_example.second, io_example.first, guard);
        feature = "@" + data::dataList2String(example_list[0]);
        return cache[feature] = res;
    }
    if (pos + 1 == example_list.size()) return res;
    ExampleList remain_examples;
    for (int i = pos + 1; i < example_list.size(); ++i) {
        remain_examples.push_back(example_list[i]);
    }
    auto remain_res = buildVSA(remain_examples, guard);
    res = builder->mergeVSA(res, remain_res, guard);
    LOG(INFO) << "Program num " << ext::vsa::getProgramNum(res);
    return cache[feature] = res;
}

FunctionContext BasicVSASolver::synthesis(const std::vector<Example> &example_list, TimeGuard *guard) {
    auto root = buildVSA(example_list, guard);
    FunctionContext res;
    res[io_space->func_name] = selector->select(root);
    return res;
}