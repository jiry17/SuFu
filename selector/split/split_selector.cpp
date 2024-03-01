//
// Created by pro on 2022/1/13.
//

#include "istool/selector/split/split_selector.h"
#include "istool/solver/enum/enum_util.h"
#include "glog/logging.h"

SplitSelector::SplitSelector(Splitor* _splitor, const PSynthInfo &info, int n, Env* env, Verifier* v, Optimizer* o): splitor(_splitor) {
    std::vector<FunctionContext> info_list;
    auto* tmp_guard = new TimeGuard(0.1);
    solver::collectAccordingNum({info}, n, info_list, EnumConfig(v, o, tmp_guard));
    for (const auto& prog: info_list) {
        seed_list.push_back(prog.begin()->second);
    }
    auto invoke = std::make_shared<InvokeSemantics>(info->name, env);

    /*LOG(INFO) << "Seed list";
    for (const auto& seed: seed_list) {
        std::cout << "  " << seed->toString() << std::endl;
    }*/

    auto* example_space = dynamic_cast<IOExampleSpace*>(splitor->example_space);
    if (!example_space) {
        LOG(FATAL) << "SplitSelector supports only IOExampleSpace";
    }
    delete tmp_guard;
}

bool SplitSelector::verify(const FunctionContext &info, Example *counter_example) {
    auto res = splitor->getCounterExample(info.begin()->second.get(), seed_list, counter_example);
    if (!res && counter_example) {
        addExampleCount();
    }
    return res;
}

SplitSelector::~SplitSelector() {
    delete splitor;
}

CompleteSplitSelector::CompleteSplitSelector(Specification *_spec, Splitor *_splitor, GrammarEquivalenceChecker *_checker, int n, Verifier* v, Optimizer* o):
    CompleteSelector(_spec, _checker), splitor(_splitor) {
    std::vector<FunctionContext> info_list;
    auto* tmp_guard = new TimeGuard(0.1);
    solver::collectAccordingNum(spec->info_list, n, info_list, EnumConfig(v, o, tmp_guard));
    for (const auto& prog: info_list) {
        seed_list.push_back(prog.begin()->second);
    }
    delete tmp_guard;
}

Example CompleteSplitSelector::getNextExample(const PProgram &x, const PProgram &y) {
    Example example;
    assert(!splitor->getDistinguishExample(x.get(), y.get(), seed_list, &example));
    return example;
}

void CompleteSplitSelector::addExample(const IOExample &example) {
    checker->addExample(example);
}

CompleteSplitSelector::~CompleteSplitSelector() {
    delete checker; delete splitor;
}