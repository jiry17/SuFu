//
// Created by pro on 2022/1/20.
//

#include "istool/solver/autolifter/basic/streamed_example_space.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "glog/logging.h"

namespace {
    int KDefaultTimeOut = 100;
}

StreamedExampleSpace::StreamedExampleSpace(const PProgram &cons_program, const PExampleGenerator& _g, Env *env):
    ExampleSpace(cons_program, env), g(_g) {
    generate_timeout = env->getConstRef(example::KExampleGenerateTimeOutName, BuildData(Int, KDefaultTimeOut));
}

bool StreamedExampleSpace::generateExample(TimeGuard* guard) {
    double timeout = theory::clia::getIntValue(*generate_timeout) / 1000.0;
    if (guard) timeout = std::min(timeout, guard->getRemainTime());
    auto* tmp_guard = new TimeGuard(timeout);
    try {
        auto res = g->generateExamples(tmp_guard);
        for (const auto& example: res) example_list.push_back(example);
    } catch (TimeOutError& e) {
        delete tmp_guard;
        return false;
    }
    return true;
}

int StreamedExampleSpace::extendExampleList(int target, TimeGuard* guard) {
    while (example_list.size() < target && generateExample(guard));
    return std::min(target, int(example_list.size()));
}

Example StreamedExampleSpace::getExample(int k) {
    if (k < 0 || k >= example_list.size()) {
        LOG(FATAL) << "Access StreamedExampleSpace of size " << example_list.size() << " with invalid index " << k;
    }
    return example_list[k];
}

StreamedIOExampleSpace::StreamedIOExampleSpace(const PProgram& cons_program, const std::string &func_name,
        const ProgramList &_inp_list, const PProgram &_oup, const PExampleGenerator &_g, Env *_env):
        IOExampleSpace(func_name), StreamedExampleSpace(cons_program, _g, _env), inp_list(_inp_list), oup(_oup) {
}
IOExample StreamedIOExampleSpace::getIOExample(const Example &example) {
    DataList inp;
    for (const auto& inp_program: inp_list) inp.push_back(env->run(inp_program.get(), example));
    return {inp, env->run(oup.get(), example)};
}
bool StreamedIOExampleSpace::satisfyExample(const FunctionContext &info, const Example &example) {
    auto it = info.find(func_name);
    if (it == info.end()) {
        LOG(FATAL) << "Cannot find target program " << func_name << " from result " << info.toString();
    }
    auto res = it->second;
    auto io_example = getIOExample(example);
    return example::satisfyIOExample(res.get(), io_example, env);
}

const std::string example::KExampleGenerateTimeOutName = "Streamed@ExampleGenerateTimeOut";