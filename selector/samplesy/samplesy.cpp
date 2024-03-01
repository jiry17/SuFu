//
// Created by pro on 2022/1/27.
//

#include "istool/selector/samplesy/samplesy.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"
#include "istool/basic/config.h"

namespace {
    int KDefaultTimeOut = 120;
    int KDefaultSampleNum = 100000;
}

const std::string selector::samplesy::KSampleTimeOutLimit = "SampleSy@TimeOut";
const std::string selector::samplesy::KSampleNumLimit = "SampleSy@SampleNum";

SampleSy::SampleSy(Specification* _spec, Splitor *_splitor, SeedGenerator* _gen, GrammarEquivalenceChecker* _checker):
    CompleteSelector(_spec, _checker), splitor(_splitor), gen(_gen) {
    auto* env = splitor->example_space->env;
    KSampleTimeOut = env->getConstRef(selector::samplesy::KSampleTimeOutLimit, BuildData(Int, KDefaultTimeOut * 1000));
    KSampleNum = env->getConstRef(selector::samplesy::KSampleNumLimit, BuildData(Int, KDefaultSampleNum));
}
Example SampleSy::getNextExample(const PProgram &x, const PProgram &y) {
    Example example;
    splitor->getDistinguishExample(x.get(), y.get(), samples, &example);
    return example;
}
FunctionContext SampleSy::synthesis(TimeGuard *guard) {
    int example_num = 0;
    double time_limit = theory::clia::getIntValue(*KSampleTimeOut) / 1000.;
    turn_guard = new TimeGuard(time_limit);
    while (1) {
        auto programs = checker->getTwoDifferentPrograms();
        if (programs.size() == 1) {
            return semantics::buildSingleContext(io_space->func_name, programs[0]);
        }
        assert(programs.size() == 2);
        LOG(INFO) << "program " << programs[0]->toString() << " " << programs[1]->toString();
        LOG(INFO) << "samples " << samples.size();
        global::recorder.start("verify");

        auto example = getNextExample(programs[0], programs[1]);
        global::recorder.end("verify");
        addExampleCount();
        // perform background sampling
        auto remain_time = turn_guard->getRemainTime();
        LOG(INFO) << "start sampling " << " " << remain_time <<  std::endl;
        samples = gen->getSeeds(theory::clia::getIntValue(*KSampleNum), std::max(0.0, remain_time));
        LOG(INFO) << "end sampling" << std::endl;

        // new turn
        delete turn_guard; turn_guard = new TimeGuard(time_limit);
        auto io_example = io_space->getIOExample(example);
        LOG(INFO) << "Add #" << ++example_num << " example " << example::ioExample2String(io_example);
        checker->addExample(io_example);
        addExample(io_example);
    }
}
void SampleSy::addExample(const IOExample &example) {
    gen->addExample(example);
    int now = 0;
    auto* env = splitor->example_space->env;
    for (auto& seed: samples) {
        if (env->run(seed.get(), example.first) == example.second) {
            samples[now++] = seed;
        }
    }
    samples.resize(now); example_list.push_back(example);

    for (auto& seed: samples) {
        for (auto& e: example_list) {
            assert(env->run(seed.get(), e.first) == e.second);
        }
    }
}
SampleSy::~SampleSy() {
    delete splitor; delete gen;
}