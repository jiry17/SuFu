//
// Created by pro on 2022/1/27.
//

#include "istool/selector/split/splitor.h"
#include "istool/sygus/theory/basic/clia/clia.h"

Splitor::Splitor(ExampleSpace *_example_space): example_space(_example_space) {
    KSplitTimeOut = selector::getSplitorTimeOut(example_space->env);
}

bool Splitor::getCounterExample(Program *p, const ProgramList &seed_list, Example *counter_example) {
    auto* tmp_guard = new TimeGuard(theory::clia::getIntValue(*KSplitTimeOut) / 1000.);
    auto res = getCounterExample(p, seed_list, counter_example, tmp_guard);
    delete tmp_guard;
    return res;
}

bool Splitor::getDistinguishExample(Program *x, Program *y, const ProgramList &seed_list, Example *counter_example) {
    auto* tmp_guard = new TimeGuard(theory::clia::getIntValue(*KSplitTimeOut) / 1000.);
    auto res = getDistinguishExample(x, y, seed_list, counter_example, tmp_guard);
    delete tmp_guard;
    return res;
}

namespace {
    const int KDefaultTimeLimit = 10000;
    const std::string KSplitorTimeLimitName = "Selector@TimeOut";
}

Data* selector::getSplitorTimeOut(Env *env) {
    auto* data = env->getConstRef(KSplitorTimeLimitName);
    if (data->isNull()) {
        env->setConst(KSplitorTimeLimitName, BuildData(Int, KDefaultTimeLimit));
    }
    return data;
}

void selector::setSplitorTimeOut(Env *env, double ti) {
    int ti_ms = int(ti * 1000);
    env->setConst(KSplitorTimeLimitName, BuildData(Int, ti_ms));
}

