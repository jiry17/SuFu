//
// Created by pro on 2022/1/14.
//

#ifndef ISTOOL_FINITE_SPLITOR_H
#define ISTOOL_FINITE_SPLITOR_H

#include "splitor.h"
#include "istool/basic/verifier.h"
#include <unordered_set>

class FiniteSplitor: public Splitor {
protected:
    virtual bool getExample(const std::function<bool(const IOExample&)>& filter, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
    virtual bool getCounterExample(Program* p, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
    virtual bool getDistinguishExample(Program* x, Program* y, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
    int getCost(const DataList& inp, const ProgramList& seed_list);
public:
    IOExampleList example_list;
    FiniteIOExampleSpace* io_space;
    Env* env;
    std::unordered_set<std::string> feature_cache;
    FiniteSplitor(ExampleSpace* _example_space);
    virtual ~FiniteSplitor() = default;
};

#endif //ISTOOL_FINITE_SPLITOR_H
