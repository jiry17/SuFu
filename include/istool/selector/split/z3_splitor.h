//
// Created by pro on 2022/1/12.
//

#ifndef ISTOOL_Z3_SPLITOR_H
#define ISTOOL_Z3_SPLITOR_H

#include "istool/ext/z3/z3_verifier.h"
#include "splitor.h"
#include <unordered_set>

class Z3Splitor: public Splitor {
protected:
    virtual bool getExample(z3::solver& s, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
    virtual bool getCounterExample(Program* p, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
    virtual bool getDistinguishExample(Program* x, Program* y, const ProgramList& seed_list, Example* counter_example, TimeGuard* guard);
public:
    Z3IOExampleSpace* io_space;
    z3::expr_vector param_list, inp_list;
    Z3Extension* ext;
    Env* env;
    TypeList inp_type_list;
    PType oup_type;
    std::unordered_set<std::string> split_set;
    Z3Splitor(ExampleSpace* _example_space, const PType& _oup_type, const TypeList& _inp_type_list);
    virtual ~Z3Splitor() = default;
};

#endif //ISTOOL_Z3_SPLITOR_H
