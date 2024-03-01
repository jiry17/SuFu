//
// Created by pro on 2022/1/12.
//

#ifndef ISTOOL_CLIA_RANDOM_SELECTOR_H
#define ISTOOL_CLIA_RANDOM_SELECTOR_H

#include "istool/ext/z3/z3_verifier.h"
#include "istool/selector/selector.h"

class CLIARandomSelector: public Selector {
public:
    int KRandomRange;
    Z3Extension* ext;
    Z3Verifier* v;
    CLIARandomSelector(Env* env, Z3IOExampleSpace* example_space);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~CLIARandomSelector();
};

namespace selector {
    extern const std::string KCLIARandomRangeName;
}

#endif //ISTOOL_CLIA_RANDOM_SELECTOR_H
