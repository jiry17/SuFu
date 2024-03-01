//
// Created by pro on 2022/7/1.
//

#ifndef ISTOOL_BIASED_BITVECTOR_SELECTOR_H
#define ISTOOL_BIASED_BITVECTOR_SELECTOR_H

#include "istool/selector/selector.h"
#include "istool/basic/example_space.h"
#include "istool/ext/z3/z3_verifier.h"

class BiasedBitVectorSelector: public Verifier {
public:
    int K;
    FiniteIOExampleSpace* example_space;
    IOExampleList io_example_list;
    std::vector<std::string> feature_list;
    std::unordered_map<std::string, int> time_stamp;
    int stamp = 0;
    BiasedBitVectorSelector(FiniteIOExampleSpace* _example_space, int _K = 2);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
};

class Z3BiasedBitVectorSelector: public Z3Verifier {
public:
    int K;
    std::vector<z3::expr> cons_list;
    std::vector<int> stamp_list;
    int stamp = 0;
    Z3BiasedBitVectorSelector(Z3ExampleSpace* example_space, int _K = 2);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
};

#endif //ISTOOL_BIASED_BITVECTOR_SELECTOR_H
