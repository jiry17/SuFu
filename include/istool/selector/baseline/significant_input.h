//
// Created by pro on 2022/7/1.
//

#ifndef ISTOOL_SIGNIFICANT_INPUT_H
#define ISTOOL_SIGNIFICANT_INPUT_H

#include "istool/selector/selector.h"

class SignificantInputSelector: public Verifier {
    void loadPartition();
public:
    std::string result_cache;
    std::string benchmark_name;
    FiniteExampleSpace* example_space;
    std::vector<std::vector<int>> partition;
    int pos = -1;
    SignificantInputSelector(FiniteExampleSpace* example_space, const std::string& benchmark_name, const std::string& result_cache = "");
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~SignificantInputSelector() = default;
};

#endif //ISTOOL_SIGNIFICANT_INPUT_H
