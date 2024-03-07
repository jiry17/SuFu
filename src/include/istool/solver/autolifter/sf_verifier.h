//
// Created by pro on 2022/1/21.
//

#ifndef ISTOOL_SF_VERIFIER_H
#define ISTOOL_SF_VERIFIER_H

#include "basic/occam_verifier.h"
#include "istool/solver/autolifter/basic/lifting_problem.h"

class SFVerifier: public Verifier {
    int size_limit, example_pos;
public:
    PartialLiftingTask* task;
    Data* example_num;
    ProgramList p_list, h_list;
    std::vector<DataList*> p_cache_list, h_cache_list;
    std::pair<int, int> verify(const PProgram& f);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    ~SFVerifier() = default;
    SFVerifier(PartialLiftingTask* _task);
};



#endif //ISTOOL_SF_VERIFIER_H
