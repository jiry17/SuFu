//
// Created by pro on 2022/1/20.
//

#ifndef ISTOOL_OCCAM_VERIFIER_H
#define ISTOOL_OCCAM_VERIFIER_H

#include "istool/basic/verifier.h"
#include "streamed_example_space.h"

class OccamVerifier: public Verifier {
public:
    int pos;
    Data* example_num;
    StreamedExampleSpace* example_space;
    OccamVerifier(StreamedExampleSpace* _example_space);
    virtual bool verify(const FunctionContext& info, Example* counter_example);
    virtual ~OccamVerifier() = default;
};

namespace solver::autolifter {
    extern const std::string KOccamExampleNumName;
}

#endif //ISTOOL_OCCAM_VERIFIER_H
