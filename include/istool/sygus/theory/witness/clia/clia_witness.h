//
// Created by pro on 2021/12/30.
//

#ifndef ISTOOL_CLIA_WITNESS_H
#define ISTOOL_CLIA_WITNESS_H

#include "istool/ext/vsa/witness.h"
#include "istool/basic/env.h"

#define DefineIntWitnessFunction(name) \
class name ## WitnessFunction: public WitnessFunction { \
public: \
    Data *int_min, *int_max; \
    name ## WitnessFunction(Data* _int_min, Data* _int_max): int_min(_int_min), int_max(_int_max) {} \
    virtual WitnessList witness(const WitnessData& oup); \
};

DefineIntWitnessFunction(IntPlus)
DefineIntWitnessFunction(IntMinus)
DefineIntWitnessFunction(IntTimes)
DefineIntWitnessFunction(IntMod)
DefineIntWitnessFunction(IntDiv)
DefineIntWitnessFunction(IntLq)
DefineIntWitnessFunction(IntLeq)
DefineIntWitnessFunction(IntGq)
DefineIntWitnessFunction(IntGeq)
// Todo: support polymorphism
DefineIntWitnessFunction(IntEq)
DefineIntWitnessFunction(IntNeq)
DefineWitnessFunction(Ite)
DefineWitnessFunction(BoolEq)

namespace theory {
    namespace clia {
        extern const std::string KWitnessIntMinName;
        extern const std::string KWitnessIntMaxName;
        void loadWitnessFunction(Env *env);
    }
}

#endif //ISTOOL_CLIA_WITNESS_H
