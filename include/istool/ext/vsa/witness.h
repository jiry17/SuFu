//
// Created by pro on 2021/12/29.
//

#ifndef ISTOOL_WITNESS_H
#define ISTOOL_WITNESS_H

#include "witness_value.h"

class WitnessFunction {
public:
    virtual WitnessList witness(const WitnessData& oup) = 0;
    virtual ~WitnessFunction() = default;
};

#define DefineWitnessFunction(name) \
class name ## WitnessFunction: public WitnessFunction { \
public: \
    virtual WitnessList witness(const WitnessData& oup); \
};

#define WitnessError(data) LOG(FATAL) << "Unsupported witness data " << data->toString()

DefineWitnessFunction(Not)
DefineWitnessFunction(And)
DefineWitnessFunction(Or)
DefineWitnessFunction(Imply)
DefineWitnessFunction(Direct)

class VSAExtension;
#define LoadWitness(ext, name, sem) ext->registerWitnessFunction(name, new sem ## WitnessFunction())

namespace ext {
    namespace vsa {
        void loadLogicWitness(VSAExtension* ext);
    }
}

#endif //ISTOOL_WITNESS_H
