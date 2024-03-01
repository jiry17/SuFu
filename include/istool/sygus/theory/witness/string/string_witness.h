//
// Created by pro on 2021/12/31.
//

#ifndef ISTOOL_STRING_WITNESS_H
#define ISTOOL_STRING_WITNESS_H

#include "istool/ext/vsa/witness.h"
#include "istool/basic/env.h"

#define DefineStringWitnessFunction(name) \
class name ## WitnessFunction: public WitnessFunction { \
public: \
    DataList* const_list, *input_list; \
    name ## WitnessFunction(DataList* _const_list, DataList* _input_list): const_list(_const_list), input_list(_input_list) {} \
    virtual WitnessList witness(const WitnessData& oup); \
};

DefineWitnessFunction(StringCat)
DefineStringWitnessFunction(StringReplace)
DefineStringWitnessFunction(StringAt)

class IntToStrWitnessFunction: public WitnessFunction {
public:
    Data *int_min, *int_max;
    IntToStrWitnessFunction(Data* _int_min, Data* _int_max): int_min(_int_min), int_max(_int_max) {}
    virtual WitnessList witness(const WitnessData& oup);
};

class StringSubStrWitnessFunction: public WitnessFunction {
public:
    DataList* const_list, *input_list;
    Data* int_min, *int_max;
    StringSubStrWitnessFunction(DataList* _const_list, DataList* _input_list, Data* _int_min, Data* _int_max);
    virtual WitnessList witness(const WitnessData& oup);
};

DefineStringWitnessFunction(StringLen)

class StringIndexOfWitnessFunction: public WitnessFunction {
public:
    DataList* const_list, *input_list;
    Data* int_min, *int_max;
    StringIndexOfWitnessFunction(DataList* _const_list, DataList* _input_list, Data* _int_min, Data* _int_max);
    virtual WitnessList witness(const WitnessData& oup);
};

DefineStringWitnessFunction(StringPrefixOf)
DefineStringWitnessFunction(StringSuffixOf)
DefineStringWitnessFunction(StringContains)
DefineStringWitnessFunction(StrToInt)

namespace theory {
    namespace string {
        extern const std::string KStringConstList;
        extern const std::string KStringInputList;
        void loadWitnessFunction(Env *env);
    }
}

#endif //ISTOOL_STRING_WITNESS_H
