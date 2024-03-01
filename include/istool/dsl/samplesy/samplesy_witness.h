//
// Created by pro on 2022/2/1.
//

#ifndef ISTOOL_SAMPLESY_WITNESS_H
#define ISTOOL_SAMPLESY_WITNESS_H

#include "istool/ext/vsa/witness.h"

namespace samplesy {
    class StringReplaceAllWitnessFunction: public WitnessFunction {
    public:
        DataList *const_list, *inp_list;
        StringReplaceAllWitnessFunction(DataList* _const_list, DataList* _inp_list);
        virtual WitnessList witness(const WitnessData& oup);
        virtual ~StringReplaceAllWitnessFunction() = default;
    };

    class StringDeleteWitnessFunction: public WitnessFunction {
    public:
        DataList *const_list, *inp_list;
        StringDeleteWitnessFunction(DataList* _const_list, DataList* _inp_list);
        virtual WitnessList witness(const WitnessData& oup);
        virtual ~StringDeleteWitnessFunction() = default;
    };

    class StringAbsSubstrWitnessFunction: public WitnessFunction {
    public:
        DataList* inp_list;
        StringAbsSubstrWitnessFunction(DataList* _inp_list);
        virtual WitnessList witness(const WitnessData& oup);
        virtual ~StringAbsSubstrWitnessFunction() = default;
    };

    class StringIndexOfWitnessFunction: public WitnessFunction {
    public:
        DataList* inp_list;
        StringIndexOfWitnessFunction(DataList* _inp_list);
        virtual WitnessList witness(const WitnessData& oup);
        virtual ~StringIndexOfWitnessFunction() = default;
    };

    class IndexMoveWitnessFunction: public WitnessFunction {
    public:
        Data* int_max;
        IndexMoveWitnessFunction(Data* _int_max);
        virtual WitnessList witness(const WitnessData& oup);
        virtual ~IndexMoveWitnessFunction() = default;
    };
}

#endif //ISTOOL_SAMPLESY_WITNESS_H
