//
// Created by pro on 2022/1/25.
//

#ifndef ISTOOL_DEEPCODER_TYPE_SYSTEM_H
#define ISTOOL_DEEPCODER_TYPE_SYSTEM_H

#include "istool/basic/type_system.h"

class DeepCoderTypeSystem: public TypeSystem {
public:
    DeepCoderTypeSystem(TypeExtension* ext);
    virtual PType intersect(const PType& x, const PType& y);
    virtual PType getType(Program* p);
    virtual ~DeepCoderTypeSystem() = default;
};

#endif //ISTOOL_DEEPCODER_TYPE_SYSTEM_H
