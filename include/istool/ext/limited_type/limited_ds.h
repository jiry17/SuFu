//
// Created by pro on 2022/2/22.
//

#ifndef ISTOOL_LIMITED_DS_H
#define ISTOOL_LIMITED_DS_H

#include "limited_type.h"
#include "istool/ext/deepcoder/data_type.h"

class LimitedTList: public LimitedType, public TList {
public:
    int max_size;
    LimitedTList(int _max_size, const PType& content);
    virtual std::string getName();
    virtual PType getBaseType(const TypeList& params);
    virtual PType clone(const TypeList& type_list);
    virtual bool isValid(Value* value);
    virtual ~LimitedTList() = default;
};

#endif //ISTOOL_LIMITED_DS_H
