//
// Created by pro on 2021/12/4.
//

#ifndef ISTOOL_TYPE_SYSTEM_H
#define ISTOOL_TYPE_SYSTEM_H

#include "type.h"
#include "value.h"
#include "program.h"
#include "env.h"
#include <functional>

class TypeExtension;

struct TypeError: public std::exception {
};

class ValueTypeInfo {
public:
    virtual bool isMatch(Value* value) = 0;
    virtual PType getType(Value* value) = 0;
    virtual ~ValueTypeInfo() = default;
};

class BasicValueTypeInfo: public ValueTypeInfo {
    PType bot_type, bool_type;
public:
    BasicValueTypeInfo();
    virtual bool isMatch(Value* value);
    virtual PType getType(Value* value);
    virtual ~BasicValueTypeInfo() = default;
};

class TypeSystem {
public:
    TypeExtension* ext;
    TypeSystem(TypeExtension* _ext);
    virtual PType intersect(const PType& x, const PType& y) = 0;
    virtual PType getType(Program* p) = 0;
    virtual ~TypeSystem() = default;
};

class BasicTypeSystem: public TypeSystem {
public:
    BasicTypeSystem(TypeExtension* _ext);
    virtual PType intersect(const PType& x, const PType& y);
    virtual PType getType(Program* p);
    virtual ~BasicTypeSystem() = default;
};

class TypeExtension: public Extension {
public:
    ValueTypeInfo* getTypeInfo(Value* value);

    std::vector<ValueTypeInfo*> type_info_list;
    TypeSystem* type_system;
    TypeExtension();
    void registerTypeSystem(TypeSystem* _type_system);
    void registerTypeInfo(ValueTypeInfo* info);
    PType join(const PType& x, const PType& y);
    PType getType(Program* p);
    PType getType(Value* value);
    ~TypeExtension();
};

namespace type {
    TypeExtension* getTypeExtension(Env* env);
    void registerTypeSystem(TypeSystem* type_system, TypeExtension* ext);
}

#endif //ISTOOL_TYPE_SYSTEM_H
