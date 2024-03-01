//
// Created by pro on 2021/12/3.
//

#ifndef ISTOOL_TYPE_H
#define ISTOOL_TYPE_H

#include <memory>
#include <vector>
#include <unordered_map>
#include "z3++.h"

class Type;
typedef std::shared_ptr<Type> PType;
typedef std::vector<PType> TypeList;

class Type {
public:
    virtual std::string getName() = 0;
    virtual bool equal(Type* type) = 0;
    virtual std::string getBaseName() = 0;
    virtual TypeList getParams() = 0;
    virtual PType clone(const TypeList& params) = 0;
    virtual std::string getHaskellName() = 0;
    virtual int getTupleLen() = 0;
    virtual ~Type() = default;
};

class SimpleType: public Type {
public:
    virtual bool equal(Type* type);
    virtual std::string getBaseName();
    virtual TypeList getParams();
    virtual int getTupleLen();
    virtual ~SimpleType() = default;
};

class TBot: public SimpleType {
public:
    virtual std::string getName();
    virtual PType clone(const TypeList& params);
    virtual std::string getHaskellName();
    virtual ~TBot() = default;
};

class TVar: public SimpleType {
public:
    std::string name;
    TVar(const std::string& _name);
    virtual std::string getName();
    virtual PType clone(const TypeList& params);
    virtual std::string getHaskellName();
    virtual ~TVar() = default;
};

class TBool: public SimpleType {
public:
    virtual std::string getName();
    virtual PType clone(const TypeList& params);
    virtual std::string getHaskellName();
    virtual ~TBool() = default;
};

typedef std::pair<TypeList, PType> Signature;

namespace type {
    std::string typeList2String(const TypeList& type_list);
    bool equal(const PType& t1, const PType& t2);
    bool equal(Type* t1, Type* t2);
    TypeList assignVarName(const TypeList& type_list);
    PType substituteVar(const PType& type, const std::unordered_map<std::string, PType>& type_map);
    PType getTBool();
    PType getTVarA();
}

#endif //ISTOOL_TYPE_H
