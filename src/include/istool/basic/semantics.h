//
// Created by pro on 2021/12/3.
//

#ifndef ISTOOL_SEMANTICS_H
#define ISTOOL_SEMANTICS_H

#include <exception>
#include <unordered_map>
#include "data.h"
#include "execute_info.h"
#include "z3++.h"
#include "type.h"

class Program;
class Env;

struct SemanticsError: public std::exception {
};
struct ExecutionNotDefinedError: public std::exception {
};

class Semantics {
public:
    std::string name;
    Semantics(const std::string& _name);
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo* info) = 0;
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    virtual std::string buildProgramStringToHaskell(const std::vector<std::string>& sub_exp);
    virtual std::string getName();
    virtual ~Semantics() = default;
};

typedef std::shared_ptr<Semantics> PSemantics;

class TypedSemantics {
public:
    PType oup_type;
    TypeList inp_type_list;
    TypedSemantics(const PType& _oup_type, const TypeList& _inp_list): oup_type(_oup_type), inp_type_list(_inp_list) {}
};

class FullExecutedSemantics: public Semantics {
public:
    FullExecutedSemantics(const std::string& name);
    Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo* info);
    virtual Data run(DataList&& inp_list, ExecuteInfo* info) = 0;
    virtual ~FullExecutedSemantics() = default;
};

class NormalSemantics: public FullExecutedSemantics, public TypedSemantics {
public:
    NormalSemantics(const std::string& name, const PType& _oup_type, const TypeList& _inp_list);
};

class ParamSemantics: public NormalSemantics {
public:
    int id;
    ParamSemantics(const PType& type, int _id);
    virtual Data run(DataList&&, ExecuteInfo* info);
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    ~ParamSemantics() = default;
};

class ConstSemantics: public FullExecutedSemantics {
public:
    Data w;
    ConstSemantics(const Data& _w);
    ConstSemantics(const Data& _w, const std::string& _name);
    virtual Data run(DataList&&, ExecuteInfo* info);
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    ~ConstSemantics() = default;
};

class DirectSemantics: public NormalSemantics {
public:
    DirectSemantics();
    virtual Data run(DataList&&, ExecuteInfo* info);
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    ~DirectSemantics() = default;
};

class InvokeSemantics: public FullExecutedSemantics {
public:
    Env* env;
    InvokeSemantics(const std::string& _func_name, Env* _env);
    virtual Data run(DataList&&, ExecuteInfo* info);
    virtual ~InvokeSemantics() = default;
};

class TypedInvokeSemantics: public InvokeSemantics, public TypedSemantics {
public:
    TypedInvokeSemantics(const std::string& _func_name, const PType& oup_type, const TypeList& inp_list, Env* _env);
    virtual ~TypedInvokeSemantics() = default;
};

#define DefineNormalSemantics(name) \
class name ## Semantics : public NormalSemantics { \
public: \
    name ## Semantics(); \
    virtual Data run(DataList &&inp_list, ExecuteInfo *info); \
    ~name ## Semantics() = default; \
};

// basic logic semantics
DefineNormalSemantics(Not)

class AndSemantics : public NormalSemantics {
public:
    AndSemantics();
    virtual Data run(DataList &&inp_list, ExecuteInfo* info);
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo *info);
    ~AndSemantics() = default;
};

class OrSemantics : public NormalSemantics {
public:
    OrSemantics();
    virtual Data run(DataList &&inp_list, ExecuteInfo* info);
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo *info);
    ~OrSemantics() = default;
};

class ImplySemantics: public NormalSemantics {
public:
    ImplySemantics();
    virtual Data run(DataList &&inp_list, ExecuteInfo* info);
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo *info);
    ~ImplySemantics() = default;
};

class AllowFailSemantics: public Semantics, public TypedSemantics {
    Data d;
public:
    AllowFailSemantics(const PType& type, const Data& _d);
    virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo* info);
};

#define LoadSemantics(name, sem) env->setSemantics(name, std::make_shared<sem ## Semantics>())

namespace semantics {
    void loadLogicSemantics(Env* env);
    PSemantics buildParamSemantics(int id, const PType& type = nullptr);
    PSemantics buildConstSemantics(const Data& w);
    FunctionContext buildSingleContext(const std::string& name, const std::shared_ptr<Program>& program);
}


#endif //ISTOOL_SEMANTICS_H
