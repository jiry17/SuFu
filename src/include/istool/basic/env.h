//
// Created by pro on 2021/12/18.
//

#ifndef ISTOOL_ENV_H
#define ISTOOL_ENV_H

#include "semantics.h"
#include <unordered_map>
#include <random>

class Extension {
public:
    virtual ~Extension() = default;
};

class Env {
    std::unordered_map<std::string, Data*> const_pool;
    std::unordered_map<std::string, DataList*> const_list_pool;
    std::unordered_map<std::string, Extension*> extension_pool;
    std::unordered_map<std::string, PSemantics> semantics_pool;
    ExecuteInfoBuilder* info_builder;
public:
    std::minstd_rand random_engine;
    Env();

    Data* getConstRef(const std::string& name, const Data& default_value = {});
    void setConst(const std::string& name, const Data& value);

    DataList* getConstListRef(const std::string& name);
    void setConst(const std::string& name, const DataList& value);

    void registerExtension(const std::string& name, Extension* ext);
    Extension* getExtension(const std::string& name) const;

    void setSemantics(const std::string& name, const PSemantics& semantics);
    PSemantics getSemantics(const std::string& name) const;

    void setExecuteInfoBuilder(ExecuteInfoBuilder* builder);
    Data run(Program* program, const DataList& param_list, const FunctionContext &ctx={});

    int setRandomSeed(int seed);
    ~Env();
};

typedef std::shared_ptr<Env> PEnv;

namespace env {
    void setTimeSeed(Env* env);
}

#endif //ISTOOL_ENV_H
