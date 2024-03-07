//
// Created by pro on 2021/12/18.
//

#include "istool/basic/env.h"
#include "istool/basic/program.h"
#include "glog/logging.h"
#include <sys/time.h>

Env::Env(): random_engine(0) {
    semantics::loadLogicSemantics(this);
    info_builder = new ExecuteInfoBuilder();
}

int Env::setRandomSeed(int seed) {
    random_engine.seed(seed);
    return seed;
}

Data * Env::getConstRef(const std::string &name, const Data& default_value) {
    //LOG(INFO) << "Get " << this << " " << name; int kk; std::cin >> kk;
    if (const_pool.find(name) == const_pool.end()) {
        const_pool[name] = new Data(default_value);
    }
    if (!default_value.isNull() && const_pool[name]->isNull()) {
        delete const_pool[name];
        const_pool[name] = new Data(default_value);
    }
    return const_pool[name];
}

void Env::setConst(const std::string &name, const Data &value) {
    //LOG(INFO) << "Set " << this << " " << name; int kk; std::cin >> kk;
    if (const_pool.find(name) == const_pool.end()) {
        const_pool[name] = new Data();
    }
    *(const_pool[name]) = value;
}

DataList * Env::getConstListRef(const std::string &name) {
    if (const_list_pool.find(name) == const_list_pool.end()) {
        const_list_pool[name] = new DataList();
    }
    return const_list_pool[name];
}

void Env::setConst(const std::string &name, const DataList &value) {
    if (const_list_pool.find(name) == const_list_pool.end()) {
        const_list_pool[name] = new DataList();
    }
    *(const_list_pool[name]) = value;
}

Env::~Env() {
    for (auto& const_info: const_pool) {
        delete const_info.second;
    }
    for (auto& ext_info: extension_pool) {
        delete ext_info.second;
    }
    delete info_builder;
}

Extension * Env::getExtension(const std::string &name) const {
    if (extension_pool.find(name) == extension_pool.end()) {
        return nullptr;
    }
    return extension_pool.find(name)->second;
}
void Env::registerExtension(const std::string &name, Extension *ext) {
    extension_pool[name] = ext;
}

void Env::setSemantics(const std::string &name, const PSemantics &semantics) {
    semantics_pool[name] = semantics;
}
PSemantics Env::getSemantics(const std::string &name) const {
    if (semantics_pool.find(name) == semantics_pool.end()) {
        LOG(FATAL) << "Unknown semantics " << name;
    }
    return semantics_pool.find(name)->second;
}

void Env::setExecuteInfoBuilder(ExecuteInfoBuilder *builder) {
    delete info_builder;
    info_builder = builder;
}

Data Env::run(Program *program, const DataList &param_list, const FunctionContext &ctx) {
    auto* info = info_builder->buildInfo(param_list, ctx);
    auto res = program->run(info);
    delete info; return res;
}

void env::setTimeSeed(Env* env) {
    timeval now; gettimeofday(&now, NULL);
    env->setRandomSeed(now.tv_usec);
}