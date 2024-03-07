//
// Created by pro on 2022/1/18.
//

#include "istool/ext/deepcoder/tmp_info.h"

TmpExecuteInfo::TmpExecuteInfo(const DataList &param_value, const FunctionContext& ctx): ExecuteInfo(param_value, ctx) {}
void TmpExecuteInfo::clear(const std::string &name) {
    auto it = tmp_value_map.find(name);
    assert(it != tmp_value_map.end());
    tmp_value_map.erase(it);
}
Data TmpExecuteInfo::get(const std::string &name, bool is_strict) {
    auto it = tmp_value_map.find(name);
    if (it == tmp_value_map.end()) {
        if (is_strict) throw ExecutionNotDefinedError();
        return {};
    }
    return it->second;
}
void TmpExecuteInfo::set(const std::string &name, const Data &value) {
    auto it = tmp_value_map.find(name);
    assert(it == tmp_value_map.end());
    tmp_value_map[name] = value;
}

ExecuteInfo * TmpExecuteInfoBuilder::buildInfo(const DataList &_param_value, const FunctionContext &ctx) {
    return new TmpExecuteInfo(_param_value, ctx);
}

void ext::ho::registerTmpExecuteInfo(Env* env) {
    env->setExecuteInfoBuilder(new TmpExecuteInfoBuilder());
}