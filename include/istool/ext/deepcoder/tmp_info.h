//
// Created by pro on 2022/1/18.
//

#ifndef ISTOOL_TMP_INFO_H
#define ISTOOL_TMP_INFO_H

#include "istool/basic/execute_info.h"
#include "istool/basic/env.h"

class TmpExecuteInfo: public ExecuteInfo {
public:
    std::unordered_map<std::string, Data> tmp_value_map;
    TmpExecuteInfo(const DataList& param_value, const FunctionContext& ctx);
    void set(const std::string& name, const Data& value);
    void clear(const std::string& name);
    Data get(const std::string& name, bool is_strict);
};

class TmpExecuteInfoBuilder: public ExecuteInfoBuilder {
public:
    virtual ExecuteInfo* buildInfo(const DataList& _param_value, const FunctionContext& ctx);
    virtual ~TmpExecuteInfoBuilder() = default;
};

namespace ext::ho {
    void registerTmpExecuteInfo(Env* env);
}

#endif //ISTOOL_TMP_INFO_H
