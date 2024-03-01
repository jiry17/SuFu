//
// Created by pro on 2022/1/18.
//

#ifndef ISTOOL_EXECUTE_INFO_H
#define ISTOOL_EXECUTE_INFO_H

#include "data.h"

class Program;

class FunctionContext: public std::unordered_map<std::string, std::shared_ptr<Program>> {
public:
    std::string toString() const;
};

class ExecuteInfo {
public:
    DataList param_value;
    FunctionContext func_context;
    ExecuteInfo(const DataList& _param_value, const FunctionContext& _context);
    virtual ~ExecuteInfo() = default;
};

class ExecuteInfoBuilder {
public:
    virtual ExecuteInfo* buildInfo(const DataList& _param_value, const FunctionContext& ctx);
    virtual ~ExecuteInfoBuilder() = default;
};


#endif //ISTOOL_EXECUTE_LOG_H
