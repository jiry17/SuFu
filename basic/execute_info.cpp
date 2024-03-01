//
// Created by pro on 2022/1/18.
//

#include "istool/basic/execute_info.h"

ExecuteInfo::ExecuteInfo(const DataList &_param_value, const FunctionContext &_context):
    param_value(_param_value), func_context(_context) {
}
ExecuteInfo * ExecuteInfoBuilder::buildInfo(const DataList &_param_value, const FunctionContext &ctx) {
    return new ExecuteInfo(_param_value, ctx);
}