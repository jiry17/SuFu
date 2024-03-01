//
// Created by pro on 2022/9/17.
//

#ifndef ISTOOL_INCRE_FROM_JSON_H
#define ISTOOL_INCRE_FROM_JSON_H

#include <json/json.h>
#include "istool/incre/language/incre_type.h"
#include "istool/incre/language/incre_term.h"
#include "istool/incre/language/incre_value.h"
#include "istool/incre/language/incre_program.h"


namespace incre {
    Ty json2ty(const Json::Value& node);
    Pattern json2pt(const Json::Value& node);
    Term json2term(const Json::Value& node);
    Binding json2binding(const Json::Value& node);
    Command json2command(const Json::Value& node);
    IncreProgram json2program(const Json::Value& node);
    IncreProgram jsonFile2program(const std::string& path);
    IncreProgram parseFromF(const std::string& path, bool is_autolabel);
}

#endif //ISTOOL_INCRE_FROM_JSON_H
