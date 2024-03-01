//
// Created by pro on 2021/12/5.
//

#ifndef ISTOOL_JSON_UTIL_H
#define ISTOOL_JSON_UTIL_H

#include "istool/basic/data.h"
#include "istool/basic/program.h"
#include "json/json.h"
#include <exception>

struct ParseError: public std::exception {
};

#define TEST_PARSER(b) if (!(b)) throw ParseError();

namespace json {
    Json::Value loadJsonFromFile(const std::string& name);
    PType getTypeFromJson(const Json::Value& value);
    Data getDataFromJson(const Json::Value& value, Env* env);
    void saveJsonToFile(const Json::Value& value, const std::string& file_path);
    PProgram getProgramFromJson(const Json::Value& value, Env* env);
    PProgram getProgramFromJsonWithLet(const Json::Value& value, Env* env);
}


#endif //ISTOOL_JSON_UTIL_H
