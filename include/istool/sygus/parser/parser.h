//
// Created by pro on 2021/12/8.
//

#ifndef ISTOOL_PARSER_H
#define ISTOOL_PARSER_H

#include "istool/sygus/theory/theory.h"
#include "json/json.h"

namespace parser {
    extern bool KIsRemoveDuplicated;
    Json::Value getJsonForSyGuSFile(const std::string& file_name);
    Specification* getSyGuSSpecFromJson(const Json::Value& value);
    Specification* getDepthLimitedSyGuSSpecFromJson(const Json::Value& value);
    Specification* getSyGuSSpecFromFile(const std::string& file_name);
}


#endif //ISTOOL_PARSER_H
