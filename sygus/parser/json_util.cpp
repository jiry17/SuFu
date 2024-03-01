//
// Created by pro on 2021/12/5.
//

#include "istool/basic/env.h"
#include "istool/sygus/parser/json_util.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include <fstream>
#include <sstream>
#include "glog/logging.h"

Json::Value json::loadJsonFromFile(const std::string &name) {
    Json::Reader reader;
    Json::Value root;
    std::ifstream inp(name, std::ios::out);
    std::stringstream buf;
    buf << inp.rdbuf();
    inp.close();
    assert(reader.parse(buf.str(), root));
    return root;
}

PType json::getTypeFromJson(const Json::Value &value) {
    if (value.isString()) {
        std::string name = value.asString();
        if (name == "Int") return theory::clia::getTInt();
        if (name == "Bool") return type::getTBool();
        if (name == "String") return theory::string::getTString();
        TEST_PARSER(false)
    }
    TEST_PARSER(value.isArray());
    if (value.size() == 3 && value[0].isString() && value[1].isString() &&
        value[0].asString() == "_" && value[1].asString() == "BitVec") {
        int size = value[2][1].asInt();
        return theory::bv::getTBitVector(size);
    }
    if (value.size() == 2 && value[0].isString() && value[0].asString() == "BitVec") {
        int size = value[1][1].asInt();
        return theory::bv::getTBitVector(size);
    }
    TEST_PARSER(false)
}

Data json::getDataFromJson(const Json::Value &value, Env* env) {
    TEST_PARSER(value.isArray())
    auto type = getTypeFromJson(value[0]);
    if (dynamic_cast<TInt*>(type.get())) {
        TEST_PARSER(value.size() == 2 && value[1].isInt())
        return BuildData(Int, value[1].asInt());
    } else if (dynamic_cast<TBool*>(type.get())) {
        TEST_PARSER(value.size() == 2 && value[1].isString())
        return BuildData(Bool, value[1].asString() == "true");
    } else if (dynamic_cast<TString*>(type.get())) {
        TEST_PARSER(value.size() == 2 && value[1].isString());
        return BuildData(String, value[1].asString());
    }
    auto* bt = dynamic_cast<TBitVector*>(type.get());
    if (bt) {
        int size = bt->size;
        auto w = value[1].asUInt64();
        Bitset res(size, 0);
        for (int i = 0; i < size; ++i) {
            if ((w >> (unsigned long long)(i)) & 1ull) res.set(i, 1);
        }
        return BuildData(BitVector, res);
    }
    TEST_PARSER(false);
}

void json::saveJsonToFile(const Json::Value& value, const std::string& file_path) {
    auto* file = fopen(file_path.c_str(), "w");
    fprintf(file, "%s\n", value.toStyledString().c_str());
    fclose(file);
}

namespace {
    PProgram _parseProgram(const Json::Value& node, const std::unordered_map<std::string, PProgram>& var_map, Env* env) {
        if (node.isString()) {
            assert(var_map.find(node.asString()) != var_map.end());
            return var_map.find(node.asString())->second;
        }
        try {
            auto data = json::getDataFromJson(node, env);
            return program::buildConst(data);
        } catch (ParseError& e) {}
        std::string op = node[0].asString();
        auto semantics = env->getSemantics(op);
        ProgramList sub_list;
        for (int i = 1; i < node.size(); ++i) {
            sub_list.push_back(_parseProgram(node[i], var_map, env));
        }
        return std::make_shared<Program>(semantics, sub_list);
    }

    PProgram _parseProgramWithLet(const Json::Value& node, const std::unordered_map<std::string, PProgram>& var_map, std::unordered_map<std::string, PProgram>& let_map, Env* env) {
        if (node.isString()) {
            auto name = node.asString();
            if (let_map.find(name) != let_map.end()) return let_map[name];
            assert(var_map.find(node.asString()) != var_map.end());
            return var_map.find(node.asString())->second;
        }
        try {
            auto data = json::getDataFromJson(node, env);
            return program::buildConst(data);
        } catch (ParseError& e) {}
        std::string op = node[0].asString();
        if (op == "let") {
            TEST_PARSER(node[1].isArray() && node.size() == 3)
            std::vector<std::pair<std::string, PProgram>> let_list;
            for (auto& sub_node: node[1]) {
                TEST_PARSER(sub_node.isArray() && sub_node.size() == 2 && sub_node[0].isString())
                auto name = sub_node[0].asString();
                PProgram prog = _parseProgramWithLet(sub_node[1], var_map, let_map, env);
                let_list.emplace_back(name, prog);
            }
            for (auto& info: let_list) {
                TEST_PARSER(let_map.count(info.first) == 0);
                let_map[info.first] = info.second;
            }
            auto res = _parseProgramWithLet(node[2], var_map, let_map, env);
            for (auto& info: let_list) {
                let_map.erase(info.first);
            }
            return res;
        }
        auto semantics = env->getSemantics(op);
        ProgramList sub_list;
        for (int i = 1; i < node.size(); ++i) {
            sub_list.push_back(_parseProgramWithLet(node[i], var_map, let_map, env));
        }
        return std::make_shared<Program>(semantics, sub_list);
    }
}

PProgram json::getProgramFromJson(const Json::Value& node, Env* env) {
    TEST_PARSER(node.isArray() && node.size() == 5);
    TEST_PARSER(node[0].isString() && node[0].asString() == "define-fun")

    std::unordered_map<std::string, PProgram> var_map;
    int id = 0;
    for (auto& var_info: node[2]) {
        auto name = var_info[0].asString();
        auto type = json::getTypeFromJson(var_info[1]);
        var_map[name] = program::buildParam(id++, type);
    }
    auto program = _parseProgram(node[4], var_map, env);
    return program;
}

PProgram json::getProgramFromJsonWithLet(const Json::Value &node, Env *env) {
    TEST_PARSER(node.isArray() && node.size() == 5);
    TEST_PARSER(node[0].isString() && node[0].asString() == "define-fun")
    std::unordered_map<std::string, PProgram> var_map;
    int id = 0;
    for (auto& var_info: node[2]) {
        auto name = var_info[0].asString();
        auto type = json::getTypeFromJson(var_info[1]);
        var_map[name] = program::buildParam(id++, type);
    }
    std::unordered_map<std::string, PProgram> let_map;
    auto program = _parseProgramWithLet(node[4], var_map, let_map, env);
    return program;
}