//
// Created by pro on 2022/9/17.
//

#include "istool/incre/io/incre_from_json.h"
#include "glog/logging.h"
#include <fstream>

using namespace incre;

Pattern incre::json2pt(const Json::Value &node) {
    auto type = node["type"].asString();
    if (type == "underscore") return std::make_shared<PtUnderScore>();
    if (type == "var") {
        auto name = node["name"].asString();
        return std::make_shared<PtVar>(name);
    }
    if (type == "tuple") {
        PatternList fields;
        for (const auto& sub_node: node["fields"]) {
            fields.push_back(json2pt(sub_node));
        }
        return std::make_shared<PtTuple>(fields);
    }
    if (type == "constructor") {
        auto name = node["name"].asString();
        auto content = json2pt(node["content"]);
        return std::make_shared<PtConstructor>(name, content);
    }
    LOG(FATAL) << "Unknown pattern " << node;
}

Ty incre::json2ty(const Json::Value &node) {
    auto type = node["type"].asString();
    if (type == "var") {
        auto name = node["name"].asString();
        return std::make_shared<TyVar>(name);
    }
    if (type == "unit") return std::make_shared<TyUnit>();
    if (type == "tuple") {
        TyList fields;
        for (const auto& sub_node: node["fields"]) {
            fields.push_back(json2ty(sub_node));
        }
        return std::make_shared<TyTuple>(fields);
    }
    if (type == "bool") return std::make_shared<TyBool>();
    if (type == "int") return std::make_shared<TyInt>();
    if (type == "arrow") {
        auto source = json2ty(node["s"]);
        auto target = json2ty(node["t"]);
        return std::make_shared<TyArrow>(source, target);
    }
    if (type == "inductive") {
        auto name = node["name"].asString();
        std::vector<std::pair<std::string, Ty>> cons_list;
        for (const auto& c_node: node["constructors"]) {
            auto cname = c_node["name"].asString();
            auto cty = json2ty(c_node["subtype"]);
            cons_list.emplace_back(cname, cty);
        }
        return std::make_shared<TyInductive>(name, cons_list);
    }
    if (type == "compress") {
        auto content = json2ty(node["content"]);
        return std::make_shared<TyCompress>(content);
    }
    LOG(FATAL) << "Unknown type " << node;
}

Term incre::json2term(const Json::Value &node) {
    auto type = node["type"].asString();
    if (type == "true") return std::make_shared<TmValue>(BuildData(Bool, true));
    if (type == "false") return std::make_shared<TmValue>(BuildData(Bool, false));
    if (type == "unit") return std::make_shared<TmValue>(Data(std::make_shared<VUnit>()));
    if (type == "int") {
        int v = node["value"].asInt();
        return std::make_shared<TmValue>(BuildData(Int, v));
    }
    if (type == "var") {
        auto name = node["name"].asString();
        return std::make_shared<TmVar>(name);
    }
    if (type == "if") {
        auto cond = json2term(node["condition"]);
        auto true_branch = json2term(node["true"]);
        auto false_branch = json2term(node["false"]);
        return std::make_shared<TmIf>(cond, true_branch, false_branch);
    }
    if (type == "op") {
        auto op_name = node["operator"].asString();
        auto res = incre::getOperator(op_name);
        for (const auto& sub_node: node["operand"]) {
            res = std::make_shared<TmApp>(res, json2term(sub_node));
        }
        return res;
    }
    if (type == "let") {
        auto name = node["name"].asString();
        auto def = incre::json2term(node["def"]);
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmLet>(name, def, content);
    }
    if (type == "tuple") {
        TermList fields;
        for (const auto& sub_node: node["fields"]) {
            fields.push_back(incre::json2term(sub_node));
        }
        return std::make_shared<TmTuple>(fields);
    }
    if (type == "proj") {
        auto content = incre::json2term(node["content"]);
        int index = node["index"].asInt();
        return std::make_shared<TmProj>(content, index);
    }
    if (type == "abs") {
        auto name = node["name"].asString();
        auto ty = incre::json2ty(node["vartype"]);
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmAbs>(name, ty, content);
    }
    if (type == "app") {
        auto func = incre::json2term(node["func"]);
        auto param = incre::json2term(node["param"]);
        return std::make_shared<TmApp>(func, param);
    }
    if (type == "fix") {
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmFix>(content);
    }
    if (type == "constructor") LOG(FATAL) << "Unexpected term type \"constructor\" found in " << node;
    if (type == "inductive") LOG(FATAL) << "Unexpected term type \"inductive\" found in " << node;
    if (type == "match") {
        auto value = incre::json2term(node["value"]);
        std::vector<std::pair<Pattern, Term>> cases;
        for (const auto& sub_node: node["cases"]) {
            cases.emplace_back(incre::json2pt(sub_node["pattern"]), incre::json2term(sub_node["branch"]));
        }
        return std::make_shared<TmMatch>(value, cases);
    }
    if (type == "label") {
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmLabel>(content);
    }
    if (type == "unlabel") {
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmUnLabel>(content);
    }
    if (type == "align") {
        auto content = incre::json2term(node["content"]);
        return std::make_shared<TmAlign>(content);
    }
    LOG(FATAL) << "Unknown term " << node;
}

Binding incre::json2binding(const Json::Value &node) {
    auto type = node["type"].asString();
    if (type == "term") {
        auto term = incre::json2term(node["content"]);
        return std::make_shared<TermBinding>(term);
    }
    if (type == "type") {
        auto ty = incre::json2ty(node["content"]);
        return std::make_shared<TypeBinding>(ty);
    }
    if (type == "var") {
        auto ty = incre::json2ty(node["content"]);
        return std::make_shared<VarTypeBinding>(ty);
    }
    LOG(FATAL) << "Unknown Binding";
}

Command incre::json2command(const Json::Value &full_node) {
    auto node = full_node["node"];
    auto deco_node = full_node["decorates"];
    auto type = node["type"].asString();
    if (type == "import") {
        auto name = node["name"].asString();
        CommandList content;
        for (auto& sub_node: node["content"]) {
            content.push_back(incre::json2command(sub_node));
        }
        return std::make_shared<CommandImport>(name, content);
    }
    if (type == "skip") return nullptr;
    if (type == "bind") {
        auto name = node["name"].asString();
        auto content = incre::json2binding(node["def"]);

        DecorateSet decorate_set;
        for (auto& deco: deco_node) {
            decorate_set.insert(string2Decorate(deco.asString()));
        }

        return std::make_shared<CommandBind>(name, content, decorate_set);
    }
    if (type == "defind") {
        auto ty = incre::json2ty(node["indtype"]);
        return std::make_shared<CommandDefInductive>(ty);
    }
    LOG(FATAL) << "Unknown command " << node;
}

#include "istool/sygus/theory/basic/string/string_value.h"

namespace {
    Data _configValue2Data(const Json::Value& node) {
        auto type = node["type"].asString();
        if (type == "int") {
            return BuildData(Int, node["value"].asInt());
        }
        if (type == "bool") {
            return BuildData(Bool, node["value"].asBool());
        }
        if (type == "string") {
            return BuildData(String, node["value"].asString());
        }
        LOG(FATAL) << "Unknown config value " << node;
    }
}

incre::IncreProgram incre::json2program(const Json::Value &node) {
    CommandList commands;
    IncreConfigMap config_map;
    for (auto& sub_node: node) {
        if (sub_node["node"]["type"].asString() == "config") {
            auto command_node = sub_node["node"];
            auto value = _configValue2Data(command_node["value"]);
            config_map[incre::string2ConfigType(command_node["name"].asString())] = value;
            continue;
        }
        auto command = incre::json2command(sub_node);
        if (!command) continue;
        commands.push_back(command);
    }
    return std::make_shared<incre::ProgramData>(commands, config_map);
}

incre::IncreProgram incre::jsonFile2program(const std::string &path) {
    Json::Reader reader;
    Json::Value root;
    std::ifstream inp(path, std::ios::out);
    std::stringstream buf;
    buf << inp.rdbuf();
    inp.close();
    assert(reader.parse(buf.str(), root));
    return incre::json2program(root);
}