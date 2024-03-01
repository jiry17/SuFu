//
// Created by pro on 2021/12/3.
//

#include "istool/basic/type.h"
#include "glog/logging.h"

std::string SimpleType::getBaseName() {
    return getName();
}
TypeList SimpleType::getParams() {
    return {};
}
bool SimpleType::equal(Type *type) {
    return getName() == type->getName();
}
int SimpleType::getTupleLen() {return 1;}

std::string TBot::getName() {
    return "Bot";
}
PType TBot::clone(const TypeList &params) {
    return std::make_shared<TBot>();
}
std::string TBot::getHaskellName() {
    return "Unit";
}

TVar::TVar(const std::string &_name): name(_name) {
}
PType TVar::clone(const TypeList &params) {
    return std::make_shared<TVar>(name);
}
std::string TVar::getName() {
    return name;
}
std::string TVar::getHaskellName() {
    return getName();
}

bool type::equal(const PType &t1, const PType &t2) {
    return t1->equal(t2.get());
}

bool type::equal(Type *t1, Type *t2) {
    return t1->equal(t2);
}

std::string TBool::getName() {
    return "Bool";
}
PType TBool::clone(const TypeList &params) {
    return std::make_shared<TBool>();
}
std::string TBool::getHaskellName() {
    return "SymBool";
}

PType type::getTBool() {
    static PType bool_type = nullptr;
    if (!bool_type) {
        bool_type = std::make_shared<TBool>();
    }
    return bool_type;
}

PType type::getTVarA() {
    static PType var_type;
    if (!var_type) var_type = std::make_shared<TVar>("a");
    return var_type;
}

std::string type::typeList2String(const TypeList &type_list) {
    std::string res = "[";
    for (int i = 0; i < type_list.size(); ++i) {
        if (i) res += ",";
        res += type_list[i]->getName();
    }
    return res + "]";
}

namespace {
    std::pair<bool, PType> _substituteName(const PType& type, const std::unordered_map<std::string, PType>& type_map) {
        auto* tv = dynamic_cast<TVar*>(type.get());
        if (tv) {
            auto it = type_map.find(tv->name);
            if (it == type_map.end()) return {false, type};
            return {true, it->second};
        }
        TypeList res;
        bool is_changed = false;
        for (const auto& param: type->getParams()) {
            auto sub_res = _substituteName(param, type_map);
            if (sub_res.first) is_changed = true;
            res.push_back(sub_res.second);
        }
        if (is_changed) return {true, type->clone(res)};
        return {false, type};
    }
}

PType type::substituteVar(const PType &type, const std::unordered_map<std::string, PType> &type_map) {
    auto res = _substituteName(type, type_map).second;
    return res;
}

namespace {
    void _buildTypeMap(Type* type, int& id, std::unordered_map<std::string, PType>& type_map) {
        auto* tv = dynamic_cast<TVar*>(type);
        if (tv) {
            if (type_map.find(tv->name) == type_map.end()) {
                type_map[tv->name] = std::make_shared<TVar>("x" + std::to_string(id++));
            }
            return;
        }
        for (const auto& sub_type: type->getParams()) {
            _buildTypeMap(sub_type.get(), id, type_map);
        }
    }
}

TypeList type::assignVarName(const TypeList &type_list) {
    TypeList result;
    int id = 0;
    for (const auto& type: type_list) {
        std::unordered_map<std::string, PType> type_map;
        _buildTypeMap(type.get(), id, type_map);
        result.push_back(_substituteName(type, type_map).second);
    }
    return result;
}