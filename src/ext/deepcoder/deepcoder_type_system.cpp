//
// Created by pro on 2022/1/25.
//

#include "istool/ext/deepcoder/deepcoder_type_system.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/basic/type.h"
#include <unordered_set>
#include "glog/logging.h"

typedef std::pair<PType, PType> Equation;
typedef std::vector<Equation> EquationList;

namespace {
    std::pair<bool, PType> _applySub(const PType& x, const std::string& name, const PType& y) {
        auto* tv = dynamic_cast<TVar*>(x.get());
        if (tv) {
            if (tv->name != name) return {false, x};
            return {true, y};
        }
        TypeList sub_types;
        bool is_changed = false;
        for (const auto& sub: x->getParams()) {
            auto sub_res = _applySub(sub, name, y);
            if (sub_res.first) is_changed = true;
            sub_types.push_back(sub_res.second);
        }
        if (is_changed) return {true, x->clone(sub_types)};
        return {false, x};
    }

    bool _isOccur(const PType& x, const std::string& name) {
        auto* tv = dynamic_cast<TVar*>(x.get());
        if (tv) return tv->name == name;
        for (const auto& sub: x->getParams()) {
            if (_isOccur(sub, name)) return true;
        }
        return false;
    }

    bool _getMGU(const EquationList& equation_list, std::unordered_map<std::string, PType>& res) {
        auto equations = equation_list; res.clear();
        for (int i = 0; i < equations.size(); ++i) {
            auto l = equations[i].first, r = equations[i].second;
            auto *lv = dynamic_cast<TVar *>(l.get()), *rv = dynamic_cast<TVar *>(r.get());
            if (!lv) {
                std::swap(l, r);
                std::swap(lv, rv);
            }
            if (lv) {
                if (_isOccur(r, lv->name)) return false;
                for (int j = i + 1; j < equations.size(); ++j) {
                    equations[j].first = _applySub(equations[j].first, lv->name, r).second;
                    equations[j].second = _applySub(equations[j].second, lv->name, r).second;
                }
                for (auto &info: res) {
                    info.second = _applySub(info.second, lv->name, r).second;
                }
                res[lv->name] = r;
                continue;
            }
            if (l->getBaseName() != r->getBaseName()) return false;
            auto l_params = l->getParams(), r_params = r->getParams();
            if (l_params.size() != r_params.size()) return false;
            for (int j = 0; j < l_params.size(); ++j) {
                equations.emplace_back(l_params[j], r_params[j]);
            }
        }
        return true;
    }
}

DeepCoderTypeSystem::DeepCoderTypeSystem(TypeExtension *ext): TypeSystem(ext) {
}

PType DeepCoderTypeSystem::intersect(const PType &x, const PType &y) {
    std::unordered_map<std::string, PType> res;
    if (!_getMGU({{x, y}}, res)) throw TypeError();
    return type::substituteVar(x, res);
}

namespace {
    std::string _getNewName(std::unordered_set<std::string>& used_name) {
        for (int i = 0; i < 26; ++i) {
            std::string name(1, 'a' + i);
            if (used_name.find(name) == used_name.end()){
                used_name.insert(name); return name;
            }
        }
        for (int i = 0;; ++i) {
            std::string name = "x" + std::to_string(i);
            if (used_name.find(name) == used_name.end()){
                used_name.insert(name); return name;
            }
        }
    }

    PType _renameVar(const PType& x, std::unordered_set<std::string>& used_name, std::unordered_map<std::string, std::string>& name_map) {
        auto* vt = dynamic_cast<TVar*>(x.get());
        if (vt) {
            auto pre_name = vt->name;
            if (name_map.find(pre_name) == name_map.end()) {
                name_map[pre_name] = _getNewName(used_name);
            }
            return std::make_shared<TVar>(name_map[pre_name]);
        }
        TypeList params;
        for (auto& type: x->getParams()) {
            params.push_back(_renameVar(type, used_name, name_map));
        }
        return x->clone(params);
    }

}

PType DeepCoderTypeSystem::getType(Program *p) {
    auto* ts = dynamic_cast<TypedSemantics*>(p->semantics.get());
    if (ts) {
        TypeList sub_list;
        std::unordered_set<std::string> used_name;
        for (const auto& sub_program: p->sub_list) {
            auto sub_type = getType(sub_program.get());
            std::unordered_map<std::string, std::string> name_map;
            sub_list.push_back(_renameVar(sub_type, used_name, name_map));
        }
        if (sub_list.size() != ts->inp_type_list.size()) throw TypeError();
        EquationList equation_list;
        std::unordered_map<std::string, std::string> name_map;
        for (int i = 0; i < sub_list.size(); ++i) {
            equation_list.emplace_back(sub_list[i], _renameVar(ts->inp_type_list[i], used_name, name_map));
        }
        auto oup_type = _renameVar(ts->oup_type, used_name, name_map);

        std::unordered_map<std::string, PType> sigma;
        if (!_getMGU(equation_list, sigma)) throw TypeError();
        return type::substituteVar(oup_type, sigma);
    }
    auto* ps = dynamic_cast<ProductSemantics*>(p->semantics.get());
    if (ps) {
        TypeList param_list;
        for (auto& sub: p->sub_list) param_list.push_back(getType(sub.get()));
        return std::make_shared<TProduct>(param_list);
    }
    auto* cs = dynamic_cast<ConstSemantics*>(p->semantics.get());
    if (cs) return type::getTVarA();
    LOG(FATAL) << "Deepcoder Type System: Unsupported semantics " << p->semantics->getName();
}