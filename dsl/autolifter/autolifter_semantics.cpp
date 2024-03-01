//
// Created by pro on 2022/2/22.
//

#include "istool/dsl/autolifter/autolifter_semantics.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/data_util.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "glog/logging.h"
#include <unordered_set>

namespace {
    void _getVarName(Type* type, std::unordered_set<std::string>& name_set) {
        auto* vt = dynamic_cast<TVar*>(type);
        if (vt) {
            name_set.insert(vt->name); return;
        }
        for (const auto& type: type->getParams()) {
            _getVarName(type.get(), name_set);
        }
    }

    std::vector<std::string> _getVarName(Type* type) {
        std::unordered_set<std::string> name_set;
        _getVarName(type, name_set);
        return {name_set.begin(), name_set.end()};
    }

    PType _getTVarB() {
        static PType res;
        if (!res) res = std::make_shared<TVar>("b");
        return res;
    }

    PType _getFMapOutputType(const PType& F) {
        auto name_list = _getVarName(F.get());
        if (name_list.size() > 1) {
            LOG(FATAL) << "FMap require endofunctors, but get " << F->getName();
        }
        if (name_list.empty()) return F;
        auto name = name_list[0];
        return type::substituteVar(F, {{name, _getTVarB()}});
    }

    TypeList _getFMapInputType(const PType& F) {
        auto name_list = _getVarName(F.get());
        if (name_list.size() > 1) {
            LOG(FATAL) << "FMap require endofunctors, but get " << F->getName();
        }
        PType func_type = std::make_shared<TArrow>((TypeList){type::getTVarA()}, _getTVarB());
        if (name_list.empty()) return {func_type, F};
        auto name = name_list[0];
        return {func_type, type::substituteVar(F, {{name, type::getTVarA()}})};
    }
}

FMapSemantics::FMapSemantics(Env* _env, const PType& _F): env(_env), F(_F.get()),
    NormalSemantics("fmap", _getFMapOutputType(_F), _getFMapInputType(_F)) {}
Data FMapSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto sem = ext::ho::getSemantics(inp_list[0]);
    auto p = std::make_shared<Program>(sem, (ProgramList){program::buildParam(0)});
    return ext::ho::polyFMap(p.get(), F, inp_list[1], env);
}