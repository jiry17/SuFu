//
// Created by pro on 2022/1/15.
//

#include "istool/ext/deepcoder/higher_order_operator.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/basic/env.h"
#include "glog/logging.h"

ApplySemantics::ApplySemantics(): Semantics("apply") {}
Data ApplySemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    auto s = ext::ho::getSemantics(sub_list[0]->run(info));
    ProgramList sub;
    for (int i = 1; i < sub_list.size(); ++i) sub.push_back(sub_list[i]);
    return s->run(sub, info);
}

CurrySemantics::CurrySemantics(): Semantics("curry") {}
Data CurrySemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    assert(sub_list.size() == 1);
    auto p = sub_list[0];
    auto f = [p](const ProgramList& extra_sub_list, ExecuteInfo* _info) {
        auto full_sub_list = p->sub_list;
        for (const auto& extra: extra_sub_list) full_sub_list.push_back(extra);
        return p->semantics->run(full_sub_list, _info);
    };
    return ext::ho::buildAnonymousData(f);
}

TmpSemantics::TmpSemantics(const std::string &_name): FullExecutedSemantics(_name) {}
std::string TmpSemantics::buildProgramString(const std::vector<std::string> &sub_exp) {
    return name;
}
Data TmpSemantics::run(DataList &&inp, ExecuteInfo *info) {
    auto* ti = dynamic_cast<TmpExecuteInfo*>(info);
    if (!ti) {
        LOG(FATAL) << "TmpSemantics require TmpExecuteInfo";
    }
    return ti->get(name, true);
}

namespace {
    std::string _getLambdaName(const std::vector<std::string>& name_list) {
        std::string res("lambda ");
        for (int i = 0; i < name_list.size(); ++i) {
            if (i) res += ","; res += name_list[i];
        }
        return res + ".";
    }
}

LambdaSemantics::LambdaSemantics(const std::vector<std::string> &_name_list):
    Semantics(_getLambdaName(_name_list)), name_list(_name_list) {
}
Data LambdaSemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    assert(sub_list.size() == 1);
    auto p = sub_list[0];
    auto tmp_list = name_list;
    auto f = [p, tmp_list](DataList&& tmp_values, ExecuteInfo* _info) {
        assert(tmp_values.size() == tmp_list.size());
        auto* ti = dynamic_cast<TmpExecuteInfo*>(_info);
        if (!ti) {
            LOG(FATAL) << "TmpSemantics require TmpExecuteInfo";
        }
        DataList pre_values;
        for (int i = 0; i < tmp_values.size(); ++i) {
            pre_values.push_back(ti->get(tmp_list[i], false));
            ti->set(tmp_list[i], tmp_values[i]);
        }
        auto res = p->run(ti);
        for (int i = 0; i < tmp_values.size(); ++i)
            if (pre_values[i].isNull()) ti->clear(tmp_list[i]);
            else ti->set(tmp_list[i], pre_values[i]);
        return res;
    };
    return ext::ho::buildAnonymousData(f);
}

TriangleSemantics::TriangleSemantics(): FullExecutedSemantics("tri") {}
Data TriangleSemantics::run(DataList &&func_list, ExecuteInfo *info) {
    auto f = [func_list](const ProgramList& sub_list, ExecuteInfo* info) {
        DataList res;
        for (auto& func: func_list) {
            auto sem = ext::ho::getSemantics(func);
            res.push_back(sem->run(sub_list, info));
        }
        return BuildData(Product, res);
    };
    return ext::ho::buildAnonymousData(f);
}

void ext::ho::loadHigherOrderOperators(Env *env) {
    LoadSemantics("apply", Apply); LoadSemantics("curry", Curry);
    LoadSemantics("tri", Triangle);
    registerTmpExecuteInfo(env);
}