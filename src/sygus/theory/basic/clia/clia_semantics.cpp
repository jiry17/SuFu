//
// Created by pro on 2021/12/19.
//

#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"

using theory::clia::getIntValue;

namespace {
    PType getVar() {
        static PType t_var = nullptr;
        if (!t_var) t_var = std::make_shared<TVar>("a");
        return t_var;
    }
}

#define TINT theory::clia::getTInt()
#define TBOOL type::getTBool()
#define TVARA getVar()

IntPlusSemantics::IntPlusSemantics(Data* _inf): inf(_inf),
    NormalSemantics("+", TINT, {TINT, TINT}) {
}
Data IntPlusSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int inf_val = getIntValue(*inf);
    int w = getIntValue(inp_list[0]) + getIntValue(inp_list[1]);
    if (std::abs(w) > inf_val) {
        throw SemanticsError();
    }
    return Data(std::make_shared<IntValue>(w));
}

IntMinusSemantics::IntMinusSemantics(Data *_inf): inf(_inf),
    NormalSemantics("-", TINT, {TINT, TINT}) {
}
Data IntMinusSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int inf_val = getIntValue(*inf);
    int w = getIntValue(inp_list[0]) - getIntValue(inp_list[1]);
    if (std::abs(w) > inf_val) {
        throw SemanticsError();
    }
    return Data(std::make_shared<IntValue>(w));
}

IntTimesSemantics::IntTimesSemantics(Data* _inf): inf(_inf),
    NormalSemantics("*", TINT, {TINT, TINT}) {
}
Data IntTimesSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int inf_val = getIntValue(*inf);
    long long w = 1ll* getIntValue(inp_list[0]) * getIntValue(inp_list[1]);
    if (std::abs(w) > inf_val) {
        throw SemanticsError();
    }
    return Data(std::make_shared<IntValue>(int(w)));
}

IntDivSemantics::IntDivSemantics(): NormalSemantics("div", TINT, {TINT, TINT}) {
}
Data IntDivSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int x = getIntValue(inp_list[0]), y = getIntValue(inp_list[1]);
    if (y == 0) throw SemanticsError();
    return Data(std::make_shared<IntValue>(x / y));
}

IntModSemantics::IntModSemantics(): NormalSemantics("mod", TINT, {TINT, TINT}) {
}
Data IntModSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int x = getIntValue(inp_list[0]), y = getIntValue(inp_list[1]);
    if (y == 0) throw SemanticsError();
    return Data(std::make_shared<IntValue>(x % y));
}

LqSemantics::LqSemantics(): NormalSemantics("<", TBOOL, {TVARA, TVARA}) {
}
Data LqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(inp_list[0] < inp_list[1]));
}

LeqSemantics::LeqSemantics(): NormalSemantics("<=", TBOOL, {TVARA, TVARA}) {
}
Data LeqSemantics::run(DataList &&inp_list, ExecuteInfo* info) {
    return Data(std::make_shared<BoolValue>(inp_list[0] <= inp_list[1]));
}

GqSemantics::GqSemantics(): NormalSemantics(">", TBOOL, {TVARA, TVARA}) {
}
Data GqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(inp_list[1] < inp_list[0]));
}

GeqSemantics::GeqSemantics(): NormalSemantics(">=", TBOOL, {TVARA, TVARA}) {
}
Data GeqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(inp_list[1] <= inp_list[0]));
}

EqSemantics::EqSemantics(): NormalSemantics("=", TBOOL, {TVARA, TVARA}) {
}
Data EqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(inp_list[0] == inp_list[1]));
}

EqBoolSemantics::EqBoolSemantics(): NormalSemantics("=b", TBOOL, {TBOOL, TBOOL}) {
}
Data EqBoolSemantics::run(DataList &&inp_list, ExecuteInfo* info) {
    return Data(std::make_shared<BoolValue>(inp_list[0] == inp_list[1]));
}

NeqSemantics::NeqSemantics(): NormalSemantics("!=", TBOOL, {TVARA, TVARA}) {
}
Data NeqSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(!(inp_list[0] == inp_list[1])));
}

IteSemantics::IteSemantics(): NormalSemantics("ite", TVARA, {TBOOL, TVARA, TVARA}) {
}
Data IteSemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    auto c = sub_list[0]->run(info);
    if (c.isTrue()) return sub_list[1]->run(info);
    return sub_list[2]->run(info);
}
Data IteSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    if (inp_list[0].isTrue()) return inp_list[1]; else return inp_list[2];
}

const std::string theory::clia::KINFName = "CLIA@INF";
