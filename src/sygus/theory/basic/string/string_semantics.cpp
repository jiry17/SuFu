//
// Created by pro on 2021/12/28.
//

#include "istool/sygus/theory/basic/string/string_semantics.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/basic/env.h"

#define TSTRING theory::string::getTString()
#define TINT theory::clia::getTInt()
#define TBOOL type::getTBool()
using theory::string::getStringValue;
using theory::clia::getIntValue;

StringCatSemantics::StringCatSemantics(): NormalSemantics("str.++", TSTRING, {TSTRING, TSTRING}) {}
Data StringCatSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto res = getStringValue(inp_list[0]) + getStringValue(inp_list[1]);
    return BuildData(String, res);
}

StringReplaceSemantics::StringReplaceSemantics(): NormalSemantics("str.replace", TSTRING, {TSTRING, TSTRING, TSTRING}) {}
Data StringReplaceSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]), t1 = getStringValue(inp_list[1]), t2 = getStringValue(inp_list[2]);
    auto pos = s.find(t1);
    if (pos == std::string::npos) return BuildData(String, s);
    auto res = s.replace(pos, t1.length(), t2);
    return BuildData(String, res);
}

StringAtSemantics::StringAtSemantics(): NormalSemantics("str.at", TSTRING, {TSTRING, TINT}) {}
Data StringAtSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]); int id = getIntValue(inp_list[1]);
    if (id < 0 || id >= s.length()) return BuildData(String, "");
    std::string t(1, s[id]);
    return BuildData(String, t);
}

StringSubStrSemantics::StringSubStrSemantics(): NormalSemantics("str.substr", TSTRING, {TSTRING, TINT, TINT}) {}
Data StringSubStrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]); int pos = getIntValue(inp_list[1]), len = getIntValue(inp_list[2]);
    if (len < 0 || pos < 0 || pos >= s.length()) return BuildData(String, "");
    return BuildData(String, s.substr(pos, len));
}

StringContainsSemantics::StringContainsSemantics(): NormalSemantics("str.contains", TBOOL, {TSTRING, TSTRING}) {}
Data StringContainsSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]), t = getStringValue(inp_list[1]);
    auto pos = s.find(t);
    return BuildData(Bool, pos != std::string::npos);
}

StringPrefixOfSemantics::StringPrefixOfSemantics(): NormalSemantics("str.prefixof", TBOOL, {TSTRING, TSTRING}) {}
Data StringPrefixOfSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[1]), t = getStringValue(inp_list[0]);
    return BuildData(Bool, s.substr(0, t.length()) == t);
}

StringSuffixOfSemantics::StringSuffixOfSemantics(): NormalSemantics("str.suffixof", TBOOL, {TSTRING, TSTRING}) {}
Data StringSuffixOfSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[1]), t = getStringValue(inp_list[0]);
    if (t.length() > s.length()) return BuildData(Bool, false);
    auto pos = s.length() - t.length();
    return BuildData(Bool, s.substr(pos, t.length()) == t);
}

StringIndexOfSemantics::StringIndexOfSemantics(): NormalSemantics("str.indexof", TINT, {TSTRING, TSTRING, TINT}) {}
Data StringIndexOfSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]), t = getStringValue(inp_list[1]);
    int l = getIntValue(inp_list[2]);
    if (l < 0 || l > s.length()) return BuildData(Int, -1);
    auto pos = s.find(t, l);
    if (pos == std::string::npos) return BuildData(Int, -1);
    return BuildData(Int, pos);
}

StringLenSemantics::StringLenSemantics(): NormalSemantics("str.len", TINT, {TSTRING}) {}
Data StringLenSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]);
    return BuildData(Int, s.length());
}

IntToStrSemantics::IntToStrSemantics(): NormalSemantics("int.to.str", TSTRING, {TINT}) {}
Data IntToStrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto i = getIntValue(inp_list[0]);
    if (i < 0) return BuildData(String, "");
    return BuildData(String, std::to_string(i));
}

StrToIntSemantics::StrToIntSemantics(Data *_inf): inf(_inf), NormalSemantics("str.to.int", TINT, {TSTRING}) {}
Data StrToIntSemantics::run(DataList &&inp_list, ExecuteInfo* info) {
    auto s = getStringValue(inp_list[0]);
    if (s.empty()) return BuildData(Int, -1);
    for (char c: s) {
        if (c > '9' || c < '0') return BuildData(Int, -1);
    }
    int res = 0;
    for (char c: s) {
        long long ne = 10ll * res + int(c - '0');
        if (ne > getIntValue(*inf)) throw SemanticsError();
        res = ne;
    }
    return BuildData(Int, res);
}


