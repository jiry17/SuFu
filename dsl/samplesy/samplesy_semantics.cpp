//
// Created by pro on 2022/2/1.
//

#include "istool/dsl/samplesy/samplesy_semantics.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"

using namespace samplesy;
using theory::string::getStringValue;
using theory::string::getTString;
using theory::clia::getTInt;
using theory::clia::getIntValue;

#define TSTRING getTString()
#define TVARA type::getTVarA()
#define TINT getTInt()

StringReplaceAllSemantics::StringReplaceAllSemantics(): NormalSemantics("replace", TSTRING, {TSTRING, TSTRING, TSTRING}) {}
Data StringReplaceAllSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    std::string x = getStringValue(inp_list[0]);
    std::string s = getStringValue(inp_list[1]), t = getStringValue(inp_list[2]);
    if (s.empty()) throw SemanticsError();
    for (int i = x.find(s); i != std::string::npos; i = x.find(s, i + t.length())) {
        x.replace(i, s.length(), t);
    }
    return BuildData(String, x);
}

StringDeleteSemantics::StringDeleteSemantics(): NormalSemantics("delete", TSTRING, {TSTRING, TSTRING}) {}
Data StringDeleteSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    std::string x = getStringValue(inp_list[0]);
    std::string s = getStringValue(inp_list[1]);
    auto pos = x.find(s);
    if (pos == std::string::npos) return BuildData(String, x);
    return BuildData(String, x.replace(pos, s.length(), ""));
}

StringAbsSubstrSemantics::StringAbsSubstrSemantics(): NormalSemantics("substr", TSTRING, {TSTRING, TINT, TINT}) {}
Data StringAbsSubstrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]);
    auto l = getIntValue(inp_list[1]), r = getIntValue(inp_list[2]);
    if (l < 0 || l > r || r > s.length()) return BuildData(String, "");
    return BuildData(String, s.substr(l, r - l));
}

StringIndexOfSemantics::StringIndexOfSemantics(): NormalSemantics("indexof", TINT, {TSTRING, TSTRING, TINT}) {}
Data StringIndexOfSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto s = getStringValue(inp_list[0]), t = getStringValue(inp_list[1]);
    int lim = getIntValue(inp_list[2]);
    for (auto pos = s.find(t); pos != std::string::npos; pos = s.find(t, pos + 1)) {
        if (pos >= lim) return BuildData(Int, pos);
    }
    return BuildData(Int, 0);
}

IndexMoveSemantics::IndexMoveSemantics(): NormalSemantics("move", TINT, {TINT, TINT}) {}
Data IndexMoveSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int res = getIntValue(inp_list[0]) + getIntValue(inp_list[1]);
    return BuildData(Int, res);
}