//
// Created by pro on 2021/12/28.
//

#ifndef ISTOOL_STRING_SEMANTICS_H
#define ISTOOL_STRING_SEMANTICS_H

#include "istool/basic/semantics.h"

DefineNormalSemantics(StringCat)
DefineNormalSemantics(StringReplace)
DefineNormalSemantics(StringAt)
DefineNormalSemantics(IntToStr)
DefineNormalSemantics(StringSubStr)
DefineNormalSemantics(StringLen)
DefineNormalSemantics(StringIndexOf)
DefineNormalSemantics(StringPrefixOf)
DefineNormalSemantics(StringSuffixOf)
DefineNormalSemantics(StringContains)

class StrToIntSemantics: public NormalSemantics {
public:
    Data* inf;
    StrToIntSemantics(Data* _inf);
    virtual Data run(DataList &&inp_list, ExecuteInfo *info);
    ~StrToIntSemantics() = default;
};

#endif //ISTOOL_STRING_SEMANTICS_H
