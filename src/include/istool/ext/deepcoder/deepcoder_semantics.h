//
// Created by pro on 2022/1/16.
//

#ifndef ISTOOL_DEEPCODER_SEMANTICS_H
#define ISTOOL_DEEPCODER_SEMANTICS_H

#include "istool/basic/semantics.h"
#include "istool/basic/semantics.h"

DefineNormalSemantics(IntMax)
DefineNormalSemantics(IntMin)

class ListSumSemantics: public NormalSemantics {
public:
    Data* inf;
    ListSumSemantics(Data* _inf);
    virtual Data run(DataList &&inp_list, ExecuteInfo *info);
    ~ListSumSemantics() = default;
};

DefineNormalSemantics(ListLen)
DefineNormalSemantics(ListMax)
DefineNormalSemantics(ListMin)
DefineNormalSemantics(ListHead)
DefineNormalSemantics(ListLast)
DefineNormalSemantics(ListAccess)
DefineNormalSemantics(ListCount)
DefineNormalSemantics(IntNeg)
DefineNormalSemantics(ListTake)
DefineNormalSemantics(ListDrop)
DefineNormalSemantics(ListMap)
DefineNormalSemantics(ListFilter)
DefineNormalSemantics(ListZipWith)
DefineNormalSemantics(ListScanl)
DefineNormalSemantics(ListScanr)
DefineNormalSemantics(ListRev)
DefineNormalSemantics(ListSort)
DefineNormalSemantics(IntOdd)
DefineNormalSemantics(IntEven)
DefineNormalSemantics(ListCat)
DefineNormalSemantics(ListFold)
DefineNormalSemantics(ListAppend)
DefineNormalSemantics(ListCons)
DefineNormalSemantics(ListNil)
DefineNormalSemantics(ListTail)

class ProductSemantics: public FullExecutedSemantics {
public:
    ProductSemantics();
    virtual Data run(DataList &&inp_list, ExecuteInfo* info);
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    ~ProductSemantics() = default;
};

class AccessSemantics: public FullExecutedSemantics {
public:
    int id;
    AccessSemantics(int _id);
    virtual Data run(DataList &&inp_list, ExecuteInfo *info);
    virtual std::string buildProgramString(const std::vector<std::string>& sub_exp);
    ~AccessSemantics() = default;
};

namespace ext::ho {
    void loadDeepCoderSemantics(Env* env);
}

#endif //ISTOOL_DEEPCODER_SEMANTICS_H
