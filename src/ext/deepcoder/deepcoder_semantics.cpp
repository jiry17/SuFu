//
// Created by pro on 2022/1/16.
//

#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/ext/deepcoder/higher_order_operator.h"
#include "istool/basic/env.h"
#include "glog/logging.h"

#define TINT theory::clia::getTInt()
#define TVARA type::getTVarA()
#define TBOOL type::getTBool()
#define TINTLIST ext::ho::getTIntList()
#define TLISTA ext::ho::getTVarAList()

namespace {
    ListValue* _getList(const Data& d) {
        auto* lv = dynamic_cast<ListValue*>(d.get());
        if (!lv) LOG(FATAL) << "Expect ListValue but get " << d.toString();
        return lv;
    }
}

using theory::clia::getIntValue;
using ext::ho::getSemantics;
using ext::ho::buildList;
using ext::ho::buildProduct;

IntMaxSemantics::IntMaxSemantics(): NormalSemantics("max", TINT, {TINT, TINT}) {}
Data IntMaxSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Int, std::max(getIntValue(inp_list[0]), getIntValue(inp_list[1])));
}

IntMinSemantics::IntMinSemantics(): NormalSemantics("min", TINT, {TINT, TINT}) {}
Data IntMinSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Int, std::min(getIntValue(inp_list[0]), getIntValue(inp_list[1])));
}

ListSumSemantics::ListSumSemantics(Data *_inf): inf(_inf), NormalSemantics("sum", TINT, {TINTLIST}) {}
Data ListSumSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    int sum = 0, inf_value = getIntValue(*inf);
    for (int i = 0; i < lv->value.size(); ++i) {
        sum += getIntValue(lv->value[i]);
        if (std::abs(sum) > inf_value) throw SemanticsError();
    }
    return BuildData(Int, sum);
}

ListLenSemantics::ListLenSemantics(): NormalSemantics("len", TINT, {TLISTA}) {}
Data ListLenSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    int len = lv->value.size();
    return BuildData(Int, len);
}

ListMaxSemantics::ListMaxSemantics(): NormalSemantics("maximum", TINT, {TINTLIST}) {}
Data ListMaxSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    if (lv->value.empty()) throw SemanticsError();
    int res = getIntValue(lv->value[0]);
    for (int i = 1; i < lv->value.size(); ++i) {
        res = std::max(res, getIntValue(lv->value[i]));
    }
    return BuildData(Int, res);
}

ListMinSemantics::ListMinSemantics(): NormalSemantics("minimum", TINT, {TINTLIST}) {}
Data ListMinSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    if (lv->value.empty()) throw SemanticsError();
    int res = getIntValue(lv->value[0]);
    for (int i = 1; i < lv->value.size(); ++i) {
        res = std::min(res, getIntValue(lv->value[i]));
    }
    return BuildData(Int, res);
}

ListHeadSemantics::ListHeadSemantics(): NormalSemantics("head", TVARA, {TLISTA}) {}
Data ListHeadSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    if (lv->value.empty()) throw SemanticsError();
    return lv->value[0];
}

ListLastSemantics::ListLastSemantics(): NormalSemantics("last", TVARA, {TLISTA}) {}
Data ListLastSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    int len = lv->value.size();
    if (!len) throw SemanticsError();
    return lv->value[len - 1];
}

ListAccessSemantics::ListAccessSemantics(): NormalSemantics("access", TVARA, {TLISTA, TINT}) {}
Data ListAccessSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    int len = lv->value.size(), pos = getIntValue(inp_list[1]);
    if (pos < 0) pos += len;
    if (pos < 0 || pos >= len) throw SemanticsError();
    return lv->value[pos];
}

namespace {
    PType _getArrowType(const TypeList& inp_list, const PType& oup) {
        return std::make_shared<TArrow>(inp_list, oup);
    }

    Data _invoke(Semantics* semantics, const DataList& inp, ExecuteInfo* info) {
        ProgramList tmp_sub_list(inp.size());
        for (int i = 0; i < inp.size(); ++i) tmp_sub_list[i] = program::buildConst(inp[i]);
        return semantics->run(tmp_sub_list, info);
    }
}

ListCountSemantics::ListCountSemantics() : NormalSemantics("count", TINT, {TLISTA, _getArrowType({TVARA}, TBOOL)}) {}
Data ListCountSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    auto* sem = getSemantics(inp_list[1]).get();
    int res = 0;
    for (auto& d: lv->value) {
        if (_invoke(sem, {d}, info).isTrue()) ++res;
    }
    return BuildData(Int, res);
}

IntNegSemantics::IntNegSemantics(): NormalSemantics("neg", TINT, {TINT}) {}
Data IntNegSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int w = getIntValue(inp_list[0]);
    return BuildData(Int, -w);
}

ListTakeSemantics::ListTakeSemantics(): NormalSemantics("take", TLISTA, {TLISTA, TINT}) {}
Data ListTakeSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int pos = getIntValue(inp_list[1]);
    auto* lv = _getList(inp_list[0]);
    DataList res;
    if (pos < 0) pos += int(lv->value.size());
    for (int i = 0; i < pos && i < lv->value.size(); ++i) {
        res.push_back(lv->value[i]);
    }
    return buildList(res);
}

ListDropSemantics::ListDropSemantics(): NormalSemantics("drop", TLISTA, {TLISTA, TINT}) {}
Data ListDropSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int pos = getIntValue(inp_list[1]);
    auto* lv = _getList(inp_list[0]);
    DataList res;
    if (pos < 0) pos += int(lv->value.size());
    pos = std::max(0, pos);
    for (int i = pos; i < lv->value.size(); ++i) {
        res.push_back(lv->value[i]);
    }
    return buildList(res);
}

namespace {
    PType _getVarB() {
        static PType res;
        if (!res) res = std::make_shared<TVar>("b");
        return res;
    }

    PType _getListB() {
        static PType res;
        if (!res) res = std::make_shared<TList>(_getVarB());
        return res;
    }
    PType _getVarC() {
        static PType res;
        if (!res) res = std::make_shared<TVar>("c");
        return res;
    }

    PType _getListC() {
        static PType res;
        if (!res) res = std::make_shared<TList>(_getVarB());
        return res;
    }
}

#define TVARB _getVarB()
#define TLISTB _getListB()
#define TVARC _getVarC()
#define TLISTC _getListC()

ListMapSemantics::ListMapSemantics(): NormalSemantics("map", TLISTB, {_getArrowType({TVARA}, TVARB), TLISTA}) {}
Data ListMapSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[1]);
    auto* sem = getSemantics(inp_list[0]).get();
    DataList res(lv->value.size());
    for (int i = 0; i < lv->value.size(); ++i) {
        res[i] = _invoke(sem, {lv->value[i]}, info);
    }
    return buildList(res);
}

ListFilterSemantics::ListFilterSemantics(): NormalSemantics("filter", TLISTA, {_getArrowType({TVARA}, TBOOL), TLISTA}) {}
Data ListFilterSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* sem = getSemantics(inp_list[0]).get();
    auto* lv = _getList(inp_list[1]);
    DataList res;
    for (int i = 0; i < lv->value.size(); ++i) {
        if (_invoke(sem, {lv->value[i]}, info).isTrue()) {
            res.push_back(lv->value[i]);
        }
    }
    return buildList(res);
}

ListZipWithSemantics::ListZipWithSemantics(): NormalSemantics("zipwith", TLISTC,
        {_getArrowType({TVARA, TVARB}, TVARC), TLISTA, TLISTB}) {}
Data ListZipWithSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* sem = getSemantics(inp_list[0]).get();
    auto* x = _getList(inp_list[1]), *y = _getList(inp_list[2]);
    DataList res;
    for (int i = 0; i < x->value.size() && i < y->value.size(); ++i) {
        res.push_back(_invoke(sem, {x->value[i], y->value[i]}, info));
    }
    return buildList(res);
}

ListScanlSemantics::ListScanlSemantics(): NormalSemantics("scanl", TLISTA, {_getArrowType({TVARA, TVARA}, TVARA), TLISTA}) {}
Data ListScanlSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* sem = getSemantics(inp_list[0]).get(); auto* lv = _getList(inp_list[1]);
    if (lv->value.empty()) throw SemanticsError();
    auto current = lv->value[0];
    DataList res = {current};
    for (int i = 1; i < lv->value.size(); ++i) {
        current = _invoke(sem, {current, lv->value[i]}, info);
        res.push_back(current);
    }
    return BuildData(List, res);
}

ListScanrSemantics::ListScanrSemantics(): NormalSemantics("scanr", TLISTA, {_getArrowType({TVARA, TVARA}, TVARA), TLISTA}) {}
Data ListScanrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* sem = getSemantics(inp_list[0]).get(); auto* lv = _getList(inp_list[1]);
    if (lv->value.empty()) throw SemanticsError();
    int n = lv->value.size(); auto current = lv->value[n - 1];
    DataList res = {current};
    for (int i = n - 2; i >= 0; --i) {
        current = _invoke(sem, {lv->value[i], current}, info);
        res.push_back(current);
    }
    std::reverse(res.begin(), res.end());
    return BuildData(List, res);
}

ListRevSemantics::ListRevSemantics(): NormalSemantics("rev", TLISTA, {TLISTA}) {}
Data ListRevSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]); int n = lv->value.size();
    DataList res(n);
    for (int i = 0; i < n; ++i) res[n - i - 1] = lv->value[i];
    return buildList(res);
}

ListSortSemantics::ListSortSemantics(): NormalSemantics("sort", TLISTA, {TLISTA}) {}
Data ListSortSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* lv = _getList(inp_list[0]);
    DataList res = lv->value;
    std::sort(res.begin(), res.end());
    return buildList(res);
}

IntOddSemantics::IntOddSemantics(): NormalSemantics("odd", TBOOL, {TINT}) {}
Data IntOddSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int w = getIntValue(inp_list[0]);
    return BuildData(Bool, w % 2 == 1);
}

IntEvenSemantics::IntEvenSemantics(): NormalSemantics("even", TBOOL, {TINT}) {}
Data IntEvenSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int w = getIntValue(inp_list[0]);
    return BuildData(Bool, w % 2 == 0);
}

ListCatSemantics::ListCatSemantics(): NormalSemantics("++", TLISTA, {TLISTA, TLISTA}) {}
Data ListCatSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* x = _getList(inp_list[0]), *y = _getList(inp_list[1]);
    auto res = x->value;
    for (const auto& d: y->value) res.push_back(d);
    return buildList(res);
}

ListFoldSemantics::ListFoldSemantics(): NormalSemantics("fold", TVARB, {_getArrowType({TVARA, TVARB}, TVARB), TVARB, TLISTA}) {}
Data ListFoldSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* f = ext::ho::getSemantics(inp_list[0]).get();
    Data s = inp_list[1]; auto* lv = _getList(inp_list[2]);
    for (int i = lv->value.size(); i; --i) {
        s = _invoke(f, {lv->value[i - 1], s}, info);
    }
    return s;
}

ListAppendSemantics::ListAppendSemantics(): NormalSemantics("append", TLISTA, {TLISTA, TVARA}) {}
Data ListAppendSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* x = _getList(inp_list[0]);
    auto res = x->value; res.push_back(inp_list[1]);
    return buildList(res);
}

ListConsSemantics::ListConsSemantics(): NormalSemantics("cons", TLISTA, {TLISTA, TVARA}) {}
Data ListConsSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* x = _getList(inp_list[1]);
    DataList res = {inp_list[0]};
    for (const auto& d: x->value) res.push_back(d);
    return buildList(res);
}

ListNilSemantics::ListNilSemantics(): NormalSemantics("nil", TLISTA, {}) {}
Data ListNilSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return buildList({});
}

ListTailSemantics::ListTailSemantics(): NormalSemantics("tail", TLISTA, {}) {}
Data ListTailSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* x = _getList(inp_list[0]);
    DataList res;
    for (int i = 1; i < x->value.size(); ++i) res.push_back(x->value[i]);
    return buildList(res);
}

ProductSemantics::ProductSemantics(): FullExecutedSemantics("prod") {}
Data ProductSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Product, inp_list);
}
std::string ProductSemantics::buildProgramString(const std::vector<std::string> &sub_list) {
    std::string res = "(";
    for (int i = 0; i < sub_list.size(); ++i) {
        if (i) res += ","; res += sub_list[i];
    }
    return res + ")";
}

AccessSemantics::AccessSemantics(int _id): id(_id), FullExecutedSemantics("access" + std::to_string(_id)) {}
Data AccessSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto* pv = dynamic_cast<ProductValue*>(inp_list[0].get());
    assert(id >= 0 && id < pv->elements.size());
    return pv->elements[id];
}
std::string AccessSemantics::buildProgramString(const std::vector<std::string> &sub_exp) {
    return sub_exp[0] + "." + std::to_string(id);
}


namespace {
    const int KDefaultINF = 1e8;
}

void ext::ho::loadDeepCoderSemantics(Env *env) {
    loadHigherOrderOperators(env);
    auto* inf = env->getConstRef(theory::clia::KINFName, BuildData(Int, KDefaultINF));
    LoadSemantics("max", IntMax); LoadSemantics("min", IntMin);
    env->setSemantics("sum", std::make_shared<ListSumSemantics>(inf));
    LoadSemantics("len", ListLen); LoadSemantics("maximum", ListMax);
    LoadSemantics("minimum", ListMin); LoadSemantics("head", ListHead);
    LoadSemantics("last", ListLast); LoadSemantics("access", ListAccess);
    LoadSemantics("count", ListCount); LoadSemantics("neg", IntNeg);
    LoadSemantics("take", ListTake); LoadSemantics("drop", ListDrop);
    LoadSemantics("map", ListMap); LoadSemantics("filter", ListFilter);
    LoadSemantics("zipwith", ListZipWith); LoadSemantics("scanl", ListScanl);
    LoadSemantics("scanr", ListScanr); LoadSemantics("rev", ListRev);
    LoadSemantics("sort", ListSort); LoadSemantics("odd", IntOdd);
    LoadSemantics("even", IntEven); LoadSemantics("++", ListCat);
    LoadSemantics("fold", ListFold); LoadSemantics("append", ListAppend);
    LoadSemantics("cons", ListCons); LoadSemantics("tail", ListTail);
    LoadSemantics("nil", ListNil); LoadSemantics("prod", Product);
}