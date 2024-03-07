//
// Created by pro on 2022/11/16.
//

#include "istool/incre/autolifter/incre_aux_semantics.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/incre/language/incre.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/sygus/theory/theory.h"
#include "istool/sygus/theory/basic/theory_semantics.h"
#include "glog/logging.h"

using namespace incre::autolifter;
using namespace incre;

namespace {
    bool isUnit(TyData* type) {
        return type->getType() == TyType::UNIT;
    }

    TypeList _getFoldTInp(const PType& type, const PType& oup_type) {
        auto* it = dynamic_cast<TIncreInductive*>(type.get());
        assert(it);
        auto* ind_type = it->type;
        auto oup_incre_type = typeToIncre(oup_type.get());

        TypeList inp_type_list = {type};
        for (auto& [_, cons_type]: ind_type->constructors) {
            auto inp_incre_type = incre::subst(cons_type, ind_type->name, oup_incre_type);
            auto inp_type = incre::typeFromIncre(inp_incre_type);
            inp_type_list.push_back(std::make_shared<TArrow>((TypeList){inp_type}, oup_type));
        }
        return inp_type_list;
    }
}
global::IncreFold::IncreFold(const PType &ind_type, const PType& oup_type): NormalSemantics("fold@" + ind_type->getName(),
        oup_type, _getFoldTInp(ind_type, oup_type)) {
    auto* it = dynamic_cast<TIncreInductive*>(ind_type.get());
    type = it->type;

    for (int i = 0; i < type->constructors.size(); ++i) {
        auto cons_name = type->constructors[i].first;
        cons_map[cons_name] = i;
    }
}
Data global::IncreFold::_run(TyData* current_type, const Data& data, const std::vector<PSemantics> &semantics_list, ExecuteInfo* info) {
    switch (current_type->getType()) {
        case TyType::COMPRESS:
        case TyType::ARROW:
            LOG(FATAL) << "Unsupported type " << current_type->toString();
        case TyType::IND:
        case TyType::INT:
        case TyType::UNIT:
        case TyType::BOOL:
            return data;
        case TyType::VAR: {
            auto* iv = dynamic_cast<VInductive*>(data.get());
            assert(iv);
            int pos = cons_map[iv->name];
            auto res = _run(type->constructors[pos].second.get(), iv->content, semantics_list, info);
            return semantics_list[pos]->run({program::buildConst(res)}, info);
        }
        case TyType::TUPLE: {
            auto* pv = dynamic_cast<VTuple*>(data.get());
            auto* pt = dynamic_cast<TyTuple*>(current_type);
            assert(pv && pt);
            DataList fields;
            for (int i = 0; i < pv->elements.size(); ++i) {
                fields.push_back(_run(pt->fields[i].get(), pv->elements[i], semantics_list, info));
            }
            return BuildData(Product, fields);
        }
    }
}
Data global::IncreFold::run(DataList &&inp_list, ExecuteInfo *info) {
    std::vector<PSemantics> semantics_list;
    for (int i = 1; i < inp_list.size(); ++i) {
        semantics_list.push_back(ext::ho::getSemantics(inp_list[i]));
    }
    auto tv = std::make_shared<TyVar>(type->name);
    return _run(tv.get(), inp_list[0], semantics_list, info);
}


bool list::isList(Type *type) {
    auto* it = dynamic_cast<TIncreInductive*>(type);
    if (!it) return false;
    auto* x = it->type;
    if (x->constructors.size() != 2) return false;
    auto cons = x->constructors[0], nil = x->constructors[1];
    if (!isUnit(nil.second.get())) std::swap(cons, nil);
    if (!isUnit(nil.second.get())) return false;

    if (cons.second->getType() != TyType::TUPLE) return false;
    auto* pt = dynamic_cast<TyTuple*>(cons.second.get());
    if (pt->fields.size() != 2) return false;

    auto field_x = pt->fields[0], field_y = pt->fields[1];
    // if (field_x->getType() != TyType::INT) std::swap(field_x, field_y);
    return field_x->getType() == TyType::INT && field_y->getType() == TyType::VAR;
}

namespace {
    void _getList(Value* value, std::vector<int>& res) {
        // LOG(INFO) << value->toString();
        auto* iv = dynamic_cast<VInductive*>(value);
        assert(iv);
        auto* uv = dynamic_cast<VUnit*>(iv->content.get());
        if (uv) return;
        auto* tv = dynamic_cast<VTuple*>(iv->content.get());
        assert(tv && tv->elements.size() == 2);
        Value* sub;
        for (auto& d: tv->elements) {
            auto* x = dynamic_cast<VInt*>(d.get());
            if (x) res.push_back(x->w);
            else sub = dynamic_cast<VInductive*>(d.get());
        }
        _getList(sub, res);
    }
    std::vector<int> _getList(Value* value) {
        std::vector<int> res;
        _getList(value, res);
        return res;
    }

    Data _buildList(int pos, const std::vector<int>& x, const std::function<Data(int, const Data&)>& cons, const Data& nil) {
        if (pos == x.size()) return nil;
        return cons(x[pos], _buildList(pos + 1, x, cons, nil));
    }
    Data _buildList(const std::vector<int>& x, TyInductive* ind) {
        auto cons = ind->constructors[0], nil = ind->constructors[1];
        if (nil.second->getType() != TyType::UNIT) std::swap(cons, nil);
        Data nil_data(std::make_shared<VInductive>(nil.first, Data(std::make_shared<VUnit>())));
        auto cons_maker = [=](int x, const Data& y) {
            Data content(std::make_shared<VTuple>((DataList){BuildData(Int, x), y}));
            return Data(std::make_shared<VInductive>(cons.first, content));
        };
        return _buildList(0, x, cons_maker, nil_data);
    }

    TyInductive* _getTyInductive(const PType& type) {
        auto* t = dynamic_cast<TIncreInductive*>(type.get());
        assert(t);
        return t->type;
    }
#define TINT theory::clia::getTInt()
#define TVAR type::getTVarA()

#define IncreSemanticsInit(nname, sname, oup_type, inp_type) \
list::Incre ## nname ## Semantics:: Incre ## nname ## Semantics(const PType& __type): \
    NormalSemantics(sname, _replaceVar(oup_type, __type), _replaceVarList(inp_type, __type)), \
    _type(__type), type(_getTyInductive(__type)) { \
    assert(isList(_type.get())); \
    name += "@" + type->name; \
}

    PType _replaceVar(const PType& x, const PType& y) {
        if (type::equal(x, TVAR)) return y;
        return x;
    }
    TypeList _replaceVarList(const TypeList& x, const PType& y) {
        TypeList res(x.size());
        for (int i = 0; i < x.size(); ++i) {
            if (type::equal(x[i], TVAR)) res[i] = y; else res[i] = x[i];
        }
        return res;
    }
}

IncreSemanticsInit(ListHead, "head", TINT, {TVAR})
Data list::IncreListHeadSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    if (l.empty()) throw SemanticsError();
    return BuildData(Int, l[0]);
}

IncreSemanticsInit(ListMax, "maximum", TINT, {TVAR})
Data list::IncreListMaxSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    if (l.empty()) throw SemanticsError();
    int res = l[0];
    for (auto w: l) res = std::max(res, w);
    return BuildData(Int, res);
}

IncreSemanticsInit(ListMin, "minimum", TINT, {TVAR})
Data list::IncreListMinSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    if (l.empty()) throw SemanticsError();
    int res = l[0];
    for (auto w: l) res = std::min(res, w);
    return BuildData(Int, res);
}

IncreSemanticsInit(ListLen, "len", TINT, {TVAR})
Data list::IncreListLenSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    return BuildData(Int, l.size());
}

IncreSemanticsInit(ListLast, "last", TINT, {TVAR})
Data list::IncreListLastSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    if (l.empty()) throw SemanticsError();
    return BuildData(Int, l[l.size() - 1]);
}

IncreSemanticsInit(ListSum, "sum", TINT, {TVAR});
Data list::IncreListSumSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    int res = 0; auto l = _getList(inp_list[0].get());
    for (auto w: l) res += w;
    return BuildData(Int, res);
}

using theory::clia::getIntValue;

namespace {
    TypeList _getAccessInputs() {
        return {TVAR, TINT};
    }
}

IncreSemanticsInit(ListAccess, "access", TINT, _getAccessInputs())
Data list::IncreListAccessSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get()); int pos = getIntValue(inp_list[1]);
    if (pos < 0 || pos >= l.size()) throw SemanticsError();
    return BuildData(Int, l[pos]);
}

namespace {
#define TBOOL type::getTBool()
    TypeList _getCountInputs() {
        return {TVAR, std::make_shared<TArrow>((TypeList){TINT}, TBOOL)};
    }
}

IncreSemanticsInit(ListCount, "access", TINT, _getCountInputs())
Data list::IncreListCountSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto semantics = ext::ho::getSemantics(inp_list[1]);
    auto l = _getList(inp_list[0].get());
    int res = 0;
    for (auto w: l) {
        if (semantics->run({program::buildConst(BuildData(Int, w))}, info).isTrue()) ++res;
    }
    return BuildData(Int, res);
}

namespace {
    TypeList _getMapInputs() {
        return {TVAR, std::make_shared<TArrow>((TypeList){TINT}, TINT)};
    }
}

IncreSemanticsInit(ListMap, "map", TVAR, _getMapInputs())
Data list::IncreListMapSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto semantics = ext::ho::getSemantics(inp_list[1]);
    auto l = _getList(inp_list[0].get());
    for (auto& w: l) {
        w = getIntValue(semantics->run({program::buildConst(BuildData(Int, w))}, info));
    }
    return _buildList(l, type);
}

IncreSemanticsInit(ListFilter, "filter", TVAR, _getCountInputs())
Data list::IncreListFilterSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto semantics = ext::ho::getSemantics(inp_list[1]);
    auto l = _getList(inp_list[0].get()); std::vector<int> res;
    for (auto w: l) {
        if (semantics->run({program::buildConst(BuildData(Int, w))}, info).isTrue()) {
            res.push_back(w);
        }
    }
    return _buildList(l, type);
}

namespace {
    TypeList _getScanInputs() {
        return {TVAR, std::make_shared<TArrow>((TypeList){TINT, TINT}, TINT)};
    }
}

IncreSemanticsInit(ListScanl, "scanl", TVAR, _getScanInputs())
Data list::IncreListScanlSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get()); auto op = ext::ho::getSemantics(inp_list[1]);
    std::vector<int> res(l);
    for (int i = 1; i < res.size(); ++i) {
        auto w = op->run({program::buildConst(BuildData(Int, res[i - 1])),
                          program::buildConst(BuildData(Int, res[i]))}, info);
        res[i] = getIntValue(w);
    }
    return _buildList(res, type);
}

IncreSemanticsInit(ListScanr, "scanr", TVAR, _getScanInputs())
Data list::IncreListScanrSemantics::run(DataList &&inp_list, ExecuteInfo* info) {
    auto l = _getList(inp_list[0].get()); auto op = ext::ho::getSemantics(inp_list[1]);
    std::vector<int> res(l); int n = l.size();
    for (int i = n - 2; i >= 0; --i) {
        auto w = op->run({program::buildConst(BuildData(Int, res[i])),
                          program::buildConst(BuildData(Int, res[i + 1]))}, info);
        res[i] = getIntValue(w);
    }
    return _buildList(res, type);
}

IncreSemanticsInit(ListRev, "rev", TVAR, {TVAR})
Data list::IncreListRevSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    std::reverse(l.begin(), l.end());
    return _buildList(l, type);
}

IncreSemanticsInit(ListSort, "sort", TVAR, {TVAR})
Data list::IncreListSortSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto l = _getList(inp_list[0].get());
    std::sort(l.begin(), l.end());
    return _buildList(l, type);
}

std::vector<std::pair<PType, Data> > list::getConstList(Env* env) {
    std::vector<std::pair<PType, Data>> res;
    for (auto w: {1, 2, -1, -2}) res.emplace_back(TINT, BuildData(Int, w));
    auto binary_op_type = std::make_shared<TArrow>((TypeList){TINT, TINT}, TINT);
    for (auto name: {"+", "-", "*", "min", "max"}) {
        res.emplace_back(binary_op_type, std::make_shared<SemanticsValue>(env->getSemantics(name)));
    }
    auto unary_op_type = std::make_shared<TArrow>((TypeList){TINT}, TINT);
    for (auto name: {"neg"}) {
        res.emplace_back(unary_op_type, std::make_shared<SemanticsValue>(env->getSemantics(name)));
    }
    auto inc = [](DataList&& inp, ExecuteInfo* info) {
        auto x = getIntValue(inp[0]); return BuildData(Int, x + 1);
    };
    auto inc_data = ext::ho::buildAnonymousData(inc, "inc");
    auto dec = [](DataList&& inp, ExecuteInfo* info) {
        auto x = getIntValue(inp[0]); return BuildData(Int, x - 1);
    };
    auto dec_data = ext::ho::buildAnonymousData(dec, "dec");
    res.emplace_back(unary_op_type, inc_data); res.emplace_back(unary_op_type, dec_data);

    auto test_type = std::make_shared<TArrow>((TypeList){TINT}, TBOOL);
    auto is_pos = [](DataList&& inp, ExecuteInfo* info) {
        auto x = getIntValue(inp[0]); return BuildData(Bool, x > 0);
    };
    auto is_pos_data = ext::ho::buildAnonymousData(is_pos, "(>0)");
    auto is_neg = [](DataList&& inp, ExecuteInfo* info) {
        auto x = getIntValue(inp[0]); return BuildData(Bool, x < 0);
    };
    auto is_neg_data = ext::ho::buildAnonymousData(is_neg, ("(<0)"));
    res.emplace_back(test_type, is_pos_data); res.emplace_back(test_type, is_neg_data);
    for (auto name: {"odd", "even"}) {
        res.emplace_back(test_type, std::make_shared<SemanticsValue>(env->getSemantics(name)));
    }
    return res;
}

#define AddIncreListSemantics(name) res.push_back(std::make_shared<Incre ## name ## Semantics>(base_type))

std::vector<PSemantics> list::getSemanticsList(const PType &base_type) {
    std::vector<PSemantics> res;
    AddIncreListSemantics(ListSum); AddIncreListSemantics(ListLen);
    AddIncreListSemantics(ListMax); AddIncreListSemantics(ListMin);
    AddIncreListSemantics(ListHead); AddIncreListSemantics(ListLast);
    AddIncreListSemantics(ListAccess); AddIncreListSemantics(ListCount);
    AddIncreListSemantics(ListMap); AddIncreListSemantics(ListFilter);
    AddIncreListSemantics(ListScanl); AddIncreListSemantics(ListScanr);
    AddIncreListSemantics(ListRev); AddIncreListSemantics(ListSort);
    return res;
}