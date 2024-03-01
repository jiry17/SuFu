//
// Created by pro on 2022/1/15.
//

#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/sygus/theory/basic/clia/clia_type.h"

TSum::TSum(const TypeList &_sub_types): sub_types(_sub_types) {
}
std::string TSum::getName() {
    std::string res;
    for (int i = 0; i < sub_types.size(); ++i) {
        if (i) res += "+";
        res += sub_types[i]->getName();
    }
    return res;
}
bool TSum::equal(Type *type) {
    auto* st = dynamic_cast<TSum*>(type);
    if (!st) return false;
    if (st->sub_types.size() != sub_types.size()) return false;
    for (int i = 0; i < sub_types.size(); ++i) {
        if (!sub_types[i]->equal(st->sub_types[i].get())) return false;
    }
    return true;
}
std::string TSum::getBaseName() {
    return "Sum";
}
TypeList TSum::getParams() {
    return sub_types;
}
PType TSum::clone(const TypeList &type_list) {
    return std::make_shared<TSum>(type_list);
}
std::string TSum::getHaskellName() {
    return getName();
}
int TSum::getTupleLen() {return 1;}

TProduct::TProduct(const TypeList &_sub_types): sub_types(_sub_types) {
}
std::string TProduct::getName() {
    std::string res;
    for (int i = 0; i < sub_types.size(); ++i) {
        if (i) res += " * ";
        res += sub_types[i]->getName();
    }
    return "(" + res + ")";
}
bool TProduct::equal(Type *type) {
    auto* pt = dynamic_cast<TProduct*>(type);
    if (!pt) return false;
    if (pt->sub_types.size() != sub_types.size()) return false;
    for (int i = 0; i < sub_types.size(); ++i) {
        if (!sub_types[i]->equal(pt->sub_types[i].get())) return false;
    }
    return true;
}
std::string TProduct::getBaseName() {
    return "Product";
}
TypeList TProduct::getParams() {
    return sub_types;
}
PType TProduct::clone(const TypeList &type_list) {
    return std::make_shared<TProduct>(type_list);
}
std::string TProduct::getHaskellName() {
    std::string res;
    for (int i = 0; i < sub_types.size(); ++i) {
        if (i) res += ", ";
        res += sub_types[i]->getHaskellName();
    }
    return "(" + res + ")";
}
int TProduct::getTupleLen() {
    return sub_types.size();
}

TArrow::TArrow(const TypeList &_inp_types, const PType &_oup_type): inp_types(_inp_types), oup_type(_oup_type) {
}
std::string TArrow::getName() {
    return type::typeList2String(inp_types) + "->" + oup_type->getName();
}
bool TArrow::equal(Type *type) {
    auto* at = dynamic_cast<TArrow*>(type);
    if (!at || at->inp_types.size() != inp_types.size()) return false;
    if (!oup_type->equal(at->oup_type.get())) return false;
    for (int i = 0; i < inp_types.size(); ++i) {
        if (!inp_types[i]->equal(at->inp_types[i].get())) return false;
    }
    return true;
}
std::string TArrow::getBaseName() {
    return "->";
}
TypeList TArrow::getParams() {
    auto res = inp_types;
    res.push_back(oup_type);
    return res;
}
PType TArrow::clone(const TypeList &type_list) {
    int n = type_list.size();
    TypeList inp_list(n - 1);
    for (int i = 0; i + 1 < n; ++i) inp_list[i] = type_list[i];
    PType oup = type_list[n - 1];
    return std::make_shared<TArrow>(inp_list, oup);
}
std::string TArrow::getHaskellName() {
    return getName();
}
int TArrow::getTupleLen() {return 1;}

TList::TList(const PType &_content): content(_content) {
}
std::string TList::getName() {
    return "List[" + content->getName() + "]";
}
bool TList::equal(Type *type) {
    auto* lt = dynamic_cast<TList*>(type);
    if (!lt) return false;
    return content->equal(lt->content.get());
}
std::string TList::getBaseName() {
    return "List";
}
TypeList TList::getParams() {
    return {content};
}
PType TList::clone(const TypeList &type_list) {
    return std::make_shared<TList>(type_list[0]);
}
std::string TList::getHaskellName() {
    return getName();
}
int TList::getTupleLen() {return 1;}

TBTree::TBTree(const PType &_content, const PType &_leaf): content(_content), leaf(_leaf) {
}
std::string TBTree::getName() {
    return "BTree[" + content->getName() + "," + leaf->getName() + "]";
}
bool TBTree::equal(Type* type) {
    auto* bt = dynamic_cast<TBTree*>(type);
    if (!bt) return false;
    return content->equal(bt->content.get()) && leaf->equal(bt->leaf.get());
}
std::string TBTree::getBaseName() {
    return "BTree";
}
TypeList TBTree::getParams() {
    return {content, leaf};
}
PType TBTree::clone(const TypeList &type_list) {
    return std::make_shared<TBTree>(type_list[0], type_list[1]);
}
std::string TBTree::getHaskellName() {
    return getName();
}
int TBTree::getTupleLen() {return 1;}

PType ext::ho::getTIntList() {
    static PType res;
    if (!res) res = std::make_shared<TList>(theory::clia::getTInt());
    return res;
}

PType ext::ho::getTVarAList() {
    static PType res;
    if (!res) res = std::make_shared<TList>(type::getTVarA());
    return res;
}