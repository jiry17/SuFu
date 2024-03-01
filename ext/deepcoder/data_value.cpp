//
// Created by pro on 2022/1/15.
//

#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "glog/logging.h"

ProductValue::ProductValue(const DataList &_elements): elements(_elements), Value() {
}
Data ProductValue::get(int id) const {
    if (id < 0 || id > elements.size()) {
        LOG(FATAL) << "Access product " << toString() << " with invalid index " << id;
    }
    return elements[id];
}
std::string ProductValue::toString() const {
    std::string res = "";
    for (int i = 0; i < elements.size(); ++i) {
        if (i) res += " ";
        res += "(";
        res += elements[i].toString();
        res += ")";
    }
    return res;
}
std::string ProductValue::toHaskell(bool in_result = false) const {
    std::string res = "";
    if (in_result) {
        res += "(";
        for (int i = 0; i < elements.size(); ++i) {
            if (i) res += ", ";
            res += "(";
            res += elements[i].value->toHaskell(true);
            res += ")";
        }
        res += ")";
    } else {
        for (int i = 0; i < elements.size(); ++i) {
            if (i) res += " ";
            res += "(";
            res += elements[i].value->toHaskell(false);
            res += ")";
        }
    }
    // return "product" + res;
    return res;
}
bool ProductValue::equal(Value *v) const {
    auto* pv = dynamic_cast<ProductValue*>(v);
    if (!pv || pv->elements.size() != elements.size()) return false;
    for (int i = 0; i < elements.size(); ++i) {
        if (!(elements[i] == pv->elements[i])) return false;
    }
    return true;
}

SumValue::SumValue(int _id, const Data &_value, int _n): id(_id), value(_value), Value(), n(_n) {}
std::string SumValue::toString() const {
    return std::to_string(id) + "@" + value.toString();
}
std::string SumValue::toHaskell(bool in_result = false) const {
    // return "sum" + toString();
    return toString();
}
bool SumValue::equal(Value *v) const {
    auto* sv = dynamic_cast<SumValue*>(v);
    if (!sv) return false;
    return sv->id == id && sv->value == value;
}

ListValue::ListValue(const DataList &_value): value(_value), Value() {}
std::string ListValue::toString() const {
    return data::dataList2String(value);
}
std::string ListValue::toHaskell(bool in_result = false) const {
    // return "list" + toString();
    return toString();
}
bool ListValue::equal(Value *v) const {
    auto* dv = dynamic_cast<ListValue*>(v);
    if (!dv) return false;
    if (dv->value.size() != value.size()) return false;
    for (int i = 0; i < value.size(); ++i)
        if (!(value[i] == dv->value[i])) return false;
    return true;
}

BTreeValue::BTreeValue(): Value() {}

BTreeInternalValue::BTreeInternalValue(const BTreeNode &_l, const BTreeNode &_r, const Data &_v): l(_l), r(_r), value(_v) {}
std::string BTreeInternalValue::toString() const {
    return "(" + l->toString() + "," + value.toString() + "," + r->toString() + ")";
}
std::string BTreeInternalValue::toHaskell(bool in_result = false) const {
    return toString();
}
bool BTreeInternalValue::equal(Value *v) const {
    auto* iv = dynamic_cast<BTreeInternalValue*>(v);
    if (!iv) return false;
    return iv->value == value && iv->l->equal(l.get()) && iv->r->equal(r.get());
}

BTreeLeafValue::BTreeLeafValue(const Data &_v): value(_v) {}
std::string BTreeLeafValue::toString() const {
    return value.toString();
}
std::string BTreeLeafValue::toHaskell(bool in_result = false) const {
    return toString();
}
bool BTreeLeafValue::equal(Value *v) const {
    auto* lv = dynamic_cast<BTreeLeafValue*>(v);
    if (!lv) return false;
    return lv->value == value;
}

Data ext::ho::buildList(const DataList &content) {
    return Data(std::make_shared<ListValue>(content));
}
Data ext::ho::buildProduct(const DataList &elements) {
    return Data(std::make_shared<ProductValue>(elements));
}
Data ext::ho::buildSum(int id, const Data &value) {
    return Data(std::make_shared<SumValue>(id, value));
}

DeepCoderValueTypeInfo::DeepCoderValueTypeInfo(TypeExtension *_ext): ext(_ext) {
}
bool DeepCoderValueTypeInfo::isMatch(Value *value) {
    return dynamic_cast<ProductValue*>(value) || dynamic_cast<ListValue*>(value) ||
           dynamic_cast<BTreeValue*>(value) || dynamic_cast<SumValue*>(value);
}
PType DeepCoderValueTypeInfo::getType(Value *value) {
    auto* pv = dynamic_cast<ProductValue*>(value);
    if (pv) {
        TypeList sub_types;
        for (const auto& d: pv->elements) {
            auto sub_type = ext->getType(d.get());
            if (!sub_type) return nullptr;
            sub_types.push_back(sub_type);
        }
        return std::make_shared<TProduct>(type::assignVarName(sub_types));
    }
    auto* sv = dynamic_cast<SumValue*>(value);
    if (sv) {
        if (sv->n == -1) return type::getTVarA();
        TypeList sub_types(sv->n);
        for (int i = 0; i < sub_types.size(); ++i) {
            if (i == sv->id) {
                auto sub_type = ext->getType(sv->value.get());
                if (!sub_type) return nullptr;
                sub_types.push_back(sub_type);
            } else sub_types[i] = type::getTVarA();
        }
        return std::make_shared<TSum>(type::assignVarName(sub_types));
    }
    auto* lv = dynamic_cast<ListValue*>(value);
    if (lv) {
        PType res = type::getTVarA();
        for (const auto& d: lv->value) {
            auto sub_type = ext->getType(d.get());
            if (!sub_type) return nullptr;
            res = ext->join(res, ext->getType(d.get()));
        }
        return res;
    }
    auto* tiv = dynamic_cast<BTreeInternalValue*>(value);
    if (tiv) {
        auto l_type = ext->getType(tiv->l.get());
        auto r_type = ext->getType(tiv->r.get());
        auto v_type = ext->getType(tiv->value.get());
        if (!l_type || !r_type || !v_type) return nullptr;
        auto t1 = ext->join(l_type, r_type);
        if (!t1) return nullptr;
        auto t2 = std::make_shared<TBTree>(v_type, type::getTVarA());
        return ext->join(t1, t2);
    }
    auto* tlv = dynamic_cast<BTreeLeafValue*>(value);
    if (tlv) {
        auto l_type = ext->getType(tlv->value.get());
        return std::make_shared<TBTree>(type::getTVarA(), l_type);
    }
    assert(0);
}