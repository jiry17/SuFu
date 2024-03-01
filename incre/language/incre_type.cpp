//
// Created by pro on 2022/9/15.
//

#include "istool/incre/language/incre_type.h"
#include "glog/logging.h"

using namespace incre;

TyData::TyData(TyType _type): type(_type) {}
TyType TyData::getType() const {
    return type;
}

TyInt::TyInt(): TyData(TyType::INT) {}
std::string TyInt::toString() const {return "Int";}

TyBool::TyBool(): TyData(TyType::BOOL) {}
std::string TyBool::toString() const {return "Bool";}

TyUnit::TyUnit(): TyData(TyType::UNIT) {}
std::string TyUnit::toString() const {return "Unit";}

TyTuple::TyTuple(const TyList &_fields): TyData(TyType::TUPLE), fields(_fields) {}
std::string TyTuple::toString() const {
    std::string res = "{";
    for (int i = 0; i < fields.size(); ++i) {
        if (i) res += ","; res += fields[i]->toString();
    }
    return res + "}";
}

TyInductive::TyInductive(const std::string &_name, const std::vector<std::pair<std::string, Ty> > &_constructors):
    TyData(TyType::IND), name(_name), constructors(_constructors) {
}
std::string TyInductive::toString() const {
    std::string res = name + ". <";
    for (int i = 0; i < constructors.size(); ++i) {
        auto [name, ty] = constructors[i];
        if (i) res += " | ";
        res += name + " " + ty->toString();
    }
    return res + ">";
}

TyVar::TyVar(const std::string &_name): TyData(TyType::VAR), name(_name) {}
std::string TyVar::toString() const {return name;}

TyCompress::TyCompress(const Ty &_content): TyData(TyType::COMPRESS), content(_content) {}
std::string TyCompress::toString() const {
    return "compress " + content->toString();
}

TyArrow::TyArrow(const Ty &_source, const Ty &_target): TyData(TyType::ARROW), source(_source), target(_target) {}
std::string TyArrow::toString() const {
    return "(" + source->toString() + ") -> (" + target->toString() + ")";
}

namespace {
#define SubstHead(tname) std::pair<Ty, bool> _subst(Ty ## tname* x, const Ty& _x, const std::string& name, const Ty& y)
#define SubstCase(tname) return _subst(dynamic_cast<Ty ## tname*>(x.get()), x, name, y)

    std::pair<Ty, bool> _subst(const Ty& x, const std::string& name, const Ty& y);

    SubstHead(Tuple) {
        bool flag = false; TyList fields;
        for (auto& field: x->fields) {
            auto res = _subst(field, name, y);
            flag |= res.second; fields.push_back(res.first);
        }
        if (flag) return {std::make_shared<TyTuple>(fields), true};
        return {_x, false};
    }
    SubstHead(Var) {
        if (x->name == name) return {y, true};
        return {_x, false};
    }
    SubstHead(Compress) {
        auto [res, flag] = _subst(x->content, name, y);
        if (flag) return {std::make_shared<TyCompress>(res), true};
        return {_x, false};
    }
    SubstHead(Arrow) {
        auto [res_source, flag_source] = _subst(x->source, name, y);
        auto [res_target, flag_target] = _subst(x->target, name, y);
        if (!flag_source && !flag_target) return {_x, true};
        return {std::make_shared<TyArrow>(res_source, res_target), false};
    }
    SubstHead(Inductive) {
        if (x->name == name) return {_x, false};
        std::vector<std::pair<std::string, Ty>> cons_list;
        bool flag = false;
        for (const auto& [cname, branch]: x->constructors) {
            auto [res, res_flag] = _subst(branch, name, y);
            flag |= res_flag;
            cons_list.emplace_back(cname, res);
        }
        if (!flag) return {_x, false};
        return {std::make_shared<TyInductive>(x->name, cons_list), true};
    }

    std::pair<Ty, bool> _subst(const Ty& x, const std::string& name, const Ty& y) {
        switch (x->getType()) {
            case TyType::BOOL:
            case TyType::INT:
            case TyType::UNIT: return {x, false};
            case TyType::TUPLE: SubstCase(Tuple);
            case TyType::VAR: SubstCase(Var);
            case TyType::COMPRESS: SubstCase(Compress);
            case TyType::ARROW: SubstCase(Arrow);
            case TyType::IND: SubstCase(Inductive);
        }
        LOG(FATAL) << "Unknown type " << x->toString();
    }
}

Ty incre::subst(const Ty &x, const std::string &name, const Ty &y) {
    return _subst(x, name, y).first;
}

Ty incre::getConstructor(const Ty &x, const std::string &name) {
    auto* it = dynamic_cast<TyInductive*>(x.get());
    if (!it) LOG(FATAL) << "Non-inductive type " << x->toString() << " does not have constructors";
    for (auto& [cname, ctype]: it->constructors) {
        if (cname == name) {
            return incre::subst(ctype, it->name, x);
        }
    }
    LOG(FATAL) << "Unknown constructor " << name << " for type " << x->toString();
}