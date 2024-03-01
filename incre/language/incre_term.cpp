//
// Created by pro on 2022/9/15.
//

#include "istool/incre/language/incre_term.h"
#include "glog/logging.h"

using namespace incre;

TermData::TermData(TermType _term_type): term_type(_term_type) {}
TermType TermData::getType() const {return term_type;}

TmValue::TmValue(const Data &_data): TermData(TermType::VALUE), data(_data) {}
std::string TmValue::toString() const {
    return data.toString();
}

TmIf::TmIf(const Term &_c, const Term &_t, const Term &_f): TermData(TermType::IF), c(_c), t(_t), f(_f) {}
std::string TmIf::toString() const {
    return "if " + c->toString() + " then " + t->toString() + " else " + f->toString();
}

TmVar::TmVar(const std::string &_name): TermData(TermType::VAR), name(_name) {}
std::string TmVar::toString() const {
    return name;
}

TmLet::TmLet(const std::string &_name, const Term &_def, const Term &_content): TermData(TermType::LET), name(_name), def(_def), content(_content) {
}
std::string TmLet::toString() const {
    return "let " + name + " = " + def->toString() + " in " + content->toString();
}

TmTuple::TmTuple(const TermList &_fields): TermData(TermType::TUPLE), fields(_fields) {}
std::string TmTuple::toString() const {
    std::string res = "{";
    for (int i = 0; i < fields.size(); ++i) {
        if (i) res += ",";
        res += fields[i]->toString();
    }
    return res + "}";
}

TmProj::TmProj(const Term &_content, int _id): TermData(TermType::PROJ), content(_content), id(_id) {}
std::string TmProj::toString() const {
    return content->toString() + "." + std::to_string(id);
}

TmAbs::TmAbs(const std::string &_name, const Ty &_type, const Term &_content): TermData(TermType::ABS),
    name(_name), type(_type), content(_content) {
}
std::string TmAbs::toString() const {
    return "lambda " + name + ": " + type->toString() + "." + content->toString();
}

TmApp::TmApp(const Term &_func, const Term &_param): TermData(TermType::APP), func(_func), param(_param) {
}
std::string TmApp::toString() const {
    return func->toString() + " (" +  param->toString() + ")";
}

TmFix::TmFix(const Term &_content): TermData(TermType::FIX), content(_content) {}
std::string TmFix::toString() const {
    return "fix " + content->toString();
}

TmMatch::TmMatch(const Term &_def, const std::vector<std::pair<Pattern, Term> > &_cases):
    TermData(TermType::MATCH), def(_def), cases(_cases) {
}
std::string TmMatch::toString() const {
    std::string res = "match " + def->toString() + " with ";
    for (int i = 0; i < cases.size(); ++i) {
        if (i) res += " | ";
        auto [pt, term] = cases[i];
        res += pt->toString() + " -> " + term->toString();
    }
    return res;
}

TmLabel::TmLabel(const Term &_content): TermData(TermType::LABEL), content(_content) {}
std::string TmLabel::toString() const {
    return "label " + content->toString();
}

TmUnLabel::TmUnLabel(const Term &_content): TermData(TermType::UNLABEL), content(_content) {}
std::string TmUnLabel::toString() const {
    return "unlabel " + content->toString();
}

TmAlign::TmAlign(const Term &_content): TermData(TermType::ALIGN), content(_content) {}
std::string TmAlign::toString() const {
    return "align " + content->toString();
}

namespace {
#define ReplaceHead(name) Term _replaceTerm(Tm ## name* term, const Term& _term, const std::function<Term(const Term&)>& replace_func)
#define ReplaceCase(name) return _replaceTerm(dynamic_cast<Tm ## name*>(term.get()), term, replace_func)
#define ReplaceSub(name) auto name = replaceTerm(term->name, replace_func)
    ReplaceHead(Abs) {
        ReplaceSub(content);
        return std::make_shared<TmAbs>(term->name, term->type, content);
    }
    ReplaceHead(App) {
        ReplaceSub(func); ReplaceSub(param);
        return std::make_shared<TmApp>(func, param);
    }
    ReplaceHead(Match) {
        ReplaceSub(def); std::vector<std::pair<Pattern, Term>> cases;
        for (auto& [pt, sub_term]: term->cases) {
            cases.emplace_back(pt, replaceTerm(sub_term, replace_func));
        }
        return std::make_shared<TmMatch>(def, cases);
    }
    ReplaceHead(If) {
        ReplaceSub(c); ReplaceSub(t); ReplaceSub(f);
        return std::make_shared<TmIf>(c, t, f);
    }
    ReplaceHead(Fix) {
        ReplaceSub(content);
        return std::make_shared<TmFix>(content);
    }
    ReplaceHead(Align) {
        ReplaceSub(content);
        return std::make_shared<TmAlign>(content);
    }
    ReplaceHead(Label) {
        ReplaceSub(content);
        return std::make_shared<TmLabel>(content);
    }
    ReplaceHead(UnLabel) {
        ReplaceSub(content);
        return std::make_shared<TmUnLabel>(content);
    }
    ReplaceHead(Let) {
        ReplaceSub(def); ReplaceSub(content);
        return std::make_shared<TmLet>(term->name, def, content);
    }
    ReplaceHead(Tuple) {
        TermList fields;
        for (auto& field: term->fields) fields.push_back(replaceTerm(field, replace_func));
        return std::make_shared<TmTuple>(fields);
    }
    ReplaceHead(Proj) {
        ReplaceSub(content);
        return std::make_shared<TmProj>(content, term->id);
    }
}

Term incre::replaceTerm(const Term& term, const std::function<Term(const Term&)>& replace_func) {
    auto res = replace_func(term);
    if (res) return res;
    switch (term->getType()) {
        case TermType::VALUE:
        case TermType::VAR: return term;
        case TermType::ABS: ReplaceCase(Abs);
        case TermType::APP: ReplaceCase(App);
        case TermType::MATCH: ReplaceCase(Match);
        case TermType::ALIGN: ReplaceCase(Align);
        case TermType::LABEL: ReplaceCase(Label);
        case TermType::UNLABEL: ReplaceCase(UnLabel);
        case TermType::IF: ReplaceCase(If);
        case TermType::FIX: ReplaceCase(Fix);
        case TermType::LET: ReplaceCase(Let);
        case TermType::TUPLE: ReplaceCase(Tuple);
        case TermType::PROJ: ReplaceCase(Proj);
        case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << term->toString();
    }
}

std::string incre::termType2String(TermType type) {
    switch (type) {
        case TermType::VALUE: return "Value";
        case TermType::VAR: return "Var";
        case TermType::ABS: return "Abs";
        case TermType::APP: return "App";
        case TermType::MATCH: return "Match";
        case TermType::ALIGN: return "Align";
        case TermType::LABEL: return "Label";
        case TermType::UNLABEL: return "UnLabel";
        case TermType::IF: return "If";
        case TermType::FIX: return "Fix";
        case TermType::LET: return "Let";
        case TermType::TUPLE: return "Tuple";
        case TermType::PROJ: return "Proj";
        case TermType::WILDCARD: return "Wildcard";
    }
}