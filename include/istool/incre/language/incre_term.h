//
// Created by pro on 2022/9/15.
//

#ifndef ISTOOL_INCRE_TERM_H
#define ISTOOL_INCRE_TERM_H

#include "incre_type.h"
#include "incre_pattern.h"
#include "istool/basic/data.h"
#include "istool/basic/env.h"

namespace incre {
    enum class TermType {
        VALUE, IF, VAR, LET, TUPLE, PROJ, ABS, APP, FIX, MATCH, LABEL, UNLABEL, ALIGN, WILDCARD
    };

    class TermData {
    public:
        TermType term_type;
        TermType getType() const;
        TermData(TermType _term_type);
        virtual std::string toString() const = 0;
        virtual ~TermData() = default;
    };

    typedef std::shared_ptr<TermData> Term;
    typedef std::vector<Term> TermList;

    class TmValue: public TermData {
    public:
        Data data;
        TmValue(const Data& data);
        virtual std::string toString() const;
        virtual ~TmValue() = default;
    };

    class TmIf: public TermData {
    public:
        Term c, t, f;
        TmIf(const Term& _c, const Term& _t, const Term& _f);
        virtual std::string toString() const;
        virtual ~TmIf() = default;
    };

    class TmVar: public TermData {
    public:
        std::string name;
        TmVar(const std::string& _name);
        virtual std::string toString() const;
        virtual ~TmVar() = default;
    };

    class TmLet: public TermData {
    public:
        std::string name;
        Term def, content;
        TmLet(const std::string& _name, const Term& _def, const Term& _content);
        virtual std::string toString() const;
        virtual ~TmLet() = default;
    };

    class TmTuple: public TermData {
    public:
        TermList fields;
        TmTuple(const TermList& fields);
        std::string toString() const;
        virtual ~TmTuple() = default;
    };

    class TmProj: public TermData {
    public:
        Term content;
        int id;
        TmProj(const Term& _content, int _id);
        std::string toString() const;
        virtual ~TmProj() = default;
    };

    class TmAbs: public TermData {
    public:
        std::string name;
        Ty type;
        Term content;
        TmAbs(const std::string& _name, const Ty& _type, const Term& _content);
        std::string toString() const;
        virtual ~TmAbs() = default;
    };

    class TmApp: public TermData {
    public:
        Term func, param;
        TmApp(const Term& _func, const Term& _param);
        std::string toString() const;
        virtual ~TmApp() = default;
    };

    class TmFix: public TermData {
    public:
        Term content;
        TmFix(const Term& _content);
        std::string toString() const;
        virtual ~TmFix() = default;
    };

    class TmMatch: public TermData {
    public:
        Term def;
        std::vector<std::pair<Pattern, Term>> cases;
        TmMatch(const Term& _def, const std::vector<std::pair<Pattern, Term>>& _cases);
        std::string toString() const;
        virtual ~TmMatch() = default;
    };

    class TmLabel: public TermData {
    public:
        Term content;
        TmLabel(const Term& _content);
        std::string toString() const;
        virtual ~TmLabel() = default;
    };

    class TmUnLabel: public TermData {
    public:
        Term content;
        TmUnLabel(const Term& _content);
        std::string toString() const;
        virtual ~TmUnLabel() = default;
    };

    class TmAlign: public TermData {
    public:
        Term content;
        TmAlign(const Term& _content);
        std::string toString() const;
        virtual ~TmAlign() = default;
    };

    void initBasicOperators(Env* env);
    Term getOperator(const std::string& name);
    bool isBasicOperator(const std::string& name);
    TermList getSubTerms(TermData* term);
    std::string termType2String(TermType type);

    Term replaceTerm(const Term& term, const std::function<Term(const Term&)>& replace_func);
}

#endif //ISTOOL_INCRE_TERM_H
