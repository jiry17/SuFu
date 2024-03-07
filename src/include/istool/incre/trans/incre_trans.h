//
// Created by pro on 2022/9/25.
//

#ifndef ISTOOL_TANS_H
#define ISTOOL_TANS_H

#include "istool/incre/language/incre.h"

namespace incre {

    class TIncreInductive: public SimpleType {
    public:
        Ty _type;
        TyInductive* type;
        virtual ~TIncreInductive() = default;
        virtual std::string getName();
        virtual PType clone(const TypeList &params);
        virtual bool equal(Type* t);
        virtual std::string getHaskellName();
        TIncreInductive(const Ty& _type);
    };

    class TCompress: public SimpleType {
    public:
        PType content;
        TCompress(const PType& _content);
        virtual std::string getName();
        virtual bool equal(Type* type);
        virtual std::string getBaseName();
        virtual TypeList getParams();
        virtual PType clone(const TypeList& params);
        virtual std::string getHaskellName();
    };

    class TLabeledCompress: public TCompress {
    public:
        int id;
        TLabeledCompress(int _id, const PType& _content);
        virtual std::string getName();
        virtual bool equal(Type* type);
    };

    PType typeFromIncre(const Ty& type);
    Ty typeToIncre(Type* type);
    Term termToIncre(Program* program, const TermList& inps);
}

#endif //ISTOOL_TANS_H
