//
// Created by pro on 2022/9/21.
//

#ifndef ISTOOL_INCRE_INSTRU_TYPE_H
#define ISTOOL_INCRE_INSTRU_TYPE_H

#include "istool/incre/language/incre.h"

namespace incre {
    class VLabeledCompress: public VCompress {
    public:
        int id;
        VLabeledCompress(const Data& _v, int _id);
        virtual std::string toString() const;
        virtual ~VLabeledCompress() = default;
    };

    class TyLabeledCompress: public TyCompress {
    public:
        int id;
        TyLabeledCompress(const Ty& _ty, int _id);
        virtual std::string toString() const;
        virtual ~TyLabeledCompress() = default;
    };

    class TmLabeledLabel: public TmLabel {
    public:
        int id;
        TmLabeledLabel(const Term& _content, int _id);
        virtual std::string toString() const;
        virtual ~TmLabeledLabel() = default;
    };

    class TmLabeledAlign: public TmAlign {
    public:
        int id;
        std::unordered_map<std::string, Data> subst_info;
        TmLabeledAlign(const Term& _content, int _id, const std::unordered_map<std::string, Data>& _subst_info={});
        virtual std::string toString() const;
        void addSubst(const std::string& name, const Data& v);
        virtual ~TmLabeledAlign() = default;
    };
}

#endif //ISTOOL_INCRE_INSTRU_TYPE_H
