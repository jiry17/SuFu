//
// Created by pro on 2021/12/28.
//

#ifndef ISTOOL_STRING_TYPE_H
#define ISTOOL_STRING_TYPE_H

#include "istool/basic/type.h"


class TString: public SimpleType {
public:
    virtual std::string getName();
    virtual PType clone(const TypeList& type_list);
    virtual std::string getHaskellName();
    virtual ~TString() = default;
};

namespace theory {
    namespace string {
        PType getTString();
    }
}


#endif //ISTOOL_STRING_TYPE_H
