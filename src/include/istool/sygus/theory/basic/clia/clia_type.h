//
// Created by pro on 2021/12/19.
//

#ifndef ISTOOL_CLIA_TYPE_H
#define ISTOOL_CLIA_TYPE_H

#include "istool/basic/data.h"

class TInt: public SimpleType {
public:
    virtual std::string getName();
    virtual PType clone(const TypeList& params);
};

namespace theory {
    namespace clia {
        PType getTInt();
    }
}

#endif //ISTOOL_CLIA_TYPE_H
