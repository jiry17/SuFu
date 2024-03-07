//
// Created by pro on 2021/12/26.
//

#ifndef ISTOOL_SYGUS_H
#define ISTOOL_SYGUS_H

#include "parser/parser.h"
#include "theory/theory.h"
#include "istool/basic/verifier.h"

namespace sygus {
    Verifier* getVerifier(Specification* spec);
}


#endif //ISTOOL_SYGUS_H
