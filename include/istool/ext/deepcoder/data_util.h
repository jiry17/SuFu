//
// Created by pro on 2022/1/21.
//

#ifndef ISTOOL_DATA_UTIL_H
#define ISTOOL_DATA_UTIL_H

#include "istool/basic/program.h"

namespace ext::ho {
    Data polyFMap(Program* p, Type* type, const Data& data, Env* env);
    ProgramList splitProduct(const PProgram& p);
    bool isProductProgram(Program* p);
    PProgram removeAccessProd(const PProgram& p);
    PProgram buildAccess(const PProgram& p, const std::vector<int>& trace);
    PProgram buildProduct(const ProgramList& sub_list);
    PProgram mergeProduct(const ProgramList& p_list);
}

#endif //ISTOOL_DATA_UTIL_H
