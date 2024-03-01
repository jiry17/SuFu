//
// Created by pro on 2021/12/27.
//

#ifndef ISTOOL_ENUM_H
#define ISTOOL_ENUM_H

#include "istool/basic/verifier.h"
#include "istool/basic/time_guard.h"

class Optimizer {
public:
    virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p) = 0;
    virtual void clear() = 0;
    virtual ~Optimizer() = default;
};

struct EnumConfig {
    TimeGuard* guard;
    Verifier* v;
    Optimizer* o;
    int size_limit = 1000000000;
    EnumConfig(Verifier* _v, Optimizer* _o, TimeGuard* _guard = nullptr);
};

namespace solver {
    FunctionContext enumerate(const std::vector<PSynthInfo>& info_list, const EnumConfig& c);
}

#endif //ISTOOL_ENUM_H
