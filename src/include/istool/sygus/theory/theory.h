//
// Created by pro on 2021/12/7.
//

#ifndef ISTOOL_THEORY_H
#define ISTOOL_THEORY_H

#include "istool/basic/specification.h"
#include "istool/basic/env.h"

enum class TheoryToken {
    CLIA, BV, STRING, NONE
};

typedef std::function<void(Env*, TheoryToken)> TheoryLoader;

class SyGuSExtension: public Extension {
public:
    TheoryToken theory;
    std::string sygus_header;
    SyGuSExtension(TheoryToken _theory);
    ~SyGuSExtension() = default;
};

namespace sygus {
    std::string theoryToken2String(TheoryToken token);
    TheoryToken getSyGuSTheory(Env* env);
    void setTheory(Env* env, TheoryToken theory);
    void loadSyGuSTheories(Env* env, const TheoryLoader& loader);
    std::string getSyGuSHeader(Env* env);
    void setSyGuSHeader(Env* env, const std::string& header);
}

#endif //ISTOOL_THEORY_H
