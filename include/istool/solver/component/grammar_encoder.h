//
// Created by pro on 2021/12/9.
//

#ifndef ISTOOL_GRAMMAR_ENCODER_H
#define ISTOOL_GRAMMAR_ENCODER_H

#include "istool/basic/grammar.h"
#include "istool/ext/z3/z3_extension.h"
#include <map>

class Z3GrammarEncoder {
public:
    Grammar* base;
    Z3Extension* ext;
    Z3GrammarEncoder(Grammar* _base, Z3Extension* _ext);
    virtual void enlarge() = 0;
    virtual z3::expr_vector encodeStructure(const std::string& prefix = "") = 0;
    virtual Z3EncodeRes encodeExample(const Z3EncodeList& inp_list, const std::string& prefix = "") const = 0;
    virtual PProgram programBuilder(const z3::model& model) const = 0;
    virtual z3::expr getBlockCons(const z3::model& model) const = 0;
    virtual ~Z3GrammarEncoder() = default;
};

#endif //ISTOOL_GRAMMAR_ENCODER_H
