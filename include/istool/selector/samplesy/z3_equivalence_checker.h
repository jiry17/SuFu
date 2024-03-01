//
// Created by pro on 2022/5/11.
//

#ifndef ISTOOL_Z3_EQUIVALENCE_CHECKER_H
#define ISTOOL_Z3_EQUIVALENCE_CHECKER_H

#include "istool/selector/selector.h"
#include "different_program_generator.h"
#include "istool/solver/component/tree_encoder.h"
#include "istool/ext/z3/z3_extension.h"

class Z3GrammarEquivalenceChecker: public GrammarEquivalenceChecker {
public:
    Z3Extension* ext;
    TreeEncoder *encoder;
    z3::expr_vector x_cons_list, diff_cons_list, param_list;
    z3::expr x_res;
    int example_count = 0;
    Z3GrammarEquivalenceChecker(Grammar* grammar, Z3Extension* ext, const TypeList& _inp_types, const PProgram& inp_cons);
    virtual void addExample(const IOExample& example);
    virtual ProgramList getTwoDifferentPrograms();
    virtual ~Z3GrammarEquivalenceChecker();
};

/*class Z3BoolEquivalenceChecker: public EquivalenceChecker {
public:
    TreeEncoder* x;
    Z3Extension* ext;
    Z3GrammarEncoder* encoder;
    z3::expr_vector cons_list, diff_cons;
    z3::expr x_res;
    int example_count = 0;
    Z3BoolEquivalenceChecker(Grammar* grammar, Z3Extension* ext, const TypeList& _inp_types, const PProgram& inp_cons);
    virtual void addExample(const IOExample& example);
    virtual ProgramList getTwoDifferentPrograms();
    virtual ~Z3BoolEquivalenceChecker();
};*/

#endif //ISTOOL_Z3_EQUIVALENCE_CHECKER_H
