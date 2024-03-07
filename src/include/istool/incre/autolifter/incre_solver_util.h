//
// Created by pro on 2023/7/18.
//

#ifndef ISTOOL_INCRE_SOLVER_UTIL_H
#define ISTOOL_INCRE_SOLVER_UTIL_H

#include "istool/invoker/invoker.h"
#include "istool/incre/language/incre.h"
#include "istool/incre/grammar/incre_component_collector.h"

namespace incre::autolifter::util {
    std::pair<SolverToken, InvokeConfig> getSolverToken(Type* oup_type);
    PProgram synthesis2Program(const TypeList& inp_type_list, const PType& oup_type, const PEnv& env, Grammar* grammar,
                               const IOExampleList& example_list);
    Term program2Term(Program* program, const incre::grammar::SynthesisComponentList& component_list, const TermList& term_list);
}

#endif //ISTOOL_INCRE_SOLVER_UTIL_H
