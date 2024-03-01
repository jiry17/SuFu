//
// Created by pro on 2022/9/25.
//

#ifndef ISTOOL_INCRE_SOLVER_H
#define ISTOOL_INCRE_SOLVER_H

#include "analysis/incre_instru_info.h"

namespace incre {

    struct IncreSolution {
        TyList compress_type_list;
        TermList align_list;
        TermList repr_list;
        IncreSolution(const TyList& _compress_type_list, const TermList& align_list, const TermList& repr_list);
        IncreSolution() = default;
        void print() const;
    };

    class IncreSolver {
    public:
        IncreInfo* info;
        IncreSolver(IncreInfo* _info);
        virtual IncreSolution solve() = 0;
        virtual ~IncreSolver() = default;
    };

    IncreProgram rewriteWithIncreSolution(ProgramData* program, const IncreSolution& solution, Env* env, bool is_mark = false);
}

#endif //ISTOOL_INCRE_SOLVER_H
