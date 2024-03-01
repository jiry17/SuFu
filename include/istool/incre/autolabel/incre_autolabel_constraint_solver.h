//
// Created by pro on 2023/1/27.
//

#ifndef ISTOOL_INCRE_AUTOLABEL_CONSTRAINT_SOLVER_H
#define ISTOOL_INCRE_AUTOLABEL_CONSTRAINT_SOLVER_H

#include "istool/incre/autolabel/incre_autolabel.h"
#include "z3++.h"

namespace incre::autolabel {
    class TyZ3LabeledCompress: public TyCompress {
    public:
        z3::expr label;
        TyZ3LabeledCompress(const Ty& content, const z3::expr& _label);
        virtual std::string toString() const;
        ~TyZ3LabeledCompress() = default;
    };

    class Z3Context: public TypeContext {
    public:
        z3::context ctx;
        z3::expr_vector cons_list;
        std::unordered_map<TermData*, Ty> type_map;
        std::unordered_map<TermData*, z3::expr> flip_map, align_map, free_map, obj_map;
        int tmp_id;

        Z3Context();
        z3::expr getVar();
        void addCons(const z3::expr& cons);
        virtual ~Z3Context() = default;
    };

    Ty unfoldTypeWithZ3Label(const Ty& type, TypeContext* ctx);
    void initZ3Context(ProgramData* init_program, Z3Context* ctx, bool is_scalar_only);
    void collectAlignConstraint(ProgramData* program, Z3Context* ctx);
    z3::expr collectMinimalAlignConstraint(ProgramData* program, Z3Context* ctx);
    IncreProgram constructLabel(ProgramData* program, const z3::model& model, Z3Context* ctx);

    class AutoLabelZ3Solver: public AutoLabelSolver {
    public:
        Z3Context* ctx;
        bool is_scalar;
        AutoLabelZ3Solver(const IncreProgram& init_program, bool _is_scalar);
        virtual IncreProgram label();
        ~AutoLabelZ3Solver();
    };

}
#endif //ISTOOL_INCRE_AUTOLABEL_CONSTRAINT_SOLVER_H
