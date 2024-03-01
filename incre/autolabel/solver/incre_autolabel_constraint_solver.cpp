//
// Created by pro on 2023/1/27.
//

#include "istool/incre/autolabel/incre_autolabel_constraint_solver.h"
#include "glog/logging.h"
#include <iostream>

using namespace incre;
using namespace incre::autolabel;

autolabel::AutoLabelZ3Solver::AutoLabelZ3Solver(const IncreProgram &init_program, bool _is_scalar):
    AutoLabelSolver(init_program), ctx(new Z3Context()), is_scalar(_is_scalar) {
}

autolabel::AutoLabelZ3Solver::~AutoLabelZ3Solver() {
    delete ctx;
}

IncreProgram AutoLabelZ3Solver::label() {
    initZ3Context(init_program.get(), ctx, is_scalar);
    collectAlignConstraint(init_program.get(), ctx);

    z3::optimize solver(ctx->ctx);
    auto obj = collectMinimalAlignConstraint(init_program.get(), ctx);
    solver.add(ctx->cons_list);

    // Minimize the number of covered AST nodes
    solver.push();
    solver.minimize(obj);
    assert(solver.check() == z3::sat);
    auto first_staged_model = solver.get_model();
    solver.pop();
    auto min_obj = first_staged_model.eval(obj).get_numeral_int();
    solver.add(obj == min_obj);

    // 2nd keyword: minimize the number of inserted operators
    z3::expr op_num = ctx->ctx.int_val(0);
    for (auto& [_, op]: ctx->flip_map) {
        op_num = op_num + z3::ite(op, ctx->ctx.int_val(1), ctx->ctx.int_val(0));
    }
    for (auto& [_, op]: ctx->align_map) {
        op_num = op_num + z3::ite(op, ctx->ctx.int_val(1), ctx->ctx.int_val(0));
    }
    solver.minimize(op_num);
    assert(solver.check() == z3::sat);
    auto model = solver.get_model();
    /*for (auto& [term, free_var]: ctx->flip_map) {
        LOG(INFO) << "is free " << term->toString() << " " << model.eval(free_var).bool_value();
    }*/

    LOG(INFO) << "objective " << model.eval(obj);

    return constructLabel(init_program.get(), model, ctx);
}
