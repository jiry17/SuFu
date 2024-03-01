//
// Created by pro on 2022/1/12.
//

#include "istool/selector/split/z3_splitor.h"
#include "istool/selector/selector.h"
#include "istool/solver/enum/enum_util.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"
#include <iostream>

namespace {
    z3::context& _getCtx(Env* env) {
        auto* ext = ext::z3::getExtension(env);
        return ext->ctx;
    }
}

Z3Splitor::Z3Splitor(ExampleSpace *_example_space, const PType &_oup_type, const TypeList &_inp_type_list):
    Splitor(_example_space), env(_example_space->env), oup_type(_oup_type), inp_type_list(_inp_type_list),
    inp_list(_getCtx(_example_space->env)), param_list(_getCtx(_example_space->env)) {
    io_space = dynamic_cast<Z3IOExampleSpace*>(example_space);
    if (!io_space) {
        LOG(FATAL) << "Z3Splitor requires Z3IOExampleSpace";
    }
    ext = ext::z3::getExtension(env);
    for (int i = 0; i < io_space->type_list.size(); ++i) {
        param_list.push_back(ext->buildVar(io_space->type_list[i].get(), "Param" + std::to_string(i)));
    }
    for (int i = 0; i < io_space->inp_list.size(); ++i) {
        auto encode_res = ext->encodeZ3ExprForProgram(io_space->inp_list[i].get(), ext::z3::z3Vector2EncodeList(param_list));
        assert(encode_res.cons_list.empty());
        inp_list.push_back(encode_res.res);
    }
}

namespace {
    std::string _getFeature(Program* x, Program* y) {
        auto sx = x->toString(), sy = y->toString();
        if (sx > sy) std::swap(sx, sy);
        return sx + "@" + sy;
    }
}

bool Z3Splitor::getExample(z3::solver &s, const ProgramList &seed_list, Example *counter_example, TimeGuard *guard) {
    auto z3_res = s.check();
    if (z3_res == z3::unsat) return true;
    if (z3_res != z3::sat) LOG(FATAL) << "Z3 failed with result " << z3_res;
    if (!counter_example) return false;
    z3::model model = s.get_model();


    LOG(INFO) << seed_list.size() << std::endl;
    z3::expr_vector seed_oup_list(ext->ctx);
    for (int i = 0; i < seed_list.size(); ++i) {
        auto oup_var = ext->buildVar(oup_type.get(), "Output" + std::to_string(i));
        auto encode_res = ext->encodeZ3ExprForProgram(seed_list[i].get(), ext::z3::z3Vector2EncodeList(inp_list));
        seed_oup_list.push_back(oup_var); s.add(oup_var == encode_res.res);
        assert(encode_res.cons_list.empty());
    }

    int size = std::min(int(seed_oup_list.size()), 100);
    seed_oup_list.resize(size);

    auto get_cost = [&](const Example& example) -> int{
        std::unordered_map<std::string, int> feature_map;
        int cost = 0;
        for (int i = 0; i < size; ++i) {
            auto w = env->run(seed_list[i].get(), example).toString();
            cost = std::max(cost, ++feature_map[w]);
        }
        return cost;
    };

    auto get_example = [&]()->Example {
        Example example;
        for (int i = 0; i < param_list.size(); ++i) {
            example.push_back(ext->getValueFromModel(model, param_list[i], io_space->type_list[i].get(), false));
        }
        return example;
    };

    auto eq_size = ext->ctx.int_const("eq_size");
    auto best_example = get_example(); int best_score = get_cost(best_example);
    for (int lim = 1;; lim = std::min(size, lim * 2 + 1)) {
        s.push();
        for (int i = 0; i < lim; ++i) {
            z3::expr_vector eq_expr_list(ext->ctx);
            eq_expr_list.push_back(ext->ctx.int_val(0));
            for (int j = i + 1; j < seed_oup_list.size(); ++j) {
                eq_expr_list.push_back(
                        z3::ite(seed_oup_list[i] == seed_oup_list[j], ext->ctx.int_val(1), ext->ctx.int_val(0)));
            }
            s.add(z3::sum(eq_expr_list) <= eq_size);
        }

        int l = 0, r = lim, ans = -1;
        while (l < r) {
            int mid = l + r >> 1;
            ext->setTimeOut(s, guard);
            s.push();
            s.add(eq_size <= ext->ctx.int_val(mid));
            z3_res = s.check();
            if (z3_res == z3::sat) {
                model = s.get_model();
                auto example = get_example();
                int current_cost = get_cost(example);
                if (current_cost < best_score) {
                    best_score = current_cost;
                    best_example = example;
                }
                r = mid;
            } else {
                s.pop();
                l = mid + 1;
            }
        }
        s.pop();
        if (lim == size || guard->getRemainTime() < 0) break;
    }
    *counter_example = best_example;
    return false;
}

namespace {
    z3::expr _getInvalidCons(Z3Extension* ext, const z3::expr_vector& param_list, const z3::expr& oup_val, Program* cons_program) {
        z3::expr_vector full_param_list(param_list.ctx());
        for (const auto& expr: param_list) full_param_list.push_back(expr);
        full_param_list.push_back(oup_val);
        auto cons_res = ext->encodeZ3ExprForProgram(cons_program, ext::z3::z3Vector2EncodeList(full_param_list));
        assert(cons_res.cons_list.empty());
        return !cons_res.res;
    }

    z3::expr _getOupCons(Z3Extension* ext, const z3::expr_vector& inp_list, const z3::expr& oup_val, Program* p) {
        auto oup_res = ext->encodeZ3ExprForProgram(p, ext::z3::z3Vector2EncodeList(inp_list));
        assert(oup_res.cons_list.empty());
        return oup_res.res == oup_val;
    }
}

bool Z3Splitor::getCounterExample(Program *p, const ProgramList &seed_list, Example *counter_example, TimeGuard *guard) {
    z3::solver solver(ext->ctx);
    auto p_oup_var = ext->buildVar(oup_type.get(), "p@output");
    solver.add(_getOupCons(ext, inp_list, p_oup_var, p));
    std::cout << io_space->oup_cons->toString() << " " << io_space->type_list.size() << std::endl;
    solver.add(_getInvalidCons(ext, param_list, p_oup_var, io_space->oup_cons.get()));
    return getExample(solver, seed_list, counter_example, guard);
}

bool Z3Splitor::getDistinguishExample(Program *x, Program *y, const ProgramList &seed_list, Example *counter_example, TimeGuard *guard) {
    z3::solver solver(ext->ctx);
    auto x_oup_var = ext->buildVar(oup_type.get(), "x@output");
    auto y_oup_var = ext->buildVar(oup_type.get(), "y@output");
    solver.add(_getOupCons(ext, inp_list, x_oup_var, x));
    solver.add(_getOupCons(ext, inp_list, y_oup_var, y));
    solver.add(x_oup_var != y_oup_var);
    solver.add(_getInvalidCons(ext, param_list, x_oup_var, io_space->oup_cons.get()) ||
               _getInvalidCons(ext, param_list, y_oup_var, io_space->oup_cons.get()));
    return getExample(solver, seed_list, counter_example, guard);
}