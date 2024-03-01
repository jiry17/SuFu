//
// Created by pro on 2021/12/7.
//

#include "istool/ext/z3/z3_verifier.h"
#include "istool/ext/z3/z3_extension.h"
#include <map>
#include "glog/logging.h"

Z3Verifier::Z3Verifier(Z3ExampleSpace *_example_space): example_space(_example_space), ext(_example_space->ext) {
}

z3::expr_vector Z3Verifier::getParamVector() {
    z3::expr_vector param_list(ext->ctx);
    for (int i = 0; i < example_space->type_list.size(); ++i) {
        param_list.push_back(ext->buildVar(example_space->type_list[i].get(), "Param" + std::to_string(i)));
    }
    return param_list;
}
void Z3Verifier::prepareZ3Solver(z3::solver &solver, const FunctionContext &info) {
    auto param_list = getParamVector();
    auto encode_res = ext->encodeZ3ExprForConsProgram(example_space->cons_program.get(), info, ext::z3::z3Vector2EncodeList(param_list));
    solver.add(!encode_res.res || !z3::mk_and(encode_res.cons_list));
}

void Z3Verifier::getExample(const z3::model &model, Example *counter_example) {
    if (!counter_example) return;
    z3::expr_vector param_list(ext->ctx);
    for (int i = 0; i < example_space->type_list.size(); ++i) {
        param_list.push_back(ext->buildVar(example_space->type_list[i].get(), "Param" + std::to_string(i)));
    }
    counter_example->clear();
    for (int i = 0; i < param_list.size(); ++i) {
        counter_example->push_back(ext->getValueFromModel(model, param_list[i], example_space->type_list[i].get()));
    }
}

bool Z3Verifier::verify(const FunctionContext &info, Example *counter_example) {
    z3::solver s(ext->ctx);
    prepareZ3Solver(s, info);
    auto res = s.check();
    if (res == z3::unsat) return true;
    if (res != z3::sat) {
        LOG(FATAL) << "Z3 failed with " << res;
    }
    auto model = s.get_model();
    getExample(model, counter_example);
    return false;
}