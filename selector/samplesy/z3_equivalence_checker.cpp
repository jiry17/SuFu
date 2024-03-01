//
// Created by pro on 2022/5/11.
//

#include "istool/selector/samplesy/z3_equivalence_checker.h"
#include <queue>
#include "glog/logging.h"

namespace {
    int _getGrammarDepth(NonTerminal* symbol, std::vector<int>& depth_list) {
        int ind = symbol->id;
        if (depth_list[ind] == -2) {
            LOG(FATAL) << "For Z3EquivalenceChecker, the grammar should be acyclic";
        }
        if (depth_list[ind] != -1) return depth_list[ind];
        depth_list[ind] = -2; int res = 0;
        for (auto* rule: symbol->rule_list) {
            for (auto* sub_symbol: rule->param_list) {
                res = std::max(res, _getGrammarDepth(sub_symbol, depth_list));
            }
        }
        return depth_list[ind] = res + 1;
    }

    int _getGrammarDepth(Grammar* grammar) {
        grammar->indexSymbol();
        std::vector<int> depth(grammar->symbol_list.size(), -1);
        return _getGrammarDepth(grammar->start, depth);
    }
}

Z3GrammarEquivalenceChecker::Z3GrammarEquivalenceChecker(Grammar *grammar, Z3Extension *_ext, const TypeList &inp_types,
                                                         const PProgram &extra_cons): x_cons_list(ext->ctx), diff_cons_list(ext->ctx), ext(_ext),
        x_res(_ext->ctx), param_list(ext->ctx) {
    int depth = _getGrammarDepth(grammar);
    encoder = new TreeEncoder(grammar, ext, depth);
    x_cons_list = encoder->encodeStructure("x");
    for (int i = 0; i < inp_types.size(); ++i) {
        param_list.push_back(ext->buildVar(inp_types[i].get(), "Param" + std::to_string(i)));
    }
    auto xs_encode = encoder->encodeExample(ext::z3::z3Vector2EncodeList(param_list), "x@v0");
    x_res = xs_encode.res;
    for (const auto& cons: xs_encode.cons_list) diff_cons_list.push_back(cons);
    auto extra_cons_res = ext->encodeZ3ExprForProgram(extra_cons.get(), ext::z3::z3Vector2EncodeList(param_list));
    diff_cons_list.push_back(extra_cons_res.res);
    for (const auto& cons: extra_cons_res.cons_list) diff_cons_list.push_back(cons);
}

void Z3GrammarEquivalenceChecker::addExample(const IOExample &example) {
    example_count += 1;
    z3::expr_vector inp_list(ext->ctx);
    for (const auto& data: example.first) inp_list.push_back(ext->buildConst(data));
    auto oup = ext->buildConst(example.second);
    auto xs_encode = encoder->encodeExample(ext::z3::z3Vector2EncodeList(inp_list), "x@v" + std::to_string(example_count));
    x_cons_list.push_back(xs_encode.res == oup);
    for (const auto& cons: xs_encode.cons_list) x_cons_list.push_back(cons);
}

ProgramList Z3GrammarEquivalenceChecker::getTwoDifferentPrograms() {
    z3::solver solver(ext->ctx);
    solver.add(x_cons_list);
    auto res = solver.check();
    assert(res == z3::sat);
    auto model = solver.get_model();
    PProgram x_program = encoder->programBuilder(model);
    solver.add(diff_cons_list);
    auto x_encode_res = ext->encodeZ3ExprForProgram(x_program.get(), ext::z3::z3Vector2EncodeList(param_list));
    solver.add(x_encode_res.cons_list);
    solver.add(x_res != x_encode_res.res);
    res = solver.check();
    if (res == z3::unsat) return {x_program};
    assert(res == z3::sat);
    model = solver.get_model();
    auto y_program = encoder->programBuilder(model);
    //std::cout << model.eval(ext->buildVar(new TInt(), "Param0")) << " " << model.eval(ext->buildVar(new TInt(), "Param1")) << std::endl;
    assert(x_program->toString() != y_program->toString());
    return {x_program, y_program};
}

Z3GrammarEquivalenceChecker::~Z3GrammarEquivalenceChecker() {
    delete encoder;
}

/*
Z3BoolEquivalenceChecker::Z3BoolEquivalenceChecker(Grammar *grammar, Z3Extension *_ext, const TypeList &inp_types, const PProgram &inp_cons):
    ext(_ext), cons_list(ext->ctx), x_res(ext->ctx), diff_cons(ext->ctx) {
    int depth = _getGrammarDepth(grammar);
    encoder = new TreeEncoder(grammar, ext, depth);
    cons_list = encoder->encodeStructure("x");
    z3::expr_vector param_list(ext->ctx);
    for (int i = 0; i < inp_types.size(); ++i) {
        param_list.push_back(ext->buildVar(inp_types[i].get(), "Param" + std::to_string(i)));
    }
    auto encode_res = encoder->encodeExample(ext::z3::z3Vector2EncodeList(param_list), "x@v0");
    diff_cons = encode_res.cons_list;
    x_res = encode_res.res;
    auto inp_encode_res = ext->encodeZ3ExprForProgram(inp_cons.get(), ext::z3::z3Vector2EncodeList(param_list));
    diff_cons.push_back(inp_encode_res.res);
    for (const auto& cons: inp_encode_res.cons_list) {
        diff_cons.push_back(cons);
    }
}*/