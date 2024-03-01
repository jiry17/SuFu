//
// Created by pro on 2022/5/12.
//

#include "istool/selector/random/complete_random_semantics_selector.h"
#include "glog/logging.h"

FiniteCompleteRandomSemanticsSelector::FiniteCompleteRandomSemanticsSelector(Specification *spec,
                                                                             GrammarEquivalenceChecker *checker, LearnedScorer *scorer, DifferentProgramGenerator* _g, int _KExampleNum):
        CompleteSelector(spec, checker), BasicRandomSemanticsSelector(spec->env.get(), spec->info_list[0]->grammar, scorer), g(_g), KExampleNum(_KExampleNum) {
    fio_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (!fio_space) {
        LOG(FATAL) << "FiniteCompleteRandomSemanticsSelector require FiniteIOExampleSpace";
    }
    for (auto& example: fio_space->example_space) {
        io_example_list.push_back(fio_space->getIOExample(example));
    }
}
FiniteCompleteRandomSemanticsSelector::~FiniteCompleteRandomSemanticsSelector() {
    delete g;
}
void FiniteCompleteRandomSemanticsSelector::addExample(const IOExample &example) {
    addHistoryExample(example.first);
    checker->addExample(example);
    g->addExample(example);
}
Example FiniteCompleteRandomSemanticsSelector::getNextExample(const PProgram &x, const PProgram &y) {
    LOG(INFO) << x->toString() << std::endl;
    std::vector<int> id_list;
    ExampleList candidate_inp_list;
    for ( int i = 0; i < io_example_list.size(); ++i) {
        auto& io_example = io_example_list[i];
        auto program_list = g->getDifferentProgram(io_example, 2);
        if (program_list.size() == 1) continue;
        //if (env->run(x.get(), io_example.first) == env->run(y.get(), io_example.first)) continue;
        id_list.push_back(i);
    }
    if (id_list.size() > KExampleNum) {
        std::shuffle(id_list.begin(), id_list.end(), spec->env->random_engine);
        id_list.resize(KExampleNum);
    }
    for (int id: id_list) candidate_inp_list.push_back(io_example_list[id].first);
    int best_id = getBestExampleId(x, candidate_inp_list);
    best_id = id_list[best_id];
    auto res = fio_space->example_space[best_id];
#ifdef DEBUG
    auto res_io = fio_space->getIOExample(res);
    // assert(!(env->run(x.get(), res_io.first) == env->run(y.get(), res_io.first)));
#endif
    return res;
}

Z3CompleteRandomSemanticsSelector::Z3CompleteRandomSemanticsSelector(Specification *spec, GrammarEquivalenceChecker *_checker, LearnedScorer *scorer,
                                                                     Program* example_cons, int _KExampleNum):
        ext(ext::z3::getExtension(spec->env.get())), CompleteSelector(spec, _checker), BasicRandomSemanticsSelector(spec->env.get(), spec->info_list[0]->grammar, scorer),
        KExampleNum(_KExampleNum), param_list(ext->ctx), inp_var_list(ext->ctx), cons_list(ext->ctx) {
    zio_space = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (!zio_space) {
        LOG(FATAL) << "Z3CompleteRandomSemanticsSelector requires Z3IOExampleSpace";
    }
    auto sig = zio_space->sig_map.begin()->second;
    for (int i = 0; i < zio_space->type_list.size(); ++i) {
        param_list.push_back(ext->buildVar(zio_space->type_list[i].get(), "Param" + std::to_string(i)));
    }
    for (int i = 0; i < sig.first.size(); ++i) {
        inp_var_list.push_back(ext->buildVar(sig.first[i].get(), "Input" + std::to_string(i)));
    }
    for (int i = 0; i < sig.first.size(); ++i) {
        auto p = zio_space->inp_list[i];
        auto var = inp_var_list[i];
        auto encode_res = ext->encodeZ3ExprForProgram(p.get(), ext::z3::z3Vector2EncodeList(param_list));
        for (const auto& cons: encode_res.cons_list) cons_list.push_back(cons);
        cons_list.push_back(encode_res.res == var);
    }
    auto encode_res = ext->encodeZ3ExprForProgram(example_cons, ext::z3::z3Vector2EncodeList(inp_var_list));
    for (const auto& cons: cons_list) encode_res.cons_list.push_back(cons);
    cons_list.push_back(encode_res.res);
}

void Z3CompleteRandomSemanticsSelector::addExample(const IOExample &example) {
    addHistoryExample(example.first);
}

Example Z3CompleteRandomSemanticsSelector::getNextExample(const PProgram &x, const PProgram &y) {
    z3::solver solver(ext->ctx);
    solver.add(cons_list);
    auto x_encode_res = ext->encodeZ3ExprForProgram(x.get(), ext::z3::z3Vector2EncodeList(inp_var_list));
    auto y_encode_res = ext->encodeZ3ExprForProgram(y.get(), ext::z3::z3Vector2EncodeList(inp_var_list));
    solver.add(x_encode_res.cons_list); solver.add(y_encode_res.cons_list);
    solver.add(x_encode_res.res != y_encode_res.res);
    ExampleList candidate_inp_list;
    ExampleList candidate_param_list;
    auto& sig = zio_space->sig_map.begin()->second;
    for (int _ = 0; _ < KExampleNum; ++_) {
        auto res = solver.check();
        if (res != z3::sat) break;
        auto model = solver.get_model();
        Example candidate_inp, candidate_param;
        for (int i = 0; i < param_list.size(); ++i) {
            candidate_param.push_back(ext->getValueFromModel(model, param_list[i], zio_space->type_list[i].get()));
        }
        for (int i = 0; i < inp_var_list.size(); ++i) {
            candidate_inp.push_back(ext->getValueFromModel(model, inp_var_list[i], sig.first[i].get()));
        }
        candidate_inp_list.push_back(candidate_inp);
        candidate_param_list.push_back(candidate_param);
        z3::expr_vector block_list(ext->ctx);
        for (int i = 0; i < param_list.size(); ++i) {
            block_list.push_back(param_list[i] != ext->buildConst(candidate_param[i]));
        }
        // std::cout << z3::mk_or(block_list) << std::endl;
        solver.add(z3::mk_or(block_list));
    }
    int best_id = getBestExampleId(x, candidate_inp_list);
    return candidate_param_list[best_id];
}