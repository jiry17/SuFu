//
// Created by pro on 2022/2/6.
//

#include "istool/selector/random_selector.h"
#include "glog/logging.h"

FiniteRandomSelector::FiniteRandomSelector(Specification *spec, GrammarEquivalenceChecker *_checker, DifferentProgramGenerator* _g):
    CompleteSelector(spec, _checker) , g(_g) {
    finite_io_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (!finite_io_space) {
        LOG(FATAL) << "FiniteRandomSelector supports only FiniteIOSpace";
    }
    env = spec->env.get();
}

void FiniteRandomSelector::addExample(const IOExample &example) {
    g->addExample(example);
}
Example FiniteRandomSelector::getNextExample(const PProgram &x, const PProgram &y) {
    std::vector<int> id_list(finite_io_space->example_space.size());
    for (int i = 0; i < id_list.size(); ++i) id_list[i] = i;
    auto info_x = semantics::buildSingleContext(finite_io_space->func_name, x);
    auto info_y = semantics::buildSingleContext(finite_io_space->func_name, y);
    std::shuffle(id_list.begin(), id_list.end(), env->random_engine);
    int best_id = 0;
    for (auto& id: id_list) {
        auto& example = finite_io_space->example_space[id];
        auto program_list = g->getDifferentProgram(finite_io_space->getIOExample(example), 2);
        if (program_list.size() == 1) continue;
        // if (finite_io_space->satisfyExample(info_x, example) && finite_io_space->satisfyExample(info_y, example)) continue;
        best_id = id;
    }
    return finite_io_space->example_space[best_id];
    assert(0);
}

void Z3RandomSelector::addExample(const IOExample &example) {
}
Z3RandomSelector::Z3RandomSelector(Specification *spec, GrammarEquivalenceChecker *_checker, Z3Verifier* _verifier, ExampleGenerator *_generator, int _KSampleNum):
    CompleteSelector(spec, _checker), generator(_generator), env(spec->env.get()), KSampleNum(_KSampleNum), verifier(_verifier) {
    io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "Z3RandomSelector requires IOExampleSpaace";
    }
}
Example Z3RandomSelector::getNextExample(const PProgram &x, const PProgram &y) {
    LOG(INFO) << "get next for " << x->toString() << " " << y->toString() << std::endl;
    int rem = KSampleNum;
    while (rem > 0) {
        auto example_list = generator->generateExamples(nullptr);
        for (auto& example: example_list) {
            auto io_example = io_space->getIOExample(example);
            // std::cout << example::ioExample2String(io_example) << std::endl;
            if (!(env->run(x.get(), io_example.first) == env->run(y.get(), io_example.first))) return example;
        }
        rem -= example_list.size();
    }
    Example res; auto name = spec->info_list[0]->name;
    if (!verifier->verify(semantics::buildSingleContext(name, x), &res)) return res;
    if (!verifier->verify(semantics::buildSingleContext(name, y), &res)) return res;
    LOG(FATAL) << "Both " << x->toString() << " and " << y->toString() << " are correct";
}
Z3RandomSelector::~Z3RandomSelector() noexcept {
    delete generator; delete verifier;
}