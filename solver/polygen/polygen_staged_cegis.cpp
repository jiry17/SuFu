//
// Created by pro on 2022/1/8.
//

#include "istool/solver/polygen/polygen_cegis.h"
#include "istool/basic/semantics.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

StagedCEGISPolyGen::StagedCEGISPolyGen(Specification *spec, TermSolver *_term_solver,
                                       PolyGenConditionSolver *_cond_solver):
                                       Solver(spec), term_solver(_term_solver), cond_solver(_cond_solver), verify_pos(0) {
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "PolyGen can only synthesize a single program";
    }
    auto* io_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "PolyGen supports only FiniteIOExampleSpace";
    }
    for (auto& example: io_space->example_space) {
        example_list.push_back(io_space->getIOExample(example));
    }
}

StagedCEGISPolyGen::~StagedCEGISPolyGen() noexcept {
    delete cond_solver; delete term_solver;
}

int StagedCEGISPolyGen::verifyTerms(const ProgramList &term_list) {
    for (int _ = 0; _ < example_list.size(); ++_) {
        verify_pos = (verify_pos + 1) % example_list.size();
        auto& example = example_list[verify_pos];
        bool is_covered = false;
        for (auto& term: term_list) {
            if (example::satisfyIOExample(term.get(), example, spec->env.get())) {
                is_covered = true; break;
            }
        }
        if (!is_covered) return verify_pos;
    }
    return -1;
}

int StagedCEGISPolyGen::verifyProgram(const PProgram &program) {
    auto* env = spec->env.get();
    for (int _ = 0; _ < example_list.size(); ++_) {
        verify_pos = (verify_pos + 1) % example_list.size();
        auto& example = example_list[verify_pos];
        if (!example::satisfyIOExample(program.get(), example, spec->env.get())) {
            return verify_pos;
        }
    }
    return -1;
}

ProgramList StagedCEGISPolyGen::synthesisTerms(TimeGuard* guard) {
    auto start = grammar::getMinimalProgram(spec->info_list[0]->grammar);
    ProgramList term_list = {start};
    ExampleList counter_examples;
    auto* example_space = dynamic_cast<FiniteExampleSpace*>(spec->example_space.get());

    {
        int base_num = 100;
        std::vector<int> index_list;
        for (int i = 0; i < index_list.size(); ++i) index_list.push_back(i);
        std::shuffle(index_list.begin(), index_list.end(), spec->env->random_engine);
        for (int i = 0; i < index_list.size() && i < base_num; ++i) {
            counter_examples.push_back(example_space->example_space[index_list[i]]);
        }
    }
    while (1) {
        global::recorder.add("#term_cegis", 1);
        auto counter_example = verifyTerms(term_list);
        if (counter_example == -1) return term_list;
        counter_examples.push_back(example_space->example_space[counter_example]);
        term_list = term_solver->synthesisTerms(counter_examples, guard);
        LOG(INFO) << "New Terms";
        for (auto& term: term_list) {
            LOG(INFO) << "  " << term->toString();
        }
    }
}

PProgram StagedCEGISPolyGen::unify(const ProgramList &term_list, TimeGuard *guard) {
    std::vector<PProgram> cond_list(int(term_list.size()) - 1, program::buildConst(BuildData(Bool, true)));
    IOExampleList counter_examples;
    while (1) {
        global::recorder.add("#term_cegis", 1);
        auto current = solver::polygen::constructDecisionList(term_list, cond_list);
        auto counter_example = verifyProgram(current);
        if (counter_example == -1) return current;

        counter_examples.push_back(example_list[counter_example]);
        cond_list = solver::polygen::updateConditionList(term_list, cond_list, counter_examples, cond_solver, guard);
    }
}

FunctionContext StagedCEGISPolyGen::synthesis(TimeGuard *guard) {
    auto term_list = synthesisTerms(guard);
    auto res = unify(term_list, guard);
    return semantics::buildSingleContext(spec->info_list[0]->name, res);
}