//
// Created by pro on 2022/1/7.
//

#include "istool/solver/polygen/polygen_unifier.h"
#include "glog/logging.h"
#include <algorithm>

PolyGenUnifier::PolyGenUnifier(Specification *spec, const PSynthInfo& info, const PBESolverBuilder &builder):
    Unifier(spec, info), solver(new PolyGenConditionSolver(spec, info, builder)) {
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "PolyGenTermSolver require the number of target programs to be 1";
    }
    io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "PolyGenTermSolver supports only IOExamples";
    }
}

PolyGenUnifier::~PolyGenUnifier() {
}

namespace {
    ProgramList _reorderTermList(const ProgramList& term_list, const IOExampleList& example_list, Env* env) {
        std::vector<std::pair<int, PProgram>> info_list;
        for (const auto& term: term_list) {
            int num = 0;
            for (const auto& example: example_list) {
                if (example::satisfyIOExample(term.get(), example, env)) ++num;
            }
            info_list.emplace_back(num, term);
        }
        std::sort(info_list.begin(), info_list.end());
        ProgramList result;
        for (auto& info: info_list) result.push_back(info.second);
        return result;
    }

    PProgram _buildIte(const ProgramList& term_list, const ProgramList& condition_list, Env* env) {
        int n = condition_list.size();
        auto res = term_list[n];
        auto semantics = env->getSemantics("ite");
        for (int i = n - 1; i >= 0; --i) {
            ProgramList sub_list = {condition_list[i], term_list[i], res};
            res = std::make_shared<Program>(semantics, sub_list);
        }
        return res;
    }
}

PProgram PolyGenUnifier::unify(const ProgramList &raw_term_list, const ExampleList &example_list, TimeGuard *guard) {
    IOExampleList io_example_list;
    for (const auto& example: example_list) {
        io_example_list.push_back(io_space->getIOExample(example));
    }
    ProgramList term_list = _reorderTermList(raw_term_list, io_example_list, spec->env.get());

    ProgramList condition_list;
    for (int term_id = 0; term_id + 1 < term_list.size(); ++term_id) {
        TimeCheck(guard);
        auto term = term_list[term_id];
        IOExampleList positive_list, negative_list, free_list;
        for (const auto& example: io_example_list) {
            if (!example::satisfyIOExample(term.get(), example, spec->env.get())) {
                negative_list.push_back(example);
            } else {
                bool is_free = false;
                for (int i = term_id + 1; i < term_list.size(); ++i) {
                    if (example::satisfyIOExample(term_list[i].get(), example, spec->env.get())) {
                        is_free = true; break;
                    }
                }
                if (is_free) free_list.push_back(example); else positive_list.push_back(example);
            }
        }

        PProgram res = solver->getCondition(term_list, positive_list, negative_list, guard);
        condition_list.push_back(res);

        io_example_list = negative_list;
        for (const auto& example: free_list) {
            if (!spec->env->run(res.get(), example.first).isTrue()) {
                io_example_list.push_back(example);
            }
        }
    }
    delete solver;
    return _buildIte(term_list, condition_list, spec->env.get());
}

const std::string solver::polygen::KIsUseTermName = "PolyGen@IsUseTerm";