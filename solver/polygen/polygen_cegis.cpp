//
// Created by pro on 2022/1/8.
//

#include "istool/solver/polygen/polygen_cegis.h"
#include "istool/basic/semantics.h"
#include "glog/logging.h"
#include "istool/sygus/theory/basic/clia/clia_semantics.h"

CEGISPolyGen::~CEGISPolyGen() {
    delete term_solver;
    delete cond_solver;
}

CEGISPolyGen::CEGISPolyGen(Specification *spec, TermSolver *_term_solver, PolyGenConditionSolver *_condition_solver, Verifier *_v):
        VerifiedSolver(spec, _v), term_solver(_term_solver), cond_solver(_condition_solver) {
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "PolyGen can only synthesize a single program";
    }
    io_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "PolyGen supports only IOExamples";
    }
}

PProgram solver::polygen::constructDecisionList(const ProgramList &term_list, const ProgramList& cond_list) {
    auto n = cond_list.size(); assert(term_list.size() == n + 1);
    auto res = term_list[n];
    auto ite_semantics = std::make_shared<IteSemantics>();
    for (int i = n - 1; i >= 0; --i) {
        ProgramList sub_list = {cond_list[i], term_list[i], res};
        res = std::make_shared<Program>(ite_semantics, sub_list);
    }
    return res;
}

namespace {
    PProgram _handleSemanticsErrorInMid(const PProgram& c, const IOExampleList& example_list, Env* env) {
        bool is_exist_error = false;
        for (auto& example: example_list) {
            try {
                env->run(c.get(), example.first);
            } catch (SemanticsError& e) {
                is_exist_error = true;
                break;
            }
        }
        if (!is_exist_error) return c;
        return std::make_shared<Program>(std::make_shared<AllowFailSemantics>(type::getTBool(), BuildData(Bool, true)), (ProgramList){c});
    }
}

ProgramList solver::polygen::updateConditionList(const ProgramList &term_list, const ProgramList &cond_list,
                                                 const IOExampleList& examples, PolyGenConditionSolver *solver,
                                                 TimeGuard* guard) {
    IOExampleList rem_example = examples; auto* env = solver->spec->env.get();
    ProgramList new_cond_list = cond_list;
    for (int i = 0; i + 1 < term_list.size(); ++i) {
        IOExampleList positive_list, negative_list, mid_list;
        auto current_term = term_list[i];
        for (const auto& current_example: rem_example) {
            if (example::satisfyIOExample(current_term.get(), current_example, env)) {
                bool is_mid = false;
                for (int j = i + 1; j < term_list.size(); ++j) {
                    if (example::satisfyIOExample(term_list[j].get(), current_example, env)) {
                        is_mid = true;
                        break;
                    }
                }
                if (is_mid) {
                    mid_list.push_back(current_example);
                } else positive_list.push_back(current_example);
            } else negative_list.push_back(current_example);
        }

        auto condition = new_cond_list[i];
        bool is_valid = true;
        for (const auto& example: positive_list) {
            try {
                if (!env->run(condition.get(), example.first).isTrue()) {
                    is_valid = false;
                    break;
                }
            } catch (SemanticsError& e) {
                is_valid = false;
            }
        }
        if (is_valid) {
            try {
                for (const auto &example: negative_list) {
                    if (env->run(condition.get(), example.first).isTrue()) {
                        is_valid = false;
                        break;
                    }
                }
            } catch (SemanticsError& e) {
                is_valid = false;
            }
        }

        if (!is_valid) {
            condition = solver->getCondition(term_list, positive_list, negative_list, guard);
        }
        condition = _handleSemanticsErrorInMid(condition, mid_list, env);

        new_cond_list[i] = condition;
        rem_example = negative_list;
        for (const auto& example: mid_list) {
            if (!env->run(condition.get(), example.first).isTrue()) {
                rem_example.push_back(example);
            }
        }
    }
    return new_cond_list;
}

FunctionContext CEGISPolyGen::synthesis(TimeGuard *guard) {
    // LOG(INFO) << "start";
    auto info = spec->info_list[0];
    auto start = grammar::getMinimalProgram(info->grammar);
    ExampleList example_list;
    IOExampleList io_example_list;
    Example counter_example;
    auto result = semantics::buildSingleContext(info->name, start);
   // LOG(INFO) << "synthesis " << result.toString();
    if (v->verify(result, &counter_example)) {
        return result;
    }
    example_list.push_back(counter_example);
    io_example_list.push_back(io_space->getIOExample(counter_example));
    ProgramList term_list, condition_list;
    auto* env = spec->env.get();

    while (true) {
        TimeCheck(guard);
        auto last_example = example_list[example_list.size() - 1];
        auto last_io_example = io_example_list[example_list.size() - 1];
        LOG(INFO) << "counter example " << example::ioExample2String(last_io_example);
        bool is_occur = false;
        for (const auto& term: term_list) {
            if (example::satisfyIOExample(term.get(), last_io_example, env)) {
                is_occur = true; break;
            }
        }
        if (!is_occur) {
            LOG(INFO) << "new term";
            /*for (auto& example: example_list) {
                LOG(INFO) << "term example " << data::dataList2String(example);
            }*/
            //LOG(INFO) << term_solver;
            for (auto& term: term_list) {
                LOG(INFO) << "  " << term->toString();
            }
            auto new_term = term_solver->synthesisTerms(example_list, guard);
            //for (const auto& p: new_term) std::cout << "  " << p->toString() << std::endl;

            ProgramList new_condition;
            for (int id = 0; id + 1 < new_term.size(); ++id) {
                auto term = new_term[id];
                PProgram cond;
                for (int i = 0; i + 1 < term_list.size(); ++i) {
                    if (term->toString() == term_list[i]->toString()) {
                        cond = condition_list[i];
                        break;
                    }
                }
                if (!cond) {
                    cond = program::buildConst(BuildData(Bool, true));
                }
                new_condition.push_back(cond);
            }
            term_list = new_term;
            condition_list = new_condition;
        }

        condition_list = solver::polygen::updateConditionList(term_list, condition_list, io_example_list, cond_solver, guard);

        auto merge = solver::polygen::constructDecisionList(term_list, condition_list);
        LOG(INFO) << "current " << merge->toString();
        result = semantics::buildSingleContext(info->name, merge);
        if (v->verify(result, &counter_example)) {
            return result;
        }
        //LOG(INFO) << "Counter Example " << example::ioExample2String(io_space->getIOExample(counter_example));
        example_list.push_back(counter_example);
        io_example_list.push_back(io_space->getIOExample(counter_example));
    }
}