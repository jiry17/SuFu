//
// Created by pro on 2022/5/8.
//

#include <cassert>
#include "istool/basic/specification.h"
#include "istool/invoker/invoker.h"
#include "istool/sygus/parser/parser.h"
#include "istool/selector/samplesy/samplesy.h"
#include "istool/selector/split/finite_splitor.h"
#include "istool/selector/split/z3_splitor.h"
#include "istool/dsl/samplesy/samplesy_dsl.h"
#include "istool/solver/vsa/vsa_builder.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/witness/string/string_witness.h"
#include "istool/sygus/theory/witness/clia/clia_witness.h"
#include <unordered_set>
#include "istool/selector/random_selector.h"
#include "istool/basic/config.h"
#include "istool/selector/samplesy/finite_equivalence_checker.h"
#include "istool/selector/samplesy/vsa_seed_generator.h"
#include "istool/selector/samplesy/different_program_generator.h"
#include "istool/selector/samplesy/z3_equivalence_checker.h"
#include "istool/selector/random/random_semantics_selector.h"
#include "istool/selector/random/learned_scorer.h"
#include "istool/selector/random/complete_random_semantics_selector.h"
#include "istool/sygus/theory/basic/clia/clia_example_sampler.h"
#include "istool/ext/vsa/vsa_inside_checker.h"
#include "istool/basic/config.h"

typedef std::pair<int, FunctionContext> SynthesisResult;

const int KIntMax = 5;

std::string getTaskName(const std::string& path) {
    int last = 0;
    for (auto i = path.find("/"); i != std::string::npos; i = path.find("/", i + 1)) last = i;
    return path.substr(last + 1);
}

// The depth is the same as the evaluation in PLDI20 "Question Selection for Interactive Program Synthesis"
const std::unordered_map<std::string, int> KSpecialDepth = {
        {"t8.sl", 2}, {"t10.sl", 3}, {"t17.sl", 3}, {"t20.sl", 3}, {"t3.sl", 3}, {"t7.sl", 3},
        {"dr-name.sl", 7}, {"dr-name-long.sl", 7}, {"dr-name-long-repeat.sl", 7}, {"dr-name_small.sl", 7},
        {"2171308.sl", 7}, {"exceljet1.sl", 7}, {"get-domain-name-from-url.sl", 7}, {"get-last-name-from-name-with-comma.sl", 8},
        {"initials.sl", 8}, {"initials-long.sl", 8}, {"initials-long-repeat.sl", 8}, {"initials_small.sl", 8},
        {"stackoverflow10.sl", 8}, {"stackoverflow11.sl", 7}
};
int KDefaultStringDepth;
int getGrammarDepth(Specification* spec, const std::string& task_name) {
    auto it = KSpecialDepth.find(task_name);
    if (it != KSpecialDepth.end()) {
        return it->second;
    }
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) return KDefaultStringDepth;
    else if (theory == TheoryToken::CLIA) return 4;
    LOG(FATAL) << "GrammarDepth undefined for the current theory";
}

namespace {
    void _collectIntParam(const PProgram& p, std::unordered_map<int, PProgram>& res) {
        auto* ps = dynamic_cast<ParamSemantics*>(p->semantics.get());
        if (ps && dynamic_cast<TInt*>(ps->oup_type.get())) res[ps->id] = p;
        for (auto& sub: p->sub_list) {
            _collectIntParam(sub, res);
        }
    }

    PProgram _buildIntRangeCons(const PProgram& p, Env* env) {
        auto upper_bound = program::buildConst(BuildData(Int, KIntMax));
        auto lower_bound = program::buildConst(BuildData(Int, -KIntMax));
        auto upper_cons = std::make_shared<Program>(env->getSemantics(">="), (ProgramList){upper_bound, p});
        auto lower_cons = std::make_shared<Program>(env->getSemantics(">="), (ProgramList){p, lower_bound});
        return std::make_shared<Program>(env->getSemantics("&&"), (ProgramList){upper_cons, lower_cons});
    }
}

void prepareSampleSy(Specification* spec, const std::string& task_name) {
    int depth = getGrammarDepth(spec, task_name);
    auto& info = spec->info_list[0];
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) {
        samplesy::registerSampleSyBasic(spec->env.get());
        samplesy::registerSampleSyWitness(spec->env.get());
        info->grammar = samplesy::rewriteGrammar(info->grammar, spec->env.get(),
                                                 dynamic_cast<FiniteIOExampleSpace *>(spec->example_space.get()));
    } else if (theory == TheoryToken::CLIA) {
        theory::clia::loadWitnessFunction(spec->env.get());
        auto* example_space = dynamic_cast<ExampleSpace*>(spec->example_space.get());
        std::unordered_map<int, PProgram> int_params;
        _collectIntParam(example_space->cons_program, int_params);
        for (auto& info: int_params) {
            auto range_cons = _buildIntRangeCons(info.second, spec->env.get());
            example_space->cons_program = std::make_shared<Program>(spec->env->getSemantics("=>"),
                    (ProgramList){range_cons, example_space->cons_program});
        }
    }
    LOG(INFO) << "grammar "; info->grammar->print();
    info->grammar = grammar::generateHeightLimitedGrammar(info->grammar, depth);
}

auto KIntPrepare = [](Grammar* g, Env* env, const IOExample& io_example) {
    int tmp_int_limit = KIntMax;
    auto* ov = dynamic_cast<IntValue*>(io_example.second.get());
    if (ov) tmp_int_limit = std::max(tmp_int_limit, ov->w);
    for (auto& data: io_example.first) {
        auto* iv = dynamic_cast<IntValue*>(data.get());
        if (iv) tmp_int_limit = std::max(tmp_int_limit, std::abs(iv->w));
    }
    env->setConst(theory::clia::KWitnessIntMinName, BuildData(Int, -tmp_int_limit * 2));
    env->setConst(theory::clia::KWitnessIntMaxName, BuildData(Int, tmp_int_limit * 2));
};

auto KStringPrepare = [](Grammar* g, Env* env, const IOExample& io_example) {
    DataList string_const_list, string_input_list;
    std::unordered_set<std::string> const_set;
    for (auto* symbol: g->symbol_list) {
        for (auto* rule: symbol->rule_list) {
            auto* sem = grammar::getConstSemantics(rule);
            if (sem) {
                auto* sv = dynamic_cast<StringValue*>(sem->w.get());
                if (!sv) continue;
                if (const_set.find(sv->s) == const_set.end()) {
                    const_set.insert(sv->s);
                    string_const_list.push_back(sem->w);
                }
            }
        }
    }
    for (const auto& inp: io_example.first) {
        auto* sv = dynamic_cast<StringValue*>(inp.get());
        if (sv) string_input_list.push_back(inp);
    }

    int int_max = 1;
    for (const auto& s: string_const_list) {
        int_max = std::max(int_max, int(theory::string::getStringValue(s).length()));
    }
    for (const auto& s: string_input_list) {
        int_max = std::max(int_max, int(theory::string::getStringValue(s).length()));
    }
    for (const auto& inp: io_example.first) {
        auto* iv = dynamic_cast<IntValue*>(inp.get());
        if (iv) int_max = std::max(int_max, iv->w);
    }

    env->setConst(theory::clia::KWitnessIntMinName, BuildData(Int, -int_max));
    env->setConst(theory::string::KStringConstList, string_const_list);
    env->setConst(theory::string::KStringInputList, string_input_list);
    env->setConst(theory::clia::KWitnessIntMaxName, BuildData(Int, int_max));
};

Splitor* getSplitor(Specification* spec) {
    auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (fio) return new FiniteSplitor(fio);
    auto* zio = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (zio) {
        auto sig = zio->sig_map.begin()->second;
        return new Z3Splitor(spec->example_space.get(), sig.second, sig.first);
    }
    LOG(FATAL) << "Unsupported type of ExampleSpace for the Splitor";
}

namespace {
    PProgram _getIntRangeCons(SynthInfo* info, Env* env) {
        TypeList type_list = info->inp_type_list;
        ProgramList cons_list;
        for (int i = 0; i < type_list.size(); ++i) {
            if (dynamic_cast<TInt*>(type_list[i].get())) {
                auto p = program::buildParam(i, type_list[i]);
                cons_list.push_back(_buildIntRangeCons(p, env));
            }
        }
        if (cons_list.empty()) return program::buildConst(BuildData(Bool, true));
        auto res = cons_list[0];
        for (int i = 1; i < cons_list.size(); ++i) {
            res = std::make_shared<Program>(env->getSemantics("&&"), (ProgramList){res, cons_list[i]});
        }
        return res;
    }
}

GrammarEquivalenceChecker* getEquivalenceChecker(Specification* spec, const PVSABuilder& builder) {
    auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (fio) {
        auto* diff_gen = new VSADifferentProgramGenerator(builder);
        return new FiniteGrammarEquivalenceChecker(diff_gen, fio);
    }
    auto* zio = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (zio) {
        auto* ext = ext::z3::getExtension(spec->env.get());
        auto* g = spec->info_list[0]->grammar;
        auto* env = spec->env.get();
        auto range_limit = _getIntRangeCons(spec->info_list[0].get(), env);
        return new Z3GrammarEquivalenceChecker(g, ext, spec->info_list[0]->inp_type_list, range_limit);
    }
    LOG(FATAL) << "Unsupported type of ExampleSpace for the EquivalenceChecker";
}

SeedGenerator* getSeedGenerator(Specification* spec, const PVSABuilder& builder) {
    auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (fio) {
        auto* diff_gen = new VSADifferentProgramGenerator(builder);
        return new FiniteVSASeedGenerator(builder, new VSASizeBasedSampler(spec->env.get()), diff_gen, fio);
    }
    auto* zio = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (zio) {
        return new Z3VSASeedGenerator(spec, builder, new VSASizeBasedSampler(spec->env.get()));
        //Z3VSASeedGenerator(Specification* spec, const PVSABuilder& _builder, VSASampler* _sampler);
    }
    LOG(FATAL) << "Unsupported type of ExampleSpace for the SeedGenerator";
}

SynthesisResult invokeSampleSy(Specification* spec, TimeGuard* guard) {
    auto* pruner = new TrivialPruner();
    auto& info = spec->info_list[0];
    auto* vsa_ext = ext::vsa::getExtension(spec->env.get());

    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) vsa_ext->setEnvSetter(KStringPrepare);
    else if (theory == TheoryToken::CLIA) {
        vsa_ext->setEnvSetter(KIntPrepare);
        spec->env->setConst(selector::samplesy::KSampleNumLimit, BuildData(Int, 1000));
    }
    else LOG(FATAL) << "Unknosn theory " << sygus::theoryToken2String(theory);
    auto builder = std::make_shared<DFSVSABuilder>(info->grammar, pruner, spec->env.get());

    auto* splitor = getSplitor(spec);
    auto* checker = getEquivalenceChecker(spec, builder);
    auto* gen = getSeedGenerator(spec, builder);
    auto* solver = new SampleSy(spec, splitor, gen, checker);
    auto res = solver->synthesis(guard);
    return {solver->example_count, res};
}

SynthesisResult invokeRandomSy(Specification* spec, TimeGuard* guard) {
    auto* pruner = new TrivialPruner();
    auto& info = spec->info_list[0];
    auto* vsa_ext = ext::vsa::getExtension(spec->env.get());
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) vsa_ext->setEnvSetter(KStringPrepare);
    else if (theory == TheoryToken::CLIA) vsa_ext->setEnvSetter(KIntPrepare);

    auto builder = std::make_shared<DFSVSABuilder>(info->grammar, pruner, spec->env.get());
    auto* checker = getEquivalenceChecker(spec, builder);

    auto* example_space = spec->example_space.get();
    if (dynamic_cast<FiniteIOExampleSpace*>(example_space)) {
        auto* diff_gen = new VSADifferentProgramGenerator(builder);
        auto *solver = new FiniteRandomSelector(spec, checker, diff_gen);
        auto res = solver->synthesis(nullptr);
        return {solver->example_count, res};
    }
    auto* zio = dynamic_cast<Z3IOExampleSpace*>(example_space);
    if (zio) {
        spec->env->setConst(theory::clia::KSampleIntMinName, BuildData(Int, -KIntMax));
        spec->env->setConst(theory::clia::KSampleIntMaxName, BuildData(Int, KIntMax));
        auto* gen = new IntExampleGenerator(spec->env.get(), zio->type_list);
        auto* z3_verifier = new Z3Verifier(zio);
        auto* solver = new Z3RandomSelector(spec, checker, z3_verifier, gen);
        auto res = solver->synthesis(nullptr);
        return {solver->example_count, res};
    }
    assert(0);
}

FlattenGrammar* getFlattenGrammar(Specification* spec, TopDownContextGraph* graph, int num) {
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) {
        auto* vsa_ext = ext::vsa::getExtension(spec->env.get());
        vsa_ext->setEnvSetter(KStringPrepare);
        auto* example_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
        example_space->removeDuplicate();
        IOExampleList io_list;
        for (auto& example: example_space->example_space) {
            io_list.push_back(example_space->getIOExample(example));
        }
        auto* g = spec->info_list[0]->grammar;
        auto* validator = new ProgramInsideVSAChecker(spec->env.get(), g, spec->example_space.get());
        auto tool = std::make_shared<FiniteEquivalenceCheckerTool>(spec->env.get(), example_space);
        return new MergedFlattenGrammar(graph, spec->env.get(), num, validator, tool); //selector::getFlattenGrammar(graph, num, validator);
    } else {
        return new TrivialFlattenGrammar(graph, spec->env.get(), num, new AllValidProgramChecker());
        // return selector::getFlattenGrammar(graph, num, [](Program *p) { return true; });
    }
}

std::string getName(FlattenGrammar* fg, const TopDownContextGraph::Edge& edge) {
    auto* sem = edge.semantics.get(); auto* ps = dynamic_cast<ParamSemantics*>(sem);
    if (ps) return fg->param_info_list[ps->id].program->toString();
    return sem->getName();
}

SynthesisResult invokeRandomSemanticsSelector(Specification* spec, int num, LearnedScorerType type, TimeGuard* guard, TopDownModel* model) {
    selector::random::setLearnedExampleLimit(type, spec->env.get());
    auto* pruner = new TrivialPruner();
    auto& info = spec->info_list[0];
    auto* vsa_ext = ext::vsa::getExtension(spec->env.get());
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    if (theory == TheoryToken::STRING) vsa_ext->setEnvSetter(KStringPrepare);
    else if (theory == TheoryToken::CLIA) vsa_ext->setEnvSetter(KIntPrepare);
    auto builder = std::make_shared<DFSVSABuilder>(info->grammar, pruner, spec->env.get());
    auto* checker = getEquivalenceChecker(spec, builder);

    auto* graph = new TopDownContextGraph(info->grammar, model, ProbModelType::NORMAL_PROB);
    global::recorder.start("verify");
    auto* fg = getFlattenGrammar(spec, graph, num);
    global::recorder.end("verify");

    LearnedScorer* scorer;
    if (sygus::getSyGuSTheory(spec->env.get()) == TheoryToken::STRING) {
        scorer = selector::random::buildVSAScorer(type, spec, fg, KStringPrepare);
    } else {
        scorer = selector::random::buildDefaultScorer(type, spec->env.get(), fg);
    }

    auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (fio) {
        auto* diff_gen = new VSADifferentProgramGenerator(builder);
        auto *solver = new FiniteCompleteRandomSemanticsSelector(spec, checker, scorer, diff_gen);
        auto res = solver->synthesis(guard);
        return {solver->example_count, res};
    }

    auto* zio = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (zio) {
        auto range_cons = _getIntRangeCons(spec->info_list[0].get(), spec->env.get());
        auto* solver = new Z3CompleteRandomSemanticsSelector(spec, checker, scorer, range_cons.get(), 5);
        auto res = solver->synthesis(guard);
        return {solver->example_count, res};
    }
    LOG(FATAL) << "Unsupported example space";
}

int main(int argc, char** argv) {
    assert(argc == 4 || argc == 5 || argc == 1);
    std::string benchmark_name, output_name, solver_name, model_name;
    if (argc >= 4) {
        benchmark_name = argv[1];
        output_name = argv[2];
        solver_name = argv[3];
        if (argc == 5) model_name = argv[4];
    } else {
        solver_name = "samplesy@1";
        benchmark_name = "/home/jiry/2022/tests/repair/t10.sl";

        //benchmark_name = config::KSourcePath + "tests/string-interactive/initials-long-repeat.sl";
        //benchmark_name = "/tmp/tmp.wHOuYKwdWN/tests/x.sl";
        output_name = "/tmp/712015926.out";
        model_name = config::KSourcePath + "runner/model/intsy_repair";
    }
    KDefaultStringDepth = 6;
    auto *spec = parser::getSyGuSSpecFromFile(benchmark_name);
    env::setTimeSeed(spec->env.get());
    std::string task_name = getTaskName(benchmark_name);
    prepareSampleSy(spec, task_name);

    /*std::string dataset_name = "intsy_string";
    std::string main_name = "samplesy";
    ext::vsa::learnNFoldModel(spec->env.get(), config::KSourcePath + "/runner/model/" + dataset_name + ".json",
                              config::KSourcePath + "/runner/model/" + dataset_name, 2, main_name);
    exit(0);*/
    SynthesisResult result;
    auto* guard = new TimeGuard(1e9);
    if (solver_name.substr(0, 8) == "samplesy") {
        if (solver_name.length() > 9) {
            int num = std::stoi(solver_name.substr(9));
            LOG(INFO) << "set time out " << num;
            spec->env->setConst(selector::samplesy::KSampleTimeOutLimit, BuildData(Int, num * 1000));
            spec->env->setConst("Selector@TimeOut", BuildData(Int, 2000));
        }
        result = invokeSampleSy(spec, guard);
    } else if (solver_name == "randomsy") {
        result = invokeRandomSy(spec, guard);
    } else {
        auto info = selector::random::splitScorerName(solver_name, spec->env.get());
        if (task_name.substr(task_name.size() - 3) == ".sl") task_name = task_name.substr(0, task_name.size() - 3);
        TopDownModel* model = nullptr;
        if (model_name.empty()) model = ext::vsa::getSizeModel(); else model = ext::vsa::loadNFoldModel(model_name, task_name);
        result = invokeRandomSemanticsSelector(spec, info.second, info.first, guard, model);
    }

    std::cout << result.first << " " << result.second.toString() << std::endl;
    std::cout << guard->getPeriod() << std::endl;
    std::cout << global::recorder.query("verify") << std::endl;
    global::recorder.printAll();
    FILE* f = fopen(output_name.c_str(), "w");
    fprintf(f, "%d %s\n", result.first, result.second.toString().c_str());
    fprintf(f, "%.10lf\n", global::recorder.query("verify"));
    fprintf(f, "%.10lf\n", global::recorder.query("max-verify"));
    fprintf(f, "%.10lf\n", guard->getPeriod());
}