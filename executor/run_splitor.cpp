//
// Created by pro on 2022/2/15.
//

#include "istool/selector/split/splitor.h"
#include "istool/basic/config.h"
#include "istool/selector/split/finite_splitor.h"
#include "istool/selector/split/z3_splitor.h"
#include "istool/selector/split/split_selector.h"
#include "istool/invoker/invoker.h"
#include "istool/sygus/sygus.h"
#include "istool/sygus/parser/parser.h"
#include "istool/ext/z3/z3_example_space.h"
#include "istool/selector/baseline/biased_bitvector_selector.h"
#include "istool/ext/vsa/vsa_inside_checker.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/dsl/samplesy/samplesy_dsl.h"
#include "istool/ext/vsa/vsa_extension.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/witness/clia/clia_witness.h"
#include "istool/sygus/theory/witness/string/string_witness.h"
#include "istool/selector/random/random_semantics_selector.h"
#include "istool/selector/random/learned_scorer.h"
#include "istool/selector/baseline/significant_input.h"
#include <ctime>
#include <sys/time.h>

int KTryNum = 1000, KIntMax = 20;

Data _generateData(Type* type, Env* env) {
    if (dynamic_cast<TBool*>(type)) {
        std::bernoulli_distribution dis;
        return BuildData(Bool, dis(env->random_engine));
    } else {
        assert(dynamic_cast<TInt*>(type));
        std::uniform_int_distribution<int> dis(-KIntMax, KIntMax);
        return BuildData(Int, dis(env->random_engine));
    }
}

class RandomVerifier: public Verifier {
public:
    Verifier* v;
    ExampleSpace* space;
    Env* env;
    RandomVerifier(Verifier* _v, ExampleSpace* _space, Env* _env): v(_v), space(_space), env(_env) {
        assert(space);
    }
    virtual bool verify(const FunctionContext& info, Example* counter_example) {
        auto* finite_space = dynamic_cast<FiniteIOExampleSpace*>(space);
        auto p = info.begin()->second;
        if (finite_space) {
            std::uniform_int_distribution<int> dis(0, int(finite_space->example_space.size()) - 1);
            for (int _ = 0; _ < KTryNum; ++_) {
                auto example = finite_space->example_space[dis(env->random_engine)];
                if (!space->satisfyExample(info, example)) {
                    *counter_example = example; return false;
                }
            }
        } else {
            auto* z3_space = dynamic_cast<Z3IOExampleSpace*>(space);
            assert(z3_space);
            for (int _ = 0; _ < KTryNum; ++_) {
                Example example;
                for (auto& type: z3_space->type_list) example.push_back(_generateData(type.get(), env));
                if (!space->satisfyExample(info, example)) {
                    *counter_example = example; return false;
                }
            }
        }
        return v->verify(info, counter_example);
    }
};

Verifier* _buildRandomVerifier(Specification* spec, Verifier* v) {
    return new RandomVerifier(v, spec->example_space.get(), spec->env.get());
}

class VSAOptimizer: public Optimizer {
public:
    Grammar* g;
    VSAExtension* ext;
    IOExampleList example_list;
    VSAOptimizer(Specification* spec) {
        g = spec->info_list[0]->grammar;
        ext = ext::vsa::getExtension(spec->env.get());
        auto* fio = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
        for (auto& example: fio->example_space) {
            example_list.push_back(fio->getIOExample(example));
        }
    }
    virtual bool isDuplicated(const std::string& name, NonTerminal* nt, const PProgram& p) {
        for (auto& example: example_list) {
            if (!ext::vsa::isConsideredByVSA(p.get(), ext, g, example)) {
                return true;
            }
        }
        return false;
    }
    virtual void clear() {}
};

class NonDuplicatedVerifier: public Verifier {
public:
    std::unordered_set<std::string> cache;
    virtual bool verify(const FunctionContext& info, Example* counter_example) {
        auto p = info.begin()->second;
        auto feature = p->toString();
        if (cache.find(feature) != cache.end()) return false;
        cache.insert(feature);
        return true;
    }
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

Verifier* _buildSplitVerifier(Specification* spec, int num, const std::string& solver_name) {
    Splitor* splitor = nullptr;
    Optimizer* o = nullptr;
    Verifier* v = nullptr;
    if (solver_name == "vsa" || solver_name == "maxflash") {
        auto* ext = ext::vsa::getExtension(spec->env.get());
        ext->setEnvSetter(KStringPrepare);
        o = new VSAOptimizer(spec);
        v = new NonDuplicatedVerifier();
    }
    if (dynamic_cast<Z3ExampleSpace*>(spec->example_space.get())) {
        splitor = new Z3Splitor(spec->example_space.get(), spec->info_list[0]->oup_type,
                                spec->info_list[0]->inp_type_list);
    } else {
        assert(dynamic_cast<FiniteExampleSpace*>(spec->example_space.get()));
        splitor = new FiniteSplitor(spec->example_space.get());
    }
    return new SplitSelector(splitor, spec->info_list[0], num, spec->env.get(), v, o);
}

RandomSemanticsScore KOutputSize = 5.0;
int KExampleNum = 5;

Verifier* _buildRandomsSemanticsVerifier(Specification* spec, int num, const std::string& name, TopDownModel* model, LearnedScorerType type) {
    FlattenGrammarBuilder* builder;
    auto* g = spec->info_list[0]->grammar;
    selector::random::setLearnedExampleLimit(type, spec->env.get());

    if (name == "maxflash" || name == "vsa") {
        auto* vsa_ext = ext::vsa::getExtension(spec->env.get());
        vsa_ext->setEnvSetter(KStringPrepare);
        auto* example_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
        IOExampleList io_list;
        for (auto& example: example_space->example_space) {
            io_list.push_back(example_space->getIOExample(example));
        }
        auto tool = std::make_shared<FiniteEquivalenceCheckerTool>(spec->env.get(), example_space);
        //auto* validator = new ProgramInsideVSAChecker(spec->env.get(), g, spec->example_space.get());
        auto* validator = new AllValidProgramChecker();
        builder = new MergedFlattenGrammarBuilder(g, model, spec->env.get(), num, validator, tool);
    } else {
        builder = new TrivialFlattenGrammarBuilder(g, model, spec->env.get(), num, new AllValidProgramChecker());
    }
    LearnedScorerBuilder scorer_builder = [type](Env* env, FlattenGrammar* fg)->LearnedScorer* {
        return selector::random::buildDefaultScorer(type, env, fg);
    };
    if (dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get())) {
        return new Z3RandomSemanticsSelector(spec, builder, scorer_builder, KExampleNum);
    } else {
        return new FiniteRandomSemanticsSelector(spec, builder, scorer_builder);
    }
}

#include "istool/sygus/theory/basic/bv/bv.h"
// train bv model


const std::string KBVBenchmarkNewStyleHead = "(define-fun ehad ((x (_ BitVec 64))) (_ BitVec 64)"
                                             "(bvlshr x #x0000000000000001))"
                                             "(define-fun arba ((x (_ BitVec 64))) (_ BitVec 64)"
                                             "(bvlshr x #x0000000000000004))"
                                             "(define-fun shesh ((x (_ BitVec 64))) (_ BitVec 64)"
                                             "(bvlshr x #x0000000000000010))"
                                             "(define-fun smol ((x (_ BitVec 64))) (_ BitVec 64)"
                                             "(bvshl x #x0000000000000001))"
                                             "(define-fun if0 ((x (_ BitVec 64)) (y (_ BitVec 64)) (z (_ BitVec 64))) (_ BitVec 64)"
                                             "(ite (= x #x0000000000000001) y z));\n";

const std::string KBVBenchmarkHead = "(define-fun ehad ((x (BitVec 64))) (BitVec 64)"
                                     "(bvlshr x #x0000000000000001))"
                                     "(define-fun arba ((x (BitVec 64))) (BitVec 64)"
                                     "(bvlshr x #x0000000000000004))"
                                     "(define-fun shesh ((x (BitVec 64))) (BitVec 64)"
                                     "(bvlshr x #x0000000000000010))"
                                     "(define-fun smol ((x (BitVec 64))) (BitVec 64)"
                                     "(bvshl x #x0000000000000001))"
                                     "(define-fun if0 ((x (BitVec 64)) (y (BitVec 64)) (z (BitVec 64))) (BitVec 64)"
                                     "(ite (= x #x0000000000000001) y z));\n";

std::string getTaskName(const std::string& task_file) {
    int pos = task_file.length() - 1;
    while (task_file[pos] != '/') --pos;
    auto name = task_file.substr(pos + 1);
    if (name.length() > 3 && name.substr(name.length() - 3) == ".sl") {
        name = name.substr(0, name.length() - 3);
    }
    std::cout << task_file << " " << name << std::endl;
    return name;
}

int main(int argc, char** argv) {
    assert(argc == 5 || argc == 6 || argc == 1);
    std::string benchmark_name, output_name, solver_name, verifier_name, model_path;
    auto* guard = new TimeGuard(1e9);
    if (argc != 1) {
        benchmark_name = argv[1];
        output_name = argv[2];
        solver_name = argv[3];
        verifier_name = argv[4];
        if (argc == 6) model_path = argv[5];
    } else {
        solver_name = "maxflash";
        benchmark_name = config::KSourcePath + "tests/1.sl";
        //benchmark_name = "/tmp/tmp.wHOuYKwdWN/tests/string/44789427.sl";
        //benchmark_name = config::KSourcePath + "tests/bv/PRE_icfp_gen_13.20.sl";
        output_name = "/tmp/629453237.out";
        verifier_name = "default";
        // model_path = config::KSourcePath + "runner/model/polygen_clia";
    }

    if (verifier_name == "significant") parser::KIsRemoveDuplicated = false;
    auto *spec = parser::getSyGuSSpecFromFile(benchmark_name);
    /*std::string domain_name = "polygen_clia";
    ext::vsa::learnNFoldModel(spec->env.get(), config::KSourcePath + "/runner/model/" + domain_name + ".json",
            config::KSourcePath + "/runner/model/" + domain_name, 2);
    exit(0);*/
    if (sygus::getSyGuSTheory(spec->env.get()) == TheoryToken::BV) {
        if (solver_name == "ext-eusolver") sygus::setSyGuSHeader(spec->env.get(), KBVBenchmarkHead);
        else if (solver_name == "ext-cvc5") sygus::setSyGuSHeader(spec->env.get(), KBVBenchmarkNewStyleHead);
    }
    auto* v = sygus::getVerifier(spec);

    if (verifier_name == "default") ;
    else if (verifier_name == "significant")
        v = new SignificantInputSelector(dynamic_cast<FiniteExampleSpace*>(spec->example_space.get()), getTaskName(benchmark_name));
    else if (verifier_name == "biased")
        v = new BiasedBitVectorSelector(dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get()));
    else {
        auto info = selector::random::splitScorerName(verifier_name, spec->env.get());
        auto* model = ext::vsa::getSizeModel();
        if (model_path.length()) model = ext::vsa::loadNFoldModel(model_path, getTaskName(benchmark_name));
        v = _buildRandomsSemanticsVerifier(spec, info.second, solver_name, model, info.first);
    }
    env::setTimeSeed(spec->env.get());
    auto solver_token = invoker::string2TheoryToken(solver_name);
    InvokeConfig config;
    if (solver_token == SolverToken::VANILLA_VSA) {
        config.set("height", 5);
        VSAProgramSelector* selector = new VSARandomProgramSelector(spec->env.get());
        config.set("selector", selector);
    }
    auto result = invoker::getExampleNum(spec, v, solver_token, guard);
    auto pure = guard->getPeriod() - global::recorder.query("verify");
    std::cout << result.first << " " << result.second.toString() << std::endl;
    std::cout << guard->getPeriod() << std::endl;
    std::cout << "pure " << guard->getPeriod() - global::recorder.query("verify") << std::endl;
    global::recorder.printAll();
    FILE* f = fopen(output_name.c_str(), "w");
    fprintf(f, "%d %s\n", result.first, result.second.toString().c_str());
    fprintf(f, "%.10lf\n", global::recorder.query("verify"));
    fprintf(f, "%.10lf\n", global::recorder.query("max-verify"));
    fprintf(f, "%.10lf\n", guard->getPeriod());
}