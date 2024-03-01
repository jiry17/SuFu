//
// Created by pro on 2022/2/13.
//

#include "istool/dsl/component/component_dataset.h"
#include "istool/sygus/theory/theory.h"
#include "istool/ext/z3/z3_verifier.h"
#include "istool/selector/selector.h"
#include "istool/invoker/invoker.h"
#include "glog/logging.h"
#include "istool/basic/bitset.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "istool/selector/split/z3_splitor.h"
#include "istool/selector/split/split_selector.h"
#include "istool/solver/maxflash/topdown_context_graph.h"
#include "istool/selector/random/grammar_flatter.h"
#include "istool/selector/random/random_semantics_selector.h"
#include "istool/selector/random/learned_scorer.h"
#include "istool/ext/vsa/top_down_model.h"
#include "istool/basic/config.h"
#include <unordered_set>
#include <istool/selector/baseline/biased_bitvector_selector.h>

const int KTryNum = 1000;

Data getRandBitVec(int size) {
    Bitset res(size, 0);
    for (int i = 0; i < size; ++i) res.set(i, rand() & 1);
    return BuildData(BitVector, res);
}

Example getRandBitVec(const std::vector<int>& size_list) {
    Example res;
    for (auto size: size_list) res.push_back(getRandBitVec(size));
    return res;
}

std::vector<int> extractSizeList(Z3ExampleSpace* example_space) {
    std::vector<int> res;
    for (auto& type: example_space->type_list) {
        auto* bt = dynamic_cast<TBitVector*>(type.get());
        assert(bt);
        res.push_back(bt->size);
    }
    return res;
}

class BVRandomVerifier: public Verifier {
    Verifier* v;
    Z3ExampleSpace* example_space;
    std::vector<int> size_list;
public:
    BVRandomVerifier(Verifier* _v, Z3ExampleSpace* _example_space): v(_v), example_space(_example_space) {
        size_list = extractSizeList(example_space);
    }
    virtual bool verify(const FunctionContext& info, Example* counter_example) {
        if (!counter_example) return v->verify(info, counter_example);
        for (int _ = 0; _ < KTryNum; ++_) {
            Example example = getRandBitVec(size_list);
            if (!example_space->satisfyExample(info, example)) {
                *counter_example = example; return false;
            }
        }
        return v->verify(info, counter_example);
    }
};

const int KBitNum = 2;
class BVPriorVerifier: public Verifier {
    Verifier* v;
    Z3ExampleSpace* example_space;
    std::vector<int> size_list;
    int cnt = -1;
public:
    BVPriorVerifier(Verifier* _v, Z3ExampleSpace* _example_space): v(_v), example_space(_example_space) {
        size_list = extractSizeList(example_space);
    }
    virtual bool verify(const FunctionContext& info, Example* counter_example) {
        ++cnt;
        if (!counter_example || cnt >= (1 << KBitNum)) return v->verify(info, counter_example);
        for (int _ = 0; _ < KTryNum; ++_) {
            Example example = getRandBitVec(size_list);
            for (int i = 0; i < example.size(); ++i) {
                auto w = theory::bv::getBitVectorValue(example[i]);
                for (int i = 0; i < KBitNum; ++i) {
                    if (cnt & (1 << i)) w.set(i, 1); else w.set(i, 0);
                }
                example[i] = BuildData(BitVector, w);
            }
            if (!example_space->satisfyExample(info, example)) {
                *counter_example = example; return false;
            }
        }
        return v->verify(info, counter_example);
    }
};

bool checkUnique(Program* p, std::unordered_set<std::string>& cache) {
    auto* s = p->semantics.get();
    if (dynamic_cast<ParamSemantics*>(s) || dynamic_cast<ConstSemantics*>(s)) return true;
    if (cache.find(s->getName()) != cache.end()) return false;
    cache.insert(s->getName());
    for (auto& sub: p->sub_list) {
        if (!checkUnique(sub.get(), cache)) return false;
    }
    return true;
}

class UniqueVerifier: public Verifier {
public:
    virtual bool verify(const FunctionContext& info, Example* counter_example) {
        assert(!counter_example && info.size() == 1);
        auto* p = info.begin()->second.get();
        std::unordered_set<std::string> cache;
        return checkUnique(p, cache);
    }
};

Verifier* getSplitVerifier(Specification* spec, int num) {
    auto* splitor = new Z3Splitor(spec->example_space.get(), spec->info_list[0]->oup_type, spec->info_list[0]->inp_type_list);
    return new SplitSelector(splitor, spec->info_list[0], num, spec->env.get(), new UniqueVerifier());
}


Verifier* getRandomSemanticsVerifier(Specification* spec, int num, LearnedScorerType type, TopDownModel* model) {
    // selector::random::setLearnedExampleLimit(type, spec->env.get());
    LearnedScorerBuilder scorer_builder = [type](Env* env, FlattenGrammar* fg) {
        return selector::random::buildDefaultScorer(type, env, fg);
    };
    if (dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get())) {
        auto* builder = new TrivialFlattenGrammarBuilder(spec->info_list[0]->grammar, model, spec->env.get(), num, new AllValidProgramChecker());
        return new Z3RandomSemanticsSelector(spec, builder, scorer_builder, 5);
    } else {
        auto* fio_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
        auto tool = std::make_shared<FiniteEquivalenceCheckerTool>(spec->env.get(), fio_space);
        auto* builder = new MergedFlattenGrammarBuilder(spec->info_list[0]->grammar, model, spec->env.get(), num, new AllValidProgramChecker(), tool);
        return new FiniteRandomSemanticsSelector(spec, builder, scorer_builder);
    }
}

const std::string KBVBenchmarkHead = "(define-fun bvinc ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvadd x #x00000001))\n"
                                     "(define-fun bvdec ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvadd x #xffffffff))\n"
                                     "(define-fun bvuleq ((x (BitVec 32)) (y (BitVec 32))) (BitVec 32)\n"
                                     "(ite (bvult (bvadd x #xffffffff) y) #x00000001 (ite (= x #x00000000) #x00000001 #x00000000) ))\n"
                                     "(define-fun bvulq ((x (BitVec 32)) (y (BitVec 32))) (BitVec 32)\n"
                                     "(ite (bvult x y) #x00000001 #x00000000))\n"
                                     "(define-fun bvsub ((x (BitVec 32)) (y (BitVec 32))) (BitVec 32)\n"
                                     "(bvadd x (bvneg y)))\n"
                                     "(define-fun bveq ((x (BitVec 32)) (y (BitVec 32))) (BitVec 32)\n"
                                     "(ite (bvult x y) #x00000000 (ite (bvult y x) #x00000000 #x00000001) ))\n"
                                     "(define-fun bvushr1 ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvlshr x #x00000001))\n"
                                     "(define-fun bvushr2 ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvlshr x #x00000002))\n"
                                     "(define-fun bvushr4 ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvlshr x #x00000004))\n"
                                     "(define-fun bvushr8 ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvlshr x #x00000008))\n"
                                     "(define-fun bvushr16 ((x (BitVec 32))) (BitVec 32)\n"
                                     "(bvlshr x #x00000010))\n";

const std::string KBVBenchmarkNewStyleHead =  "(define-fun bvinc ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvadd x #x00000001))\n"
                                              "(define-fun bvdec ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvadd x #xffffffff))\n"
                                              "(define-fun bvuleq ((x (_ BitVec 32)) (y (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(ite (bvult (bvadd x #xffffffff) y) #x00000001 (ite (= x #x00000000) #x00000001 #x00000000) ))\n"
                                              "(define-fun bvulq ((x (_ BitVec 32)) (y (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(ite (bvult x y) #x00000001 #x00000000))\n"
                                              "(define-fun bveq ((x (_ BitVec 32)) (y (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(ite (bvult x y) #x00000000 (ite (bvult y x) #x00000000 #x00000001) ))\n"
                                              "(define-fun bvushr1 ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvlshr x #x00000001))\n"
                                              "(define-fun bvushr2 ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvlshr x #x00000002))\n"
                                              "(define-fun bvushr4 ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvlshr x #x00000004))\n"
                                              "(define-fun bvushr8 ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvlshr x #x00000008))\n"
                                              "(define-fun bvushr16 ((x (_ BitVec 32))) (_ BitVec 32)\n"
                                              "(bvlshr x #x00000010))\n";


int main(int argc, char** argv) {
    assert(argc == 5 || argc == 6 || argc == 1);
    int benchmark_id;
    std::string output_name, solver_name, verifier_name, model_name;
    if (argc >= 5) {
        benchmark_id = std::atoi(argv[1]);
        output_name = argv[2];
        solver_name = argv[3];
        verifier_name = argv[4];
        if (argc == 6) model_name = argv[5];
    } else {
        benchmark_id = 12;
        solver_name = "ext-eusolver";
        output_name = "/tmp/629453237.out";
        verifier_name = "biased";
        model_name = "/tmp/tmp.wHOuYKwdWN//runner/model/ext-cvc5_icse10_bv";
    }

    auto* spec = dsl::component::getTask(benchmark_id);
    //ext::vsa::learnNFoldModel(spec->env.get(), config::KSourcePath + "/runner/model/ext-eusolver_icse10_bv.json",
    //        config::KSourcePath + "/runner/model/ext-eusolver_icse10_bv", 2);
    //exit(0);
    env::setTimeSeed(spec->env.get());
    if (solver_name == "ext-eusolver") sygus::setSyGuSHeader(spec->env.get(), KBVBenchmarkHead);
    else if (solver_name == "ext-cvc5") sygus::setSyGuSHeader(spec->env.get(), KBVBenchmarkNewStyleHead);

    Verifier* v = new Z3Verifier(dynamic_cast<Z3ExampleSpace*>(spec->example_space.get()));
    if (verifier_name == "default") ;
    else if (verifier_name == "biased")
        v = new Z3BiasedBitVectorSelector(dynamic_cast<Z3ExampleSpace*>(spec->example_space.get()));
    else {
        auto info = selector::random::splitScorerName(verifier_name, spec->env.get());
        TopDownModel* model;
        if (model_name.empty()) model = ext::vsa::getSizeModel();
        else model = ext::vsa::loadNFoldModel(model_name, std::to_string(benchmark_id));
        v = getRandomSemanticsVerifier(spec, info.second, info.first, model);
    }
    auto* guard = new TimeGuard(1e9);
    auto solver_token = invoker::string2TheoryToken(solver_name);
    auto res = invoker::getExampleNum(spec, v, solver_token, guard);

    LOG(INFO) << res.first << " " << res.second.toString();
    LOG(INFO) << global::recorder.query("verify");
    LOG(INFO) << guard->getPeriod();
    FILE* f = fopen(output_name.c_str(), "w");
    fprintf(f, "%d %s\n", res.first, res.second.toString().c_str());
    fprintf(f, "%.10lf\n", global::recorder.query("verify"));
    fprintf(f, "%.10lf\n", guard->getPeriod());
}