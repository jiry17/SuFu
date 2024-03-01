//
// Created by pro on 2022/5/3.
//

#include "istool/selector/random/random_semantics_selector.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/selector/random/program_adaptor.h"
#include "glog/logging.h"
#include <iomanip>
#include "istool/basic/config.h"

namespace {
    const int KDefaultExampleNumLimit = 4;
    const int KMaxFiniteNum = 5;
    int _getDepth(Program* p) {
        if (p->sub_list.empty()) return 0;
        int res = 0;
        for (const auto& sub: p->sub_list) {
            res = std::max(res, _getDepth(sub.get()));
        }
        return res + 1;
    }
}

BasicRandomSemanticsSelector::BasicRandomSemanticsSelector(Env *_env, Grammar *_g, LearnedScorer *_scorer):
    env(_env), g(_g), scorer(_scorer), example_num(0) {
    KExampleNumLimit = theory::clia::getIntValue(*(env->getConstRef(selector::random::KExampleNumLimitName, BuildData(Int, KDefaultExampleNumLimit))));
}
void BasicRandomSemanticsSelector::addHistoryExample(const Example &inp) {
    if (example_num == KExampleNumLimit) scorer->popExample();
    else example_num++;
    scorer->pushExample(inp);
}
int BasicRandomSemanticsSelector::getBestExampleId(const PProgram &program, const ExampleList &candidate_list) {
    int best_id = -1; RandomSemanticsScore best_cost = 1e9;
    PProgram p;
    p = selector::adaptor::programAdaptorWithLIARules(program.get(), g);
    std::vector<int> id_list(candidate_list.size());
    for (int i = 0; i < id_list.size(); ++i) id_list[i] = i;
    std::shuffle(id_list.begin(), id_list.end(), env->random_engine);
    if (id_list.size() > KMaxFiniteNum) id_list.resize(KMaxFiniteNum);
    for (auto id: id_list) {
        auto& inp = candidate_list[id]; auto cost = scorer->getScore(p, inp);
        printf("%s %.11lf\n", data::dataList2String(inp).c_str(), (double)cost);
        if (cost < best_cost) {
            best_cost = cost; best_id = id;
        }
    }
    return best_id;
}
BasicRandomSemanticsSelector::~BasicRandomSemanticsSelector() {
    delete scorer;
}

namespace {
    const int KStartDepth = 3;
}

void AdaptiveRandomSemanticsSelector::initScorer() {
    LOG(INFO) << "build for depth " << current_depth;
    global::recorder.start("flatten");
    delete scorer;
    auto* fg = builder->getFlattenGrammar(current_depth);
    scorer = scorer_builder(env, fg);
    for (auto& example: example_list) {
        scorer->pushExample(example);
    }
    global::recorder.end("flatten");
}

AdaptiveRandomSemanticsSelector::AdaptiveRandomSemanticsSelector(Env *_env, FlattenGrammarBuilder *_builder, const LearnedScorerBuilder & _score_builder):
    g(_builder->grammar), builder(_builder), env(_env), current_depth(KStartDepth), scorer(nullptr), scorer_builder(_score_builder) {
    initScorer();
    std::cout << KIsPairScore << std::endl;
    KExampleNumLimit = theory::clia::getIntValue(*(env->getConstRef(selector::random::KExampleNumLimitName, BuildData(Int, KDefaultExampleNumLimit))));
}
AdaptiveRandomSemanticsSelector::~AdaptiveRandomSemanticsSelector() noexcept {
    delete scorer;
}
void AdaptiveRandomSemanticsSelector::addHistoryExample(const Example &inp) {
    if (example_list.size() == KExampleNumLimit) {
        scorer->popExample();
        for (int i = 1; i < KExampleNumLimit; ++i) example_list[i - 1] = example_list[i];
        example_list[KExampleNumLimit - 1] = inp;
    } else example_list.push_back(inp);
    scorer->pushExample(inp);
}
int AdaptiveRandomSemanticsSelector::getBestExampleId(const PProgram& program, const ExampleList &candidate_list) {
    global::recorder.start("choose");
    int best_id = -1; RandomSemanticsScore best_cost = 1e9;
    auto p = selector::adaptor::programAdaptorWithLIARules(program.get(), g);
    int depth = _getDepth(p.get());
    if (depth > current_depth) {
        current_depth = depth; initScorer();
    }
#ifdef DEBUG
    assert(p);
    if (p->toString() != program->toString()) {
        // LOG(INFO) << "Program adaption: " << program->toString() << " --> " << p->toString();
        for (auto& inp: candidate_list) {
            assert(env->run(p.get(), inp) == env->run(program.get(), inp));
        }
    }
#endif
    std::vector<int> id_list(candidate_list.size());
    for (int i = 0; i < id_list.size(); ++i) id_list[i] = i;
    std::shuffle(id_list.begin(), id_list.end(), env->random_engine);
    if (id_list.size() > KMaxFiniteNum) id_list.resize(KMaxFiniteNum);
    for (auto id: id_list) {
        auto& inp = candidate_list[id];
        auto cost = scorer->getScore(p, inp);
        printf("%s %.20Lf\n", data::dataList2String(inp).c_str(), cost);
        if (cost < best_cost) {
            best_cost = cost; best_id = id;
        }
    }
    global::recorder.end("choose");
    return best_id;
}

FiniteRandomSemanticsSelector::FiniteRandomSemanticsSelector(Specification *spec, FlattenGrammarBuilder *builder, const LearnedScorerBuilder& _scorer_builder):
        AdaptiveRandomSemanticsSelector(spec->env.get(), builder, _scorer_builder) {
    io_space = dynamic_cast<FiniteIOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "FiniteRandomSemanticsSelector supports only FiniteIOExampleSpace";
    }
    for (auto& example: io_space->example_space) {
        io_example_list.push_back(io_space->getIOExample(example));
    }
}
bool FiniteRandomSemanticsSelector::verify(const FunctionContext &info, Example *counter_example) {
    auto p = info.begin()->second;
    std::vector<int> counter_id_list;
    ExampleList candidate_inp_list;
    for (int i = 0; i < io_example_list.size(); ++i) {
        auto& example = io_example_list[i];
        if (!(env->run(p.get(), example.first) == example.second)) {
            counter_id_list.push_back(i);
            candidate_inp_list.push_back(example.first);
            if (!counter_example) return false;
        }
    }
    if (candidate_inp_list.empty()) return true;
    int best_id = counter_id_list[getBestExampleId(p, candidate_inp_list)];
    *counter_example = io_space->example_space[best_id];
    addHistoryExample(io_example_list[best_id].first);
    return false;
}

Z3RandomSemanticsSelector::Z3RandomSemanticsSelector(Specification *spec, FlattenGrammarBuilder *builder, const LearnedScorerBuilder& _scorer_builder, int _KExampleNum):
        Z3Verifier(dynamic_cast<Z3ExampleSpace*>(spec->example_space.get())), AdaptiveRandomSemanticsSelector(spec->env.get(), builder, _scorer_builder),
        KExampleNum(_KExampleNum) {
    z3_io_space = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (!z3_io_space) {
        LOG(FATAL) << "Z3RandomSemanticsSelector supports only Z3IOExampleSpace";
    }
}
bool Z3RandomSemanticsSelector::verify(const FunctionContext &info, Example *counter_example) {
    global::recorder.start("candidate");
    auto p = info.begin()->second;
    z3::solver s(ext->ctx);
    prepareZ3Solver(s, info);
    auto res = s.check();
    if (res == z3::unsat) return true;
    if (res != z3::sat) {
        LOG(FATAL) << "Z3 failed with " << res;
    }
    ExampleList example_list;
    Example example;
    auto model = s.get_model();
    getExample(model, &example);
    example_list.push_back(example);
    auto param_list = getParamVector();
    for (int _ = 1; _ < KExampleNum; ++_) {
        z3::expr_vector block(ext->ctx);
        for (int i = 0; i < param_list.size(); ++i) {
            block.push_back(ext->buildConst(example[i]) != param_list[i]);
        }
        s.add(z3::mk_or(block));
        auto status = s.check();
        if (status == z3::unsat) break;
        model = s.get_model(); getExample(model, &example);
        example_list.push_back(example);
    }
    global::recorder.end("candidate");
    ExampleList inp_list;
    global::recorder.start("output");
    for (auto& current_example: example_list) {
        inp_list.push_back(z3_io_space->getInput(current_example));
    }
    global::recorder.end("output");
    int best_id = getBestExampleId(p, inp_list);
    addHistoryExample(inp_list[best_id]);
    *counter_example = example_list[best_id];
    return false;
}

GurobiRandomSemanticsSelector::GurobiRandomSemanticsSelector(Specification *spec, FlattenGrammarBuilder *builder, const LearnedScorerBuilder& _scorer_builder, int _KExampleNum):
    GRBIOVerifier(dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get())), AdaptiveRandomSemanticsSelector(spec->env.get(), builder, _scorer_builder), KExampleNum(_KExampleNum) {
}
bool GurobiRandomSemanticsSelector::verify(const FunctionContext &info, Example *counter_example) {
    if (!counter_example) return GRBIOVerifier::verify(info, counter_example);
    auto p = info.begin()->second; auto sig = zio_space->sig_map.begin()->second;
    GRBModel model(ext->env);
    model.set(GRB_IntParam_OutputFlag, 0);
    std::vector<GRBVar> param_list, var_list;
    for (int i = 0; i < sig.first.size(); ++i) {
        param_list.push_back(ext->buildVar(sig.first[i].get(), model, "Param" + std::to_string(i)));
    }
    for (int i = 0; i < zio_space->type_list.size(); ++i) {
        var_list.push_back(ext->buildVar(zio_space->type_list[i].get(), model, "Var" + std::to_string(i)));
    }
    auto oup = ext->buildVar(sig.second.get(), model, "Oup");
    prepareModel(model, p.get(), param_list, oup, var_list);

    model.optimize();
    model.set(GRB_IntParam_SolutionLimit, KExampleNum);
    LOG(INFO) << KExampleNum << std::endl;
    if (model.get(GRB_IntAttr_Status) == GRB_INFEASIBLE) return true;
    ExampleList example_list;
    int diff_example_num = model.get(GRB_IntAttr_SolCount);
    LOG(INFO) << "Diff number num " << diff_example_num;

    for (int id = 0; id < diff_example_num; ++id) {
        model.set(GRB_IntParam_SolutionNumber, id);
        Example example;
        for (int i = 0; i < var_list.size(); ++i) {
            double value = var_list[i].get(GRB_DoubleAttr_Xn);
            example.push_back(ext->getValueFromModel(value, zio_space->type_list[i].get()));
        }
#ifdef DEBUG
        auto io_example = zio_space->getIOExample(example);
        LOG(INFO) << example::ioExample2String(io_example);
        assert(!(env->run(p.get(), io_example.first) == io_example.second));
        for (auto& pre_example: example_list) assert(!(pre_example == example));
#endif
        example_list.push_back(example);
    }

    ExampleList inp_list;
    for (auto& current_example: example_list) {
        inp_list.push_back(zio_space->getIOExample(current_example).first);
    }
    int best_id = getBestExampleId(p, inp_list);
    addHistoryExample(inp_list[best_id]);
    *counter_example = example_list[best_id];
    return false;
}

const std::string selector::random::KExampleNumLimitName = "RandomSelector@ExampleLimit";