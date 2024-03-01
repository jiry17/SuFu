//
// Created by pro on 2022/2/16.
//

#include "istool/invoker/invoker.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/witness/string/string_witness.h"
#include "istool/sygus/theory/witness/clia/clia_witness.h"
#include "istool/sygus/theory/witness/theory_witness.h"
#include "istool/ext/composed_semantics/composed_witness.h"
#include "istool/solver/vsa/vsa_builder.h"
#include "istool/solver/vsa/vsa_solver.h"
#include "istool/sygus/sygus.h"
#include <unordered_set>

namespace {
    const auto KDefaultPrepare = [](Grammar* g, Env* env, const IOExample& io_example) {
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

    int _getLength(const Data& d) {
        auto* sv = dynamic_cast<StringValue*>(d.get());
        if (!sv) return 0;
        return sv->s.length();
    }

    VSAPruner* _getDefaultPruner(ExampleSpace* example_space) {
        auto* fio = dynamic_cast<FiniteIOExampleSpace*>(example_space);
        int length_max = 0;
        if (fio) {
            for (auto& example: fio->example_space) {
                auto io_example = fio->getIOExample(example);
                for (auto& d: io_example.first) length_max = std::max(length_max, _getLength(d));
                length_max = std::max(length_max, _getLength(io_example.second));
            }
        }
        return new SizeLimitPruner(1e4, [length_max](VSANode *node) {
            auto *sn = dynamic_cast<SingleVSANode *>(node);
            if (sn) {
                auto *oup = dynamic_cast<DirectWitnessValue *>(sn->oup.get());
                if (oup) {
                    auto *w = dynamic_cast<StringValue *>(oup->d.get());
                    if (w) return w->s.length() > length_max;
                }
            }
            return false;
        });
    }

    const std::string& KDefaultBuilderType = "bfs";
    const int KDefaultHeight = 5;
}

Solver * invoker::single::buildVanillaVSA(Specification *spec, Verifier *v, const InvokeConfig &config) {
    sygus::loadSyGuSTheories(spec->env.get(), theory::loadWitnessFunction);
    ext::vsa::registerDefaultComposedManager(ext::vsa::getExtension(spec->env.get()));

    auto& info = spec->info_list[0];
    int height = config.access("height", KDefaultHeight);
    info->grammar = grammar::generateHeightLimitedGrammar(info->grammar, height);
    auto prepare = config.access("prepare", KDefaultPrepare);
    auto pruner = config.access("pruner", _getDefaultPruner(spec->example_space.get()));

    auto* ext = ext::vsa::getExtension(spec->env.get());
    ext->setEnvSetter(prepare);
    auto builder = config.access("builder",
                                 (std::shared_ptr<VSABuilder>)(std::make_shared<BFSVSABuilder>(info->grammar, pruner, spec->env.get())));
    auto* selector = config.access("selector", static_cast<VSAProgramSelector*>(new VSAMinimalProgramSelector(ext::vsa::getSizeModel())));
    auto* solver = new BasicVSASolver(spec, builder, selector);
    auto* cegis = new CEGISSolver(solver, v);
    return cegis;
}