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
#include "istool/ext/vsa/vsa_extension.h"
#include "istool/ext/vsa/top_down_model.h"
#include "istool/solver/maxflash/maxflash.h"
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

}

Solver * invoker::single::buildMaxFlash(Specification *spec, Verifier *v, const InvokeConfig &config) {
    sygus::loadSyGuSTheories(spec->env.get(), theory::loadWitnessFunction);
    ext::vsa::registerDefaultComposedManager(ext::vsa::getExtension(spec->env.get()));

    auto prepare = config.access("prepare", KDefaultPrepare);

    TopDownModel* model = nullptr;
    model = config.access("model", model);
    if (!model) model = ext::vsa::getSizeModel();
    auto* solver = new MaxFlash(spec, v, model, prepare);
    return solver;
}