//
// Created by pro on 2022/2/15.
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
#include "istool/basic/config.h"
#include "istool/solver/tmp/batched_maxflash.h"
#include "istool/sygus/sygus.h"
#include <ctime>
#include <istool/sygus/parser/parser.h>
#include <iostream>


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

int main(int argc, char** argv) {
    auto benchmark_name = config::KSourcePath + "/tests/1.sl";

    auto *spec = parser::getSyGuSSpecFromFile(benchmark_name);
    auto* v = sygus::getVerifier(spec);
    spec->env->random_engine.seed(time(0));

    sygus::loadSyGuSTheories(spec->env.get(), theory::loadWitnessFunction);
    ext::vsa::registerDefaultComposedManager(ext::vsa::getExtension(spec->env.get()));

    auto* solver = new tmp::BatchedMaxFlash(spec, v, ext::vsa::getSizeModel(), KDefaultPrepare);

    auto res = solver->batchedSynthesis(100);
    for (int i = 0; i < res.size(); ++i) {
        std::cout << "  " << res[i]->toString() << std::endl;
    }
}