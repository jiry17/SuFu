//
// Created by pro on 2022/2/15.
//

#include "istool/invoker/invoker.h"
#include "istool/solver/component/component_based_solver.h"
#include "istool/solver/component/linear_encoder.h"
#include "istool/solver/component/tree_encoder.h"
#include "glog/logging.h"

namespace {
    Z3GrammarEncoder* _buildEncoder(Grammar* grammar, Z3Extension* ext, const std::string& encoder_name) {
        if (encoder_name == "Linear") return new LinearEncoder(grammar, ext);
        else if (encoder_name == "Tree") return new TreeEncoder(grammar, ext);
        LOG(FATAL) << "Unknown encoder name " << encoder_name;
    }
}

Solver * invoker::single::buildCBS(Specification *spec, Verifier *v, const InvokeConfig &config) {
    std::unordered_map<std::string, Z3GrammarEncoder*> encoder_list;
    auto* ext = ext::z3::getExtension(spec->env.get());
    auto encoder_name = config.access("encoder", (std::string)"Linear");
    for (auto& info: spec->info_list) {
        encoder_list[info->name] = _buildEncoder(info->grammar, ext, encoder_name);
    }
    auto* s = new CEGISSolver(new ComponentBasedSynthesizer(spec, encoder_list), v);
    return s;
}