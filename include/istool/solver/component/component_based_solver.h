//
// Created by pro on 2021/12/9.
//

#ifndef ISTOOL_COMPONENT_BASED_SOLVER_H
#define ISTOOL_COMPONENT_BASED_SOLVER_H

#include "grammar_encoder.h"
#include "istool/solver/solver.h"

class ComponentBasedSynthesizer: public PBESolver {
    std::unordered_map<std::string, Z3GrammarEncoder*> encoder_map;
    Z3Extension* ext;
    virtual Z3EncodeRes encodeExample(Program* program, const Example& example, const std::string& prefix,
            std::unordered_map<std::string, Z3EncodeRes>& cache) const;
    virtual Z3EncodeRes encodeExample(Program* program, const Example& example, const std::string& prefix);
public:
    ComponentBasedSynthesizer(Specification* _spec, const std::unordered_map<std::string, Z3GrammarEncoder*>& _map);
    virtual FunctionContext synthesis(const std::vector<Example>& example_list, TimeGuard* guard = nullptr);
    virtual ~ComponentBasedSynthesizer();
};


#endif //ISTOOL_COMPONENT_BASED_SOLVER_H
