//
// Created by pro on 2022/2/13.
//

#ifndef ISTOOL_COMPONENT_DSL_H
#define ISTOOL_COMPONENT_DSL_H

#include "istool/basic/specification.h"
#include "istool/basic/grammar.h"

namespace dsl::component {
    struct ComponentBenchmarkInfo {
    public:
        PEnv env;
        int inp_num;
        std::vector<std::string> extra_list;
        std::vector<unsigned int> const_list;
        PProgram target;
        ComponentBenchmarkInfo() = default;
        ComponentBenchmarkInfo(const PEnv& env, const PProgram& _target, int _inp_num=1,
                const std::vector<std::string>& _extra_list={}, const std::vector<unsigned int>& const_list={});
    };

    Specification* getComponentSpecification(const ComponentBenchmarkInfo& info);
    void prepareEnv(Env* env);
}

#endif //ISTOOL_COMPONENT_DSL_H
