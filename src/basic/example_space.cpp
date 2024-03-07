//
// Created by pro on 2021/12/4.
//

#include "istool/basic/example_space.h"
#include "istool/basic/type_system.h"
#include "glog/logging.h"
#include <unordered_map>

namespace {
    bool getBool(const Data& data) {
        auto* bv = dynamic_cast<BoolValue*>(data.get());
        return bv->w;
    }
}

ExampleSpace::ExampleSpace(const PProgram &_cons_program, Env* _env): cons_program(_cons_program), env(_env) {
}
bool ExampleSpace::satisfyExample(const FunctionContext &info, const Example &example) {
    try {
        auto res = env->run(cons_program.get(), example, info);
        return getBool(res);
    } catch (SemanticsError& e) {
        return false;
    }
}

IOExampleSpace::IOExampleSpace(const std::string &_func_name): func_name(_func_name) {
}
DataList IOExampleSpace::getInput(const Example &example) {
    return getIOExample(example).first;
}

#include "unordered_set"
void FiniteExampleSpace::removeDuplicate() {
    std::unordered_set<std::string> cache;
    int now = 0;
    for (auto& example: example_space) {
        auto feature = data::dataList2String(example);
        if (cache.count(feature)) continue;
        cache.insert(feature); example_space[now++] = example;
    }
    example_space.resize(now);
}
FiniteExampleSpace::FiniteExampleSpace(const PProgram &_cons_program, const ExampleList &_example_space, Env* env):
    ExampleSpace(_cons_program, env), example_space(_example_space) {
}
FiniteIOExampleSpace::FiniteIOExampleSpace(const PProgram &_cons_program, const ExampleList &_example_space,
        const std::string &_name, const ProgramList &_inp_list, const PProgram &_oup, Env* env):
        FiniteExampleSpace(_cons_program, _example_space, env), IOExampleSpace(_name), inp_list(_inp_list), oup(_oup) {
}
IOExample FiniteIOExampleSpace::getIOExample(const Example &example) {
    DataList inp_vals;
    for (const auto& p: inp_list) {
        inp_vals.push_back(env->run(p.get(), example));
    }
    auto oup_val = env->run(oup.get(), example);
    return {inp_vals, oup_val};
}
bool FiniteIOExampleSpace::satisfyExample(const FunctionContext &ctx, const Example &example) {
    auto io_example = getIOExample(example);
    if (ctx.find(func_name) == ctx.end()) {
        LOG(FATAL) << "Cannot find program " << func_name;
    }
    return example::satisfyIOExample(ctx.find(func_name)->second.get(), io_example, env);
}

bool example::satisfyIOExample(Program *program, const IOExample &example, Env* env) {
    try {
        auto oup = env->run(program, example.first);
        return oup == example.second;
    } catch (SemanticsError& e) {
        return false;
    }
}

#include <unordered_set>

std::shared_ptr<FiniteIOExampleSpace> example::buildFiniteIOExampleSpace(const IOExampleList &examples, const std::string& name, Env *env, const TypeList& given_types) {
    if (examples.empty()) {
        LOG(FATAL) << "Example space should not be empty";
    }
    auto* type_ext = type::getTypeExtension(env);
    int n = examples[0].first.size();
    ProgramList l_subs;
    TypeList inp_types = given_types;
    if (inp_types.empty()) {
        for (int i = 0; i < n; ++i) {
            auto type = type_ext->getType(examples[0].first[i].get());
            inp_types.push_back(type);
        }
    }
    for (int i = 0; i < n; ++i) {
        l_subs.push_back(program::buildParam(i, inp_types[i]));
    }
    auto oup_type = type_ext->getType(examples[0].second.get());
    auto r = program::buildParam(n, oup_type);
    auto l = std::make_shared<Program>(
            std::make_shared<TypedInvokeSemantics>(name, oup_type, inp_types, env),
            l_subs);
    ProgramList sub_list = {l, r};
    auto cons_program = std::make_shared<Program>(env->getSemantics("="), sub_list);
    ExampleList example_list;
    for (auto& io_example: examples) {
        Example example = io_example.first;
        example.push_back(io_example.second);
        example_list.push_back(example);
    }
    return std::make_shared<FiniteIOExampleSpace>(cons_program, example_list, name, l_subs, r, env);
}

std::string example::ioExample2String(const IOExample &example) {
    return data::dataList2String(example.first) + "=>" + example.second.toString();
}

Example example::ioExample2Example(const IOExample &example) {
    Example res = example.first; res.push_back(example.second);
    return res;
}