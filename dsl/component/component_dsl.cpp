//
// Created by pro on 2022/2/13.
//

#include "istool/dsl/component/component_dsl.h"
#include "istool/dsl/component/component_extra_semantics.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "istool/sygus/theory/z3/bv/bv_z3.h"
#include "istool/ext/z3/z3_extension.h"
#include "istool/ext/z3/z3_example_space.h"
#include "glog/logging.h"

namespace {
    Bitset _buildConst(unsigned int w) {
        Bitset res(32, 0);
        for (unsigned int i = 0; i < 32; ++i) {
            if ((w >> i) & 1u) res.set(i, 1);
        }
        return res;
    }

    const int KBVLength = 32;
    const std::string KTargetName = "f";
}

using namespace dsl::component;

ComponentBenchmarkInfo::ComponentBenchmarkInfo(const PEnv& _env, const PProgram &_target, int _inp_num,
        const std::vector<std::string> &_extra_list, const std::vector<unsigned int> &_const_list): env(_env),
        target(_target), inp_num(_inp_num), extra_list(_extra_list), const_list(_const_list) {
}

namespace {
    Signature _getSignature(int inp_num) {
        auto bv_type = theory::bv::getTBitVector(KBVLength);
        TypeList inp_type_list;
        for (int i = 0; i < inp_num; ++i) {
            inp_type_list.push_back(bv_type);
        }
        return {inp_type_list, bv_type};
    }

    PSynthInfo _getCompSynthInfo(Env *env, const ComponentBenchmarkInfo &info) {
        auto bv_type = theory::bv::getTBitVector(KBVLength);
        auto *start = new NonTerminal("start", bv_type);

        auto add_semantics = [&](const std::string &name) {
            auto sem = env->getSemantics(name);
            auto *type_sem = dynamic_cast<TypedSemantics *>(sem.get());
            if (!type_sem) {
                LOG(FATAL) << "Expect typed semantics, but get " << name;
            }
            if (!type::equal(type_sem->oup_type, bv_type)) {
                LOG(FATAL) << "Expect semantics for bv32, but get " << name << " which outputs "
                           << type_sem->oup_type->getName();
            }
            NTList sub_list;
            for (auto &inp: type_sem->inp_type_list) {
                if (!type::equal(inp, bv_type)) {
                    LOG(FATAL) << "Expect semantics for bv32, but get " << name << " which requires " << inp->getName();
                }
                sub_list.push_back(start);
            }
            start->rule_list.push_back(new ConcreteRule(sem, std::move(sub_list)));
        };

        for (const auto &name: {"bvnot", "bvneg", "bvinc", "bvdec", "bvand", "bvor", "bvxor"}) {
            add_semantics(name);
        }
        for (const auto &name: info.extra_list) add_semantics(name);
        for (auto w: info.const_list) {
            auto c = _buildConst(w); NTList sub;
            start->rule_list.push_back(
                    new ConcreteRule(semantics::buildConstSemantics(BuildData(BitVector, c)), std::move(sub)));
        }
        for (int i = 0; i < info.inp_num; ++i) {
            NTList sub;
            start->rule_list.push_back(
                    new ConcreteRule(semantics::buildParamSemantics(i, bv_type), std::move(sub)));
        }
        auto* grammar = new Grammar(start, {start});
        auto sig = _getSignature(info.inp_num);

        return std::make_shared<SynthInfo>(KTargetName, sig.first, sig.second, grammar);
    }

    PExampleSpace _buildExampleSpace(const PProgram& target, int inp_num, Env* env) {
        auto sig = _getSignature(inp_num);
        std::unordered_map<std::string, Signature> sig_map = {{KTargetName, sig}};

        ProgramList sub_list;
        auto l_sem = std::make_shared<InvokeSemantics>(KTargetName, env);
        for (int i = 0; i < inp_num; ++i) sub_list.push_back(program::buildParam(i, sig.first[i]));
        auto l_prog = std::make_shared<Program>(l_sem, sub_list);
        sub_list = {l_prog, target};
        auto cons_program = std::make_shared<Program>(env->getSemantics("="), sub_list);

        return example::buildZ3ExampleSpace(cons_program, env, sig.first, sig_map);
    }
}
Specification * dsl::component::getComponentSpecification(const ComponentBenchmarkInfo &info) {
    auto& env = info.env;
    auto synth_info = _getCompSynthInfo(env.get(), info);
    auto example_space = _buildExampleSpace(info.target, info.inp_num, env.get());
    return new Specification({synth_info}, env, example_space);
}

void dsl::component::prepareEnv(Env* env) {
    theory::bv::setBitVectorLength(env, KBVLength);
    theory::loadBVTheory(env);
    theory::loadZ3BV(env);
    dsl::component::registerExtraComponent(env);
}