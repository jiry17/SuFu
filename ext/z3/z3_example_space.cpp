//
// Created by pro on 2021/12/22.
//

#include "istool/ext/z3/z3_example_space.h"
#include "glog/logging.h"

Z3ExampleSpace::Z3ExampleSpace(const PProgram &_cons_prog, Env *_env, const TypeList& _type_list, const std::unordered_map<std::string, Signature> &_sig_map):
        ExampleSpace(_cons_prog, _env), ext(ext::z3::getExtension(_env)), type_list(_type_list), sig_map(_sig_map) {
}
Z3IOExampleSpace::Z3IOExampleSpace(const PProgram &_cons_prog, Env *_env, const TypeList& _type_list, const Signature &sig,
        const std::string& _name, const ProgramList &_inp_list, const PProgram &_oup_cons):
        Z3ExampleSpace(_cons_prog, _env, _type_list, {{_name, sig}}), IOExampleSpace(_name), inp_list(_inp_list), oup_cons(_oup_cons) {
}

bool Z3IOExampleSpace::satisfyExample(const FunctionContext &info, const Example &example) {
     if (info.find(func_name) == info.end()) {
         LOG(FATAL) << "Cannot find program " << func_name;
     }
     auto* p = info.find(func_name)->second.get();
     DataList inp_vals;
     for (const auto& inp: inp_list) inp_vals.push_back(env->run(inp.get(), example));
     Data oup = env->run(p, example);
     auto full_inp = example; full_inp.push_back(oup);
     auto res = env->run(oup_cons.get(), full_inp);
     // LOG(INFO) << oup.toString() << " " << env->run(p->sub_list[0].get(), full_inp).toString();
     // LOG(INFO) << env->run(p->sub_list[0]->sub_list[0].get(), full_inp).toString();
     auto* bv = dynamic_cast<BoolValue*>(res.get());
     return bv->w;
}

Example Z3IOExampleSpace::getInput(const Example &example) {
    Example res(inp_list.size());
    for (int i = 0; i < inp_list.size(); ++i) res[i] = env->run(inp_list[i].get(), example);
    return res;
}

IOExample Z3IOExampleSpace::getIOExample(const Example &example) {
    auto feature = data::dataList2String(example);
    if (cache.find(feature) != cache.end()) return cache[feature];
    z3::expr_vector z3_inp_list(ext->ctx);
    for (const auto& inp: inp_list) {
        auto val = env->run(inp.get(), example);
        z3_inp_list.push_back(ext->buildConst(val));
    } 
    auto oup_type = sig_map.find(func_name)->second.second;
    auto oup_var = ext->buildVar(oup_type.get(), "oup");
    z3_inp_list.push_back(oup_var);
    z3::solver s(ext->ctx);
    auto encode_res = ext->encodeZ3ExprForProgram(oup_cons.get(), ext::z3::z3Vector2EncodeList(z3_inp_list));
    s.add(encode_res.res); s.add(z3::mk_and(encode_res.cons_list));
    auto res = s.check();
    if (res != z3::sat) {
        LOG(FATAL) << "Cannot find a valid output for input " << data::dataList2String(example);
    }
    auto model = s.get_model();
    auto oup_val = ext->getValueFromModel(model, oup_var, oup_type.get());
    return cache[feature] = {example, oup_val};
}

namespace {
    void collectAllInvokes(const PProgram& p, std::unordered_map<std::string, ProgramList>& invoke_map) {
        auto* is = dynamic_cast<InvokeSemantics*>(p->semantics.get());
        if (is) {
            invoke_map[is->name].push_back(p);
        }
        for (const auto& sub: p->sub_list) {
            collectAllInvokes(sub, invoke_map);
        }
    }
}

PExampleSpace example::buildZ3ExampleSpace(const PProgram &cons, Env *env, const TypeList& type_list, const std::unordered_map<std::string, Signature> &sig_map) {
    std::unordered_map<std::string, ProgramList> invoke_map;
    collectAllInvokes(cons, invoke_map);
    if (invoke_map.size() != 1) return buildZ3ExampleSpace(cons, env, type_list, sig_map);
    std::string name; ProgramList invokes;
    for (auto& info: invoke_map) {
        name = info.first; invokes = info.second;
    }
    std::string feature = invokes[0]->toString();
    bool is_all_same = true;
    for (const auto& p: invokes) {
        if (p->toString() != feature) {
            is_all_same = false; break;
        }
    }
    if (!is_all_same) return buildZ3ExampleSpace(cons, env, type_list, sig_map);
    ProgramList inp_list = invokes[0]->sub_list;
    auto sig = sig_map.find(name)->second;
    PProgram oup_program = program::buildParam(type_list.size(), sig.second);

    auto rewrite = [=](const PSemantics& sem, const ProgramList& sub_list) {
        auto* iv = dynamic_cast<InvokeSemantics*>(sem.get());
        if (iv) return oup_program;
        return std::make_shared<Program>(sem, sub_list);
    };
    auto oup_cons = program::programMap(cons.get(), rewrite);

    return std::make_shared<Z3IOExampleSpace>(cons, env, type_list, sig_map.find(name)->second, name, inp_list, oup_cons);
}