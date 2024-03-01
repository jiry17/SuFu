//
// Created by pro on 2021/12/18.
//

#include "istool/basic/type_system.h"
#include "istool/ext/z3/z3_extension.h"
#include "glog/logging.h"
#include <sstream>

Z3EncodeRes::Z3EncodeRes(const z3::expr &_res, const z3::expr_vector &_cons_list): res(_res), cons_list(_cons_list) {
}
std::string Z3EncodeRes::toString() const {
    std::string s = res.to_string() + "@{";
    for (int i = 0; i < cons_list.size(); ++i) {
        if (i) s += ","; s += cons_list[i].to_string();
    }
    return s + "}";
}

Z3Extension::Z3Extension(Env* _env): env(_env) {
    ext::z3::loadLogicSemantics(this);
    semantics_list.push_back(new BasicZ3SemanticsManager(this));
    type_ext = type::getTypeExtension(env);
}

void Z3Extension::registerZ3Type(Z3Type *util) {
    util_list.push_front(util);
}
void Z3Extension::registerSemanticsManager(Z3SemanticsManager *manager) {
    semantics_list.push_front(manager);
}

Z3Type * Z3Extension::getZ3Type(Type *type) const {
    for (auto* util: util_list) {
        if (util->matchType(type)) return util;
    }
    LOG(FATAL) << "Z3 ext: unsupported type " << type->getName();
}

z3::expr Z3Extension::buildConst(const Data &data) {
    auto* util = getZ3Type(type_ext->getType(data.get()).get());
    return util->buildConst(data, ctx);
}

z3::expr Z3Extension::buildVar(Type *type, const std::string &name) {
    auto* util = getZ3Type(type);
    return util->buildVar(type, name, ctx);
}

Z3EncodeRes Z3Extension::encodeZ3ExprForSemantics(Semantics *semantics, const std::vector<Z3EncodeRes> &inp_list, const Z3EncodeList &param_list) {
    for (auto* manager: semantics_list) {
        if (manager->isMatch(semantics)) {
            return manager->encodeZ3ExprForSemantics(semantics, inp_list, param_list);
        }
    }
    LOG(FATAL) << "Z3: unsupported semantics " << semantics->name;
}

Z3EncodeRes Z3Extension::encodeZ3ExprForProgram(Program *program, const Z3EncodeList &param_list) {
    std::vector<Z3EncodeRes> inp_list;
    for (const auto& sub: program->sub_list) {
        inp_list.push_back(encodeZ3ExprForProgram(sub.get(), param_list));
    }
    return encodeZ3ExprForSemantics(program->semantics.get(), inp_list, param_list);
}

Z3EncodeRes Z3Extension::encodeZ3ExprForConsProgram(Program *program, const FunctionContext& info, const Z3EncodeList &param_list) {
    std::vector<Z3EncodeRes> sub_list;
    for (auto& p: program->sub_list) {
        sub_list.push_back(encodeZ3ExprForConsProgram(p.get(), info, param_list));
    }

    auto* iv = dynamic_cast<InvokeSemantics*>(program->semantics.get());
    if (iv) {
        std::string name = iv->name;
        if (info.find(name) == info.end()) {
            LOG(FATAL) << "Cannot find program " << name;
        }
        auto encode_res = encodeZ3ExprForProgram(info.find(name)->second.get(), sub_list);
        for (auto& sub: sub_list) {
            for (const auto& cons: sub.cons_list) encode_res.cons_list.push_back(cons);
        }
        return encode_res;
    }
    auto encode_res = encodeZ3ExprForSemantics(program->semantics.get(), sub_list, param_list);
    return encode_res;
}

Data Z3Extension::getValueFromModel(const z3::model &model, const z3::expr &expr, Type *type, bool is_strict) {
    auto* util = getZ3Type(type);
    return util->getValueFromModel(model, expr, type, is_strict);
}

Z3Extension::~Z3Extension() {
    for (auto* util: util_list) delete util;
    for (auto* manager: semantics_list) delete manager;
}

void Z3Extension::setTimeOut(z3::solver &solver, TimeGuard *guard) {
    if (!guard) return;
    double remain_time = std::max(guard->getRemainTime() * 1e3, 1.0);
    z3::params p(ctx);
    p.set(":timeout", int(remain_time) + 1u);
    solver.set(p);
}

const std::string KZ3Name = "Z3";

Z3Extension * ext::z3::getExtension(Env *env) {
    auto* res = env->getExtension(KZ3Name);
    if (res) {
        auto* z3_ext = dynamic_cast<Z3Extension*>(res);
        if (!z3_ext) {
            LOG(FATAL) << "Unmatched Extension " << KZ3Name;
        }
        return z3_ext;
    }
    auto* z3_ext = new Z3Extension(env);
    env->registerExtension(KZ3Name, z3_ext);
    return z3_ext;
}

void Z3Extension::registerOperator(const std::string &name, Z3Semantics *semantics) {
    OperatorZ3SemanticsManager* m = nullptr;
    for (auto* manager: semantics_list) {
        m = dynamic_cast<OperatorZ3SemanticsManager*>(manager);
        if (m) break;
    }
    if (!m) {
        m = new OperatorZ3SemanticsManager();
        registerSemanticsManager(m);
    }
    m->registerZ3Semantics(name, semantics);
}

Z3EncodeList ext::z3::z3Vector2EncodeList(const ::z3::expr_vector &expr_list) {
    Z3EncodeList res; ::z3::expr_vector empty_list(expr_list.ctx());
    for (const auto& expr: expr_list) {
        res.emplace_back(expr, empty_list);
    }
    return res;
}