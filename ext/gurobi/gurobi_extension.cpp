//
// Created by pro on 2022/5/20.
//

#include "istool/ext/gurobi/gurobi_extension.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "glog/logging.h"

GRBExtension::GRBExtension(int _KIntLimit): KIntLimit(_KIntLimit) {
    env.set("LogFile", "gurobi.log");
    env.start();
}

GRBVar GRBExtension::buildVar(Type *type, GRBModel &model, const std::string &name) {
    if (dynamic_cast<TInt *>(type)) {
        return model.addVar(-KIntLimit, KIntLimit, 0.0, GRB_INTEGER, name);
    } else if (dynamic_cast<TBool *>(type)) {
        return model.addVar(0, 1, 0.0, GRB_BINARY, name);
    }
    LOG(FATAL) << "GRBExtension does not support type " << type->getName();
}
GRBLinExpr GRBExtension::buildConst(const Data &data) {
    auto* bv = dynamic_cast<BoolValue*>(data.get());
    if (bv) {
        if (bv->w) return 1.0; else return 0.0;
    }
    auto* cv = dynamic_cast<IntValue*>(data.get());
    if (cv) return cv->w;
    LOG(FATAL) << "GRBExtension does not support value " << data.toString();
}
Data GRBExtension::getValueFromModel(double w, Type *type) {
    if (dynamic_cast<TBool*>(type)) return BuildData(Bool, w > 0.5);
    if (dynamic_cast<TInt*>(type)) {
        int res = int(w);
        while (res < w - 0.5) ++res; while (res > w + 0.5) --res;
        return BuildData(Int, w);
    }
    LOG(FATAL) << "GRBExtension does not support type " << type->getName();
}
Data GRBExtension::getValueFromModel(const GRBVar &var, Type *type) {
    auto w = var.get(GRB_DoubleAttr_X);
    return getValueFromModel(w, type);
}

#define getOutput(type, ind)  encode ## type ## Program(program->sub_list[ind].get(), param_list, model)

GRBVar GRBExtension::encodeBoolProgram(Program *program, const std::vector<GRBVar> &param_list, GRBModel &model) {
    auto* ps = dynamic_cast<ParamSemantics*>(program->semantics.get());
    if (ps) return param_list[ps->id];
    auto* cs = dynamic_cast<ConstSemantics*>(program->semantics.get());
    auto tb = type::getTBool(); auto res = buildVar(tb.get(), model);
    if (cs) {
        model.addConstr(res == buildConst(cs->w));
        return res;
    }
    if (program->semantics->getName() == "&&") {
        auto l = getOutput(Bool, 0), r = getOutput(Bool, 1);
        GRBVar vars[] = {l, r};
        model.addGenConstrAnd(res, vars, 2);
        return res;
    }
    if (program->semantics->getName() == "||") {
        auto l = getOutput(Bool, 0), r = getOutput(Bool, 1);
        GRBVar vars[] = {l, r};
        model.addGenConstrOr(res, vars, 2);
        return res;
    }
    if (program->semantics->getName() == "!") {
        auto sub = getOutput(Bool, 0);
        model.addConstr(res == 1 - sub);
        return res;
    }
    if (program->semantics->getName() == "<=") {
        auto l = getOutput(Int, 0); auto r = getOutput(Int, 1);
        model.addConstr(l >= r - KIntLimit * res + 1);
        model.addConstr(l - KIntLimit + KIntLimit * res <= r);
        return res;
    }
    if (program->semantics->getName() == "<") {
        auto l = getOutput(Int, 0); auto r = getOutput(Int, 1);
        model.addConstr(l >= r - KIntLimit * res);
        model.addConstr(l + 1 - KIntLimit + KIntLimit * res <= r);
        return res;
    }
    if (program->semantics->getName() == ">=") {
        auto l = getOutput(Int, 0), r = getOutput(Int, 1);
        model.addConstr(r >= l - KIntLimit * res + 1);
        model.addConstr(r - KIntLimit + KIntLimit * res <= l);
        return res;
    }
    if (program->semantics->getName() == ">") {
        auto l = getOutput(Int, 0), r = getOutput(Int, 1);
        model.addConstr(r >= l - KIntLimit * res);
        model.addConstr(r + 1 - KIntLimit + KIntLimit * res <= l);
        return res;
    }
    if (program->semantics->getName() == "==" || program->semantics->getName() == "=") {
        auto l = getOutput(Int, 0), r = getOutput(Int, 1);
        auto delta = buildVar(tb.get(), model);
        model.addConstr(1 - res - (KIntLimit + 1) * delta <= l - r);
        model.addConstr(res - 1 + (KIntLimit + 1) * (1 - delta) >= l - r);
        return res;
    }
    LOG(FATAL) << "GRBExtension does not support semantics " << program->semantics->getName();
}

GRBLinExpr GRBExtension::encodeIntProgram(Program *program, const std::vector<GRBVar> &param_list, GRBModel &model) {
    auto* cs = dynamic_cast<ConstSemantics*>(program->semantics.get());
    if (cs) return buildConst(cs->w);
    auto* ps = dynamic_cast<ParamSemantics*>(program->semantics.get());
    if (ps) return param_list[ps->id];
    if (program->semantics->getName() == "+") {
        auto l = getOutput(Int, 0), r = getOutput(Int, 1);
        return l + r;
    }
    if (program->semantics->getName() == "-") {
        auto l = getOutput(Int, 0), r = getOutput(Int, 1);
        return l - r;
    }
    if (program->semantics->getName() == "ite") {
        auto b = getOutput(Bool, 0);
        auto l = getOutput(Int, 1), r = getOutput(Int, 2);
        auto ti = theory::clia::getTInt(); auto res = buildVar(ti.get(), model);
        model.addGenConstrIndicator(b, true, l == res);
        model.addGenConstrIndicator(b, false, r == res);
        return res;
    }
    LOG(FATAL) << "GRBExtension does not support semantics " << program->semantics->getName();
}

GRBLinExpr GRBExtension::encodeProgram(Program *program, Type *type, const std::vector<GRBVar> &param_list, GRBModel &model) {
    // LOG(INFO) << "encode program " << program->toString() << " " << type->getName();
    if (dynamic_cast<TBool*>(type)) return encodeBoolProgram(program, param_list, model);
    if (dynamic_cast<TInt*>(type)) return encodeIntProgram(program, param_list, model);
    LOG(FATAL) << "GRBExtension does not support semantics " << program->semantics->getName();
}

namespace {
    const std::string KGurobiName = "Gurobi";
    const int KDefaultIntLimit = 5;
}

const std::string ext::gurobi::KIntLimitName = "GRB@IntLimit";

GRBExtension * ext::gurobi::getExtension(Env *env) {
    auto* res = env->getExtension(KGurobiName);
    if (res) {
        auto* grb_ext = dynamic_cast<GRBExtension*>(res);
        if (!grb_ext) {
            LOG(FATAL) << "Unmatched Extension " << KGurobiName;
        }
        return grb_ext;
    }
    int w = theory::clia::getIntValue(*(env->getConstRef(KIntLimitName, BuildData(Int, KDefaultIntLimit))));
    auto* grb_ext = new GRBExtension(w);
    env->registerExtension(KGurobiName, grb_ext);
    return grb_ext;
}