//
// Created by pro on 2022/5/21.
//

#include "istool/ext/gurobi/gurobi_verifier.h"
#include "glog/logging.h"

GRBIOVerifier::GRBIOVerifier(Z3IOExampleSpace *_zio_space): zio_space(_zio_space), ext(ext::gurobi::getExtension(_zio_space->env)) {
}
void GRBIOVerifier::prepareModel(GRBModel &model, Program* p, const std::vector<GRBVar>& param_list, const GRBVar& oup, const std::vector<GRBVar>& var_list) {
    auto sig = zio_space->sig_map.begin()->second;
    for (int i = 0; i < zio_space->inp_list.size(); ++i) {
        auto res = ext->encodeProgram(zio_space->inp_list[i].get(), sig.first[i].get(), var_list, model);
        model.addConstr(param_list[i] == res);
    }
    auto prog_res = ext->encodeProgram(p, sig.second.get(), param_list, model);
    model.addConstr(prog_res == oup);

    auto full_var_list = var_list; full_var_list.push_back(oup);
    auto tb = type::getTBool();
    auto cons_res = ext->encodeProgram(zio_space->oup_cons.get(), tb.get(), full_var_list, model);
    model.addConstr(cons_res == 0);
}
Example GRBIOVerifier::getExample(const std::vector<GRBVar> &var_list) {
    Example res(var_list.size()); auto sig = zio_space->sig_map.begin()->second;
    for (int i = 0; i < var_list.size(); ++i) {
        res[i] = ext->getValueFromModel(var_list[i], sig.first[i].get());
    }
    return res;
}
bool GRBIOVerifier::verify(const FunctionContext &info, Example *counter_example) {
    GRBModel model(ext->env);
    model.set(GRB_IntParam_OutputFlag, 0);
    std::vector<GRBVar> param_list, var_list;
    for (int i = 0; i < zio_space->type_list.size(); ++i) {
        var_list.push_back(ext->buildVar(zio_space->type_list[i].get(), model, "Var" + std::to_string(i)));
    }
    auto sig = zio_space->sig_map.begin()->second;
    GRBVar oup = ext->buildVar(sig.second.get(), model, "Oup");
    for (int i = 0; i < sig.first.size(); ++i) {
        param_list.push_back(ext->buildVar(sig.first[i].get(), model, "Param" + std::to_string(i)));
    }
    auto p = info.begin()->second;
    prepareModel(model, p.get(), param_list, oup, var_list);

    model.optimize();
    int status = model.get(GRB_IntAttr_Status);
    if (status == GRB_INFEASIBLE) return true;
    /*LOG(INFO) << "Var values";
    int nvars = model.get(GRB_IntAttr_NumVars);
    for (int i = 0; i < nvars; ++i) {
        auto var = model.getVar(i);
        LOG(INFO) << var.get(GRB_StringAttr_VarName) << " " << var.get(GRB_DoubleAttr_X);
    }*/
    if (counter_example) {
        (*counter_example) = getExample(var_list);
#ifdef DEBUG
        auto counter_io = zio_space->getIOExample(*counter_example);
        LOG(INFO) << example::ioExample2String(counter_io);
        assert(!(zio_space->env->run(p.get(), counter_io.first) == counter_io.second));
#endif
    }
    return false;
}