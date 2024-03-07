//
// Created by pro on 2021/12/3.
//

#include "istool/basic/semantics.h"
#include "istool/basic/program.h"
#include "istool/basic/env.h"
#include <map>
#include "glog/logging.h"

Semantics::Semantics(const std::string &_name): name(_name) {
}
std::string Semantics::getName() {
    return name;
}
std::string Semantics::buildProgramString(const std::vector<std::string> &sub_list) {
    std::string res = name + "(";
    for (int i = 0; i < sub_list.size(); ++i) {
        if (i) res += ","; res += sub_list[i];
    }
    return res + ")";
}

std::string Semantics::buildProgramStringToHaskell(const std::vector<std::string> &sub_list) {
    std::string res = name;
    /* already modified in composed_rule.cpp/_buildSketchToHaskell
    // modify constructor
    if (res == "+") res = "Cadd";
    else if (res == "-") res = "Csub";
    else if (res == "*") res = "Cmul";
    else if (res == "=") res = "Ceq";
    else if (res == "<") res = "Cless";
    else if (res == "&&") res = "Cand";
    else if (res == "||") res = "Cor";
    else if (res == "!") res = "Cnot";
    else if (res == "0") res = "Czero";
    else if (res == "1") res = "Cone";
    else if (res == "ite") res = "CIte";
    else if (!(res[0] >= 'a' && res[0] <= 'z' || res[0] >= 'A' && res[0] <= 'Z')) {
        std::cout << "error: res is not a letter!" << std::endl;
        return res;
    }
    else res[0] = std::toupper(res[0]);
    // change name of semantics
    name = res;
    */

    for (int i = 0; i < sub_list.size(); ++i) {
        res += " (UnionM ";
        res += sub_list[i];
        res += ")";
    }
    return res;
}

FullExecutedSemantics::FullExecutedSemantics(const std::string &name): Semantics(name) {}
Data FullExecutedSemantics::run(const std::vector<std::shared_ptr<Program>> &sub_list, ExecuteInfo *info) {
    DataList res;
    for (const auto& p: sub_list) res.push_back(p->run(info));
    return run(std::move(res), info);
}

NormalSemantics::NormalSemantics(const std::string& name, const PType &_oup_type, const TypeList &_inp_list):
    TypedSemantics(std::move(_oup_type), std::move(_inp_list)), FullExecutedSemantics(name) {
}

ParamSemantics::ParamSemantics(const PType &type, int _id):
        NormalSemantics("Param" + std::to_string(_id), type, {}), id(_id) {
}
Data ParamSemantics::run(DataList&& inp_list, ExecuteInfo *info) {
    return info->param_value[id];
}
std::string ParamSemantics::buildProgramString(const std::vector<std::string> &sub_exp) {
    return name;
}

ConstSemantics::ConstSemantics(const Data &_w): FullExecutedSemantics(_w.toString()), w(_w) {
}
ConstSemantics::ConstSemantics(const Data& _w, const std::string& _name): FullExecutedSemantics(_name), w(_w) {
}
Data ConstSemantics::run(DataList&& inp_list, ExecuteInfo *info) {
    return w;
}
std::string ConstSemantics::buildProgramString(const std::vector<std::string> &sub_exp) {
    return name;
}

PSemantics semantics::buildConstSemantics(const Data &w) {
    return std::make_shared<ConstSemantics>(w);
}

PSemantics semantics::buildParamSemantics(int id, const PType &type) {
    if (!type) {
        auto var_type = std::make_shared<TVar>("a");
        return std::make_shared<ParamSemantics>(std::move(var_type), id);
    }
    PType tmp = type;
    return std::make_shared<ParamSemantics>(std::move(tmp), id);
}

std::string FunctionContext::toString() const {
    if (empty()) return "{}";
    std::string res = "{";
    for (const auto& info: *this) {
        res += info.first + ":" + info.second->toString() + ";";
    }
    res[res.length() - 1] = '}';
    return res;
}

DirectSemantics::DirectSemantics(): NormalSemantics("", type::getTVarA(), {type::getTVarA()}) {}
Data DirectSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return inp_list[0];
}
std::string DirectSemantics::buildProgramString(const std::vector<std::string> &sub_exp) {
    return sub_exp[0];
}

InvokeSemantics::InvokeSemantics(const std::string &_func_name, Env *_env): FullExecutedSemantics(_func_name), env(_env) {
}
Data InvokeSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto p = info->func_context[name];
    if (!p) throw SemanticsError();
    return env->run(p.get(), inp_list);
}


TypedInvokeSemantics::TypedInvokeSemantics(const std::string &_func_name, const PType &oup_type, const TypeList &inp_list, Env* _env):
    InvokeSemantics(_func_name, _env), TypedSemantics(oup_type, inp_list) {
}

#define TBOOL type::getTBool()

NotSemantics::NotSemantics(): NormalSemantics("!", TBOOL, {TBOOL}) {
}
Data NotSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return Data(std::make_shared<BoolValue>(!inp_list[0].isTrue()));
}

AndSemantics::AndSemantics(): NormalSemantics("&&", TBOOL, {TBOOL, TBOOL}) {
}
Data AndSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Bool, inp_list[0].isTrue() && inp_list[1].isTrue());
}
Data AndSemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    auto x = sub_list[0]->run(info);
    if (!x.isTrue()) return x;
    return sub_list[1]->run(info);
}

OrSemantics::OrSemantics(): NormalSemantics("||", TBOOL, {TBOOL, TBOOL}) {
}
Data OrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Bool, inp_list[0].isTrue() || inp_list[1].isTrue());
}
Data OrSemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    auto x = sub_list[0]->run(info);
    if (x.isTrue()) return x;
    return sub_list[1]->run(info);
}

ImplySemantics::ImplySemantics(): NormalSemantics("=>", TBOOL, {TBOOL, TBOOL}) {
}
Data ImplySemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return BuildData(Bool, !inp_list[0].isTrue() || inp_list[1].isTrue());
}
Data ImplySemantics::run(const ProgramList &sub_list, ExecuteInfo *info) {
    auto x = sub_list[0]->run(info);
    if (!x.isTrue()) return Data(std::make_shared<BoolValue>(true));
    return sub_list[1]->run(info);
}

AllowFailSemantics::AllowFailSemantics(const PType& t, const Data &_d): Semantics("error->" + _d.toString()), TypedSemantics(t, {t}), d(_d) {
}
Data AllowFailSemantics::run(const std::vector<std::shared_ptr<Program> > &sub_list, ExecuteInfo *info) {
    try {
        return sub_list[0]->run(info);
    } catch (SemanticsError& e) {
        return d;
    }
}

void semantics::loadLogicSemantics(Env* env) {
    LoadSemantics("=>", Imply); LoadSemantics("!", Not);
    LoadSemantics("&&", And); LoadSemantics("||", Or);
    LoadSemantics("and", And); LoadSemantics("or", Or);
    LoadSemantics("not", Not);
}

FunctionContext semantics::buildSingleContext(const std::string &name, const std::shared_ptr<Program> &program) {
    FunctionContext res; res[name] = program;
    return res;
}