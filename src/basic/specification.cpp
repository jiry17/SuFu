//
// Created by pro on 2021/12/5.
//

#include "istool/basic/specification.h"

SynthInfo::SynthInfo(const std::string &_name, const TypeList &_inp, const PType &_oup, Grammar *_g):
    name(_name), inp_type_list(_inp), oup_type(_oup), grammar(_g) {
}
SynthInfo::~SynthInfo() {
    // delete grammar;
}

Specification::Specification(const std::vector<PSynthInfo> &_info_list, const PEnv& _env, const PExampleSpace& _example_space):
    info_list(_info_list), env(_env), example_space(_example_space) {
}