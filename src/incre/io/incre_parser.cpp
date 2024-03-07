//
// Created by pro on 2023/3/5.
//

#include "istool/incre/io/incre_from_json.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

using namespace incre;

IncreProgram incre::parseFromF(const std::string &path, bool is_autolabel) {
    std::string tmp_file = "/tmp/" + std::to_string(rand()) + ".json";
    std::string command = config::KIncreParserPath + "f " + path + " " + tmp_file;
    if (is_autolabel) command += " true";
    std::system(command.c_str());
    auto res = incre::jsonFile2program(tmp_file);
    std::system(("rm " + tmp_file).c_str());
    return res;
}