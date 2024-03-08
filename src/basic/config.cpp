//
// Created by pro on 2021/12/5.
//

#include "istool/basic/config.h"
#include "glog/logging.h"

#ifdef LINUX
const std::string config::KSourcePath = SOURCEPATH + "/";
#else
const std::string config::KSourcePath = SOURCEPATH + "/";
#endif

#ifdef LINUX
const std::string config::KIncreParserPath = SOURCEPATH + "/surface/";
#else
const std::string config::KIncreParserPath = SOURCEPATH + "/surface/";
#endif

const bool config::KIsDefaultSelf = true;

std::string global::KStageInfoPath = "";
TimeRecorder global::recorder;

void global::printStageResult(const std::string& info) {
    if (KStageInfoPath.empty()) {
        LOG(INFO) << info;
    } else {
        auto* f = std::fopen(KStageInfoPath.c_str(), "a");
        fprintf(f, "%s\n", info.c_str());
        fclose(f);
    }
}