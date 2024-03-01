//
// Created by pro on 2021/12/5.
//

#ifndef ISTOOL_CONFIG_H
#define ISTOOL_CONFIG_H

#include <string>
#include "time_guard.h"

namespace config {
    extern const std::string KSourcePath;
    extern const std::string KEuSolverPath;
    extern const std::string KCVC5Path;
    extern const std::string KIncreParserPath;
    extern const bool KIsDefaultSelf;
}

namespace global {
    extern TimeRecorder recorder;
    extern std::string KStageInfoPath;
    void printStageResult(const std::string& info);
}


#endif //ISTOOL_CONFIG_H
