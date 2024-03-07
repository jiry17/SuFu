//
// Created by pro on 2022/11/24.
//

#ifndef ISTOOL_INCRE_LOOKUP_H
#define ISTOOL_INCRE_LOOKUP_H

#include "incre_program.h"

namespace incre::match {
    typedef std::unordered_map<std::string, bool> MatchContext;

    struct MatchTask {
        std::function<bool(TermData*, const MatchContext&)> term_matcher;
        std::function<bool(TyData*, const MatchContext&)> type_matcher;
        MatchTask();
    };

    bool match(TyData* type, const MatchTask& task, const MatchContext& ctx = {});
    bool match(TermData* term, const MatchTask& task, const MatchContext& ctx = {});

    MatchContext match(ProgramData* program, const MatchTask& task);
}

#endif //ISTOOL_INCRE_LOOKUP_H
