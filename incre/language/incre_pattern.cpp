//
// Created by pro on 2022/9/16.
//

#include "istool/incre/language/incre_pattern.h"

using namespace incre;

PatternType PatternData::getType() const {
    return pattern_type;
}
PatternData::PatternData(PatternType _pattern_type): pattern_type(_pattern_type) {
}

PtUnderScore::PtUnderScore(): PatternData(PatternType::UNDER_SCORE) {}
std::string PtUnderScore::toString() const {
    return "_";
}

PtVar::PtVar(const std::string &_name): PatternData(PatternType::VAR), name(_name) {}
std::string PtVar::toString() const {
    return name;
}

PtTuple::PtTuple(const PatternList &_pattern_list): PatternData(PatternType::TUPLE), pattern_list(_pattern_list) {
}
std::string PtTuple::toString() const {
    std::string res = "{";
    for (int i = 0; i < pattern_list.size(); ++i) {
        if (i) res += ","; res += pattern_list[i]->toString();
    }
    return res + "}";
}

PtConstructor::PtConstructor(const std::string &_name, const Pattern &_pattern):
    PatternData(PatternType::CONSTRUCTOR), name(_name), pattern(_pattern) {
}
std::string PtConstructor::toString() const {
    return name + " " + pattern->toString();
}

namespace {
    void _collectNames(PatternData* pt, std::vector<std::string>& res) {
        switch (pt->getType()) {
            case PatternType::TUPLE: {
                auto* tt = dynamic_cast<PtTuple*>(pt);
                for (auto& sub: tt->pattern_list) _collectNames(sub.get(), res);
                return;
            }
            case PatternType::UNDER_SCORE: return;
            case PatternType::CONSTRUCTOR: {
                auto* tc = dynamic_cast<PtConstructor*>(pt);
                _collectNames(tc->pattern.get(), res);
                return;
            }
            case PatternType::VAR: {
                auto* tv = dynamic_cast<PtVar*>(pt);
                res.push_back(tv->name);
                return;
            }
        }
    }
}

std::vector<std::string> incre::collectNames(PatternData *pattern) {
    std::vector<std::string> res;
    _collectNames(pattern, res);
    return res;
}