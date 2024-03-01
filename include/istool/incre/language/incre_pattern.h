//
// Created by pro on 2022/9/16.
//

#ifndef ISTOOL_INCRE_PATTERN_H
#define ISTOOL_INCRE_PATTERN_H

#include <memory>
#include <vector>
#include <string>

namespace incre {

    enum class PatternType {
        UNDER_SCORE, VAR, TUPLE, CONSTRUCTOR
    };

    class PatternData {
    public:
        PatternType pattern_type;
        PatternData(PatternType _pattern_type);
        PatternType getType() const;
        virtual std::string toString() const = 0;
        virtual ~PatternData() = default;
    };

    typedef std::shared_ptr<PatternData> Pattern;
    typedef std::vector<Pattern> PatternList;

    class PtUnderScore: public PatternData {
    public:
        PtUnderScore();
        virtual std::string toString() const;
        virtual ~PtUnderScore() = default;
    };

    class PtVar: public PatternData {
    public:
        std::string name;
        PtVar(const std::string& _name);
        virtual std::string toString() const;
        virtual ~PtVar() = default;
    };

    class PtTuple: public PatternData {
    public:
        PatternList pattern_list;
        PtTuple(const PatternList& _pattern_list);
        virtual std::string toString() const;
        virtual ~PtTuple() = default;
    };

    class PtConstructor: public PatternData {
    public:
        std::string name;
        Pattern pattern;
        PtConstructor(const std::string& _name, const Pattern& _pattern);
        virtual std::string toString() const;
        virtual ~PtConstructor() = default;
    };

    std::vector<std::string> collectNames(PatternData* pattern);
}

#endif //ISTOOL_INCRE_PATTERN_H
