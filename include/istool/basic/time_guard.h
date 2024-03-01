//
// Created by pro on 2021/12/4.
//

#ifndef ISTOOL_TIME_GUARD_H
#define ISTOOL_TIME_GUARD_H

#include <ctime>
#include <exception>
#include <unordered_map>
#include <string>

struct TimeOutError: public std::exception {
};

class TimeGuard {
public:
    timeval start_time;
    double time_limit;
    TimeGuard(double _time_limit);
    virtual void check();
    double getRemainTime() const;
    double getPeriod() const;
    virtual ~TimeGuard() = default;
};

class TimeRecorder {
public:
    std::unordered_map<std::string, double> value_map;
    std::unordered_map<std::string, timeval> start_time_map;
    std::unordered_map<std::string, int> record_map;
    TimeRecorder() = default;
    void start(const std::string& type);
    void end(const std::string& type);
    double query(const std::string& type);
    void record(const std::string& name, int value);
    void add(const std::string& name, int value);
    void printAll();
};

#define TimeCheck(g) if (g) g->check()


#endif //ISTOOL_TIME_GUARD_H
