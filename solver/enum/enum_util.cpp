//
// Created by pro on 2022/1/12.
//

#include "istool/solver/enum/enum_util.h"
#include "glog/logging.h"

void TrivialOptimizer::clear() {}
bool TrivialOptimizer::isDuplicated(const std::string& name, NonTerminal *nt, const PProgram &p) {
    return false;
}

bool TrivialVerifier::verify(const FunctionContext &info, Example *counter_example) {
    return true;
}

namespace {
    bool _isRemoveCom(const std::string& name, const PProgram& p) {
        assert(p->sub_list.size() <= 2);
        if (p->sub_list.size() != 2) return false;
        auto l = p->sub_list[0]->toString();
        auto r = p->sub_list[1]->toString();
        return l >= r;
    }
    bool _isRemoveAssoc(const std::string& name, const PProgram& p) {
        assert(p->sub_list.size() <= 2);
        if (p->sub_list.size() != 2) return false;
        return p->sub_list[0]->semantics->getName() == name;
    }
    bool _isRemoveConst(const PProgram& p) {
        if (p->sub_list.empty()) {
            return dynamic_cast<ConstSemantics*>(p->semantics.get());
        }
        bool is_remove = true;
        for (auto& sub_program: p->sub_list) {
            if (!_isRemoveConst(sub_program)) {
                is_remove = false;
            }
        }
        return is_remove;
    }
}

void RuleBasedOptimizer::clear() {}

bool RuleBasedOptimizer::isDuplicated(const std::string &name, NonTerminal *nt, const PProgram &p) {
    auto sem_name = p->semantics->getName();
    if (KComOpSet.find(sem_name) != KComOpSet.end() && _isRemoveCom(sem_name, p)) {
        return true;
    }
    if (KAssocOpSet.find(sem_name) != KAssocOpSet.end() && _isRemoveAssoc(sem_name, p)) {
        return true;
    }
    if (!p->sub_list.empty() && _isRemoveConst(p)) {
        return true;
    }
    return false;
}

const std::unordered_set<std::string> RuleBasedOptimizer::KComOpSet = {"+", "*", "||", "&&", "max", "min"};
const std::unordered_set<std::string> RuleBasedOptimizer::KAssocOpSet = {"+", "*", "||", "&&", "max", "min"};

OBEOptimizer::OBEOptimizer(ProgramChecker* _is_runnable, const std::unordered_map<std::string, ExampleList> &_pool, Env* _env):
        is_runnable(_is_runnable), example_pool(_pool), env(_env) {
}
bool OBEOptimizer::isDuplicated(const std::string& name, NonTerminal *nt, const PProgram &p) {
    if (!is_runnable->isValid(p.get()) || example_pool.find(name) == example_pool.end()) return false;
    auto& example_list = example_pool[name];
    DataList res;
    for (auto& example: example_list) {
        try {
            res.push_back(env->run(p.get(), example));
        } catch (SemanticsError& e) {
            return true;
        }
    }
    std::string feature = std::to_string(nt->id) + "@" + data::dataList2String(res);
    if (visited_set.find(feature) != visited_set.end()) return true;
    visited_set.insert(feature);
    return false;
}
void OBEOptimizer::clear() {
    visited_set.clear();
}
// TODO: change the type of is_runnable to PProgramChecker to avoid memory leak;
OBEOptimizer::~OBEOptimizer() {
    // delete is_runnable;
}

NumberLimitedVerifier::NumberLimitedVerifier(int _n, Verifier* _v): n(_n), v(_v) {}
bool NumberLimitedVerifier::verify(const FunctionContext &info, Example *counter_example) {
    if (counter_example) {
        LOG(FATAL) << "NumberLimitedVerifier cannot return counter examples";
    }
    if (v && !v->verify(info, counter_example)) return false;
    result.push_back(info);
    return result.size() >= n;
}

SizeLimitedVerifier::SizeLimitedVerifier(int _size_limit, Verifier* _v): size_limit(_size_limit), v(_v) {}
bool SizeLimitedVerifier::verify(const FunctionContext &info, Example *counter_example) {
    if (counter_example) {
        LOG(FATAL) << "SizeLimitedVerifier cannot return counter examples";
    }
    int total_size = 0;
    for (const auto& func: info) {
        total_size += func.second->size();
    }
    if (total_size > size_limit) return true;
    if (v && !v->verify(info, counter_example)) return false;
    result.push_back(info);
    return false;
}

bool solver::collectAccordingNum(const std::vector<PSynthInfo> &info_list, int n, std::vector<FunctionContext> &result, EnumConfig c) {
    auto* o = new TrivialOptimizer();
    auto* v = new NumberLimitedVerifier(n, c.v);
    EnumConfig tmp(v, c.o ? c.o : o, c.guard);
    bool is_timeout = false;
    try {
        solver::enumerate(info_list, tmp);
    } catch (TimeOutError& e) {
        is_timeout = true;
    }
    result = v->result;
    delete o; delete v;
    return is_timeout;
}

bool solver::collectAccordingSize(const std::vector<PSynthInfo> &info_list, int size_limit, std::vector<FunctionContext> &result, EnumConfig c) {
    auto* o = new TrivialOptimizer();
    auto* v = new SizeLimitedVerifier(size_limit, c.v);
    EnumConfig tmp(v, c.o ? c.o : o, c.guard);
    tmp.size_limit = size_limit;
    bool is_timeout = false;
    try {
        solver::enumerate(info_list, tmp);
    } catch (TimeOutError& e) {
        is_timeout = true;
    }
    result = v->result;
    delete o; delete v;
    return is_timeout;
}