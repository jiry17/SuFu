//
// Created by pro on 2022/1/5.
//

#include "istool/solver/polygen/polygen_term_solver.h"
#include "istool/basic/bitset.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include <random>
#include <algorithm>
#include "glog/logging.h"
#include <iostream>

using namespace polygen;

namespace {
    const int KDefaultMaxTermNum = 4;
    const int KDefaultExampleNum = 5;
    const int KDefaultRandomFactor = 5;

    int _getMaxTermNum(const PSynthInfo& info) {
        return KDefaultMaxTermNum;
        // return std::max(KDefaultMaxTermNum, int(info->inp_type_list.size()));
    }

    int _getMaxExampleNum(const PSynthInfo& info) {
        return KDefaultExampleNum;
        // if (info->inp_type_list.size() > 5) return 2; else return KDefaultExampleNum;
    }

    int _getRandomFactor(const PSynthInfo& info) {
        return std::max(1, int(info->inp_type_list.size() * 3));
    }
}

PolyGenTermSolver::PolyGenTermSolver(Specification *spec, const PSynthInfo& info, const PBESolverBuilder &builder):
    TermSolver(spec, info) {
    term_spec = new Specification({info}, spec->env, spec->example_space);
    domain_solver_list.push_back(builder(term_spec));
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "PolyGenTermSolver require the number of target programs to be 1";
    }

    cache.push_back(new TermSolverCache());
    auto* val = spec->env->getConstRef(solver::polygen::KMaxTermNumName);
    if (!val->isNull()) KMaxTermNum = theory::clia::getIntValue(*val);
    else KMaxTermNum = _getMaxTermNum(spec->info_list[0]);
    val = spec->env->getConstRef(solver::polygen::KMaxExampleNumName);
    if (!val->isNull()) KMaxExampleNum = theory::clia::getIntValue(*val);
    else KMaxExampleNum = _getMaxExampleNum(spec->info_list[0]);
    val = spec->env->getConstRef(solver::polygen::KRandomFactorName);
    if (!val->isNull()) KRandomFactor = theory::clia::getIntValue(*val);
    else KRandomFactor = _getRandomFactor(spec->info_list[0]);
}

namespace {
    std::mt19937 rng;
    std::uniform_real_distribution<double> distribution(0, 1);
}

AssignmentInfo::AssignmentInfo(const FunctionContext &_term): term(_term) {
}
SampleInfo::SampleInfo(const std::vector<int> &_example_list): example_list(_example_list), status(0), result(nullptr) {
    std::sort(example_list.begin(), example_list.end());
}
void SampleInfo::print() const {
    std::cout << "status " << status << " examples";
    for (auto example: example_list) std::cout << " " << example;
    std::cout << std::endl;
    if (status) {
        if (result) std::cout << "  " << result->term.toString() << std::endl; else std::cout << "  null" << std::endl;
    }
}
TermPlan::TermPlan(int _n, const std::vector<AssignmentInfo *> &_term_list): n(_n), term_list(_term_list) {
}
bool TermPlan::checkCover(SampleInfo *sample) {
    for (int &id: sample->example_list) if (!info[id]) return false;
    return true;
}
PSampleInfo TermPlan::generateSampleInfoWithLowerBound(int lim) {
    std::vector<int> id_list(n);
    while (1) {
        bool is_valid = false;
        for (int i = 0; i < n; ++i) {
            int now = rem_example[std::rand() % rem_example.size()];
            if (now >= lim) is_valid = true;
            id_list[i] = now;
        }
        if (is_valid) break;
    }
    return std::make_shared<SampleInfo>(id_list);
}
PSampleInfo TermPlan::generateSampleInfoWithUpperBound(int lim) {
    std::vector<int> id_list(n);
    for (int i = 0; i < n; ++i) id_list[i] = rem_example[std::rand() % lim];
    return std::make_shared<SampleInfo>(id_list);
}

TermSolverCache::~TermSolverCache() {
    for (const auto& info: info_map) {
        delete info.second;
    }
    for (auto* info: plan_set) delete info;
}

void AssignmentInfo::update(const ExampleList &example_list, ExampleSpace *example_space) {
    for (int i = P.size(); i < example_list.size(); ++i) {
        P.append(example_space->satisfyExample(term, example_list[i]));
    }
}

AssignmentInfo * PolyGenTermSolver::buildAssignmentInfo(const FunctionContext &term) {
    auto feature = term.toString();
    if (cache[solver_id]->info_map.count(feature)) {
        auto* res = cache[solver_id]->info_map[feature];
        res->update(example_list, spec->example_space.get());
        return res;
    }
    auto* info = new AssignmentInfo(term);
    info->update(example_list, spec->example_space.get());
    return cache[solver_id]->info_map[feature] = info;
}

namespace {
    ExampleList _idToExample(const std::vector<int>& id_list, const ExampleList& example_list) {
        ExampleList res;
        for (auto& id: id_list) res.push_back(example_list[id]);
        return res;
    }
}

void PolyGenTermSolver::performSample(polygen::SampleInfo *sample) {
    if (sample->status) return;
    sample->status = 1;
    if (cache[solver_id]->solved_sample.count(sample->example_list)) {
        sample->result = cache[solver_id]->solved_sample[sample->example_list];
        if (sample->result) sample->result->update(example_list, spec->example_space.get());
        return;
    }
    auto current_list = _idToExample(sample->example_list, example_list);
    auto res = domain_solver_list[solver_id]->synthesis(current_list, guard);
    if (res.empty()) {
        cache[solver_id]->solved_sample[sample->example_list] = nullptr; return;
    }
    sample->result = buildAssignmentInfo(res);
    cache[solver_id]->solved_sample[sample->example_list] = sample->result;
}

int PolyGenTermSolver::calculateRandomTime(int branch_num, int example_num) {
    int ti = KRandomFactor;
    for (int i = 1; i <= example_num; ++i) ti *= branch_num;
    return ti;
}

void PolyGenTermSolver::extendStart(polygen::TermPlan *plan, int sample_num) {
    int pre_size = plan->info.size(); int n = plan->n;
    sample_num = std::max(sample_num, int(plan->sample_list.size()));
    for (int i = pre_size; i < example_list.size(); ++i) {
        plan->info.append(true); plan->rem_example.push_back(i);
    }
    for (const auto& sample: plan->sample_list) {
        if (sample->result) sample->result->update(example_list, spec->example_space.get());
    }
    std::vector<PSampleInfo> new_sample;
    double p = std::pow(1.0 * pre_size / int(example_list.size()), n);
    int pos = 0;
    while (pos < plan->sample_list.size() && new_sample.size() < sample_num) {
        if (distribution(rng) <= p) {
            new_sample.push_back(plan->sample_list[pos++]);
        } else {
            new_sample.push_back(plan->generateSampleInfoWithLowerBound(pre_size));
        }
    }
    plan->sample_list = new_sample;
}

void PolyGenTermSolver::extendPlan(polygen::TermPlan *plan, polygen::AssignmentInfo *info, polygen::TermPlan *father, int sample_num) {
    int pre_size = plan->info.size(); int n = plan->n;
    sample_num = std::max(sample_num, int(plan->sample_list.size()));
    int pre_example_num = plan->rem_example.size();
    for (int i = pre_size; i < example_list.size(); ++i) {
        if (!info->P[i] && father->info[i]) {
            plan->info.append(true); plan->rem_example.push_back(i);
        } else {
            plan->info.append(false);
        }
    }
    for (const auto& sample: plan->sample_list) {
        if (sample->result) sample->result->update(example_list, spec->example_space.get());
    }
    std::vector<PSampleInfo> reusable_result, new_sample;
    for (const auto& sample: father->sample_list) {
        if (plan->checkCover(sample.get()) && sample->example_list[n - 1] >= pre_size) {
            reusable_result.push_back(sample);
        }
    }
    double p = std::pow(1.0 * pre_example_num / plan->rem_example.size(), n);
    int l_pos = 0, r_pos = 0;
    while ((l_pos < plan->sample_list.size() || r_pos < reusable_result.size()) && new_sample.size() < sample_num) {
        if (distribution(rng) <= p) {
            if (l_pos < plan->sample_list.size()) {
                new_sample.push_back(plan->sample_list[l_pos++]);
            } else {
                new_sample.push_back(plan->generateSampleInfoWithUpperBound(pre_example_num));
            }
        } else {
            if (r_pos < reusable_result.size()) {
                new_sample.push_back(reusable_result[r_pos++]);
            } else {
                new_sample.push_back(plan->generateSampleInfoWithLowerBound(pre_example_num));
            }
        }
    }
    plan->sample_list = new_sample;
}

namespace {
    std::vector<AssignmentInfo*> _insertTermList(TermPlan* plan, AssignmentInfo* info) {
        auto result = plan->term_list; int pos = 0;
        while (pos < plan->term_list.size() && plan->term_list[pos]->term.toString() < info->term.toString()) ++pos;
        result.push_back(nullptr);
        for (int i = int(result.size()) - 1; i > pos; --i) result[i] = result[i - 1];
        result[pos] = info;
        return result;
    }
}

bool polygen::TermPlanCmp::operator()(TermPlan *p1, TermPlan *p2) const {
    if (p1->n < p2->n) return true; if (p1->n > p2->n) return false;
    if (p1->term_list.size() < p2->term_list.size()) return true;
    if (p1->term_list.size() > p2->term_list.size()) return false;
    for (int i = 0; i < p1->term_list.size(); ++i) {
        auto f1 = p1->term_list[i]->term.toString();
        auto f2 = p2->term_list[i]->term.toString();
        if (f1 == f2) continue;
        if (f1 < f2) return true;
    }
    return false;
}

TermPlan * TermSolverCache::buildTermPlan(int n, const std::vector<AssignmentInfo *> &term_list) {
    auto* dummy_plan = new TermPlan(n, term_list);
    auto it = plan_set.find(dummy_plan);
    if (it != plan_set.end()) {
        delete dummy_plan; return *it;
    } else {
        plan_set.insert(dummy_plan);
        return dummy_plan;
    }
}

polygen::TermPlan * PolyGenTermSolver::buildTermPlan(polygen::TermPlan *father, polygen::AssignmentInfo *info, int sample_num) {
    auto term_list = _insertTermList(father, info);
    auto* result = cache[solver_id]->buildTermPlan(father->n, term_list);
    extendPlan(result, info, father, sample_num);
    return result;
}

std::vector<AssignmentInfo*> PolyGenTermSolver::getNextAssignment(TermPlan* plan, int n, int rem_branch) {
    int ti = calculateRandomTime(rem_branch, n);
    int limit = (int(plan->rem_example.size()) - 1) / rem_branch + 1;
    while (plan->sample_list.size() < ti) {
        plan->sample_list.push_back(plan->generateSampleInfoWithUpperBound(plan->rem_example.size()));
    }
    std::set<AssignmentInfo*> info_set;
    std::vector<AssignmentInfo*> result;
    for (int i = 0; i < ti; ++i) {
        auto* sample = plan->sample_list[i].get();
        performSample(sample);
        auto* info = sample->result;
        if (!info) continue;
        if (info_set.find(info) == info_set.end() && (plan->info & info->P).count() >= limit) {
            info_set.insert(info); result.push_back(info);
        }
    }
    return result;
}

namespace {
    struct AssignmentCmp {
        Bitset all;
        AssignmentCmp(const Bitset& _all): all(_all) {}
        int operator()(AssignmentInfo* x, AssignmentInfo* y) {
            return (x->P & all).count() > (y->P & all).count();
        }
    };

    void _printPlan(TermPlan* plan) {
        std::cout << "plan " << plan->n << std::endl;
        for (auto* info: plan->term_list) {
            std::cout << "  " << info->term.toString() << std::endl;
        }
    }
}

bool PolyGenTermSolver::search(polygen::TermPlan *plan, ProgramList &result, int n, int rem_branch) {
    if (rem_branch == 0 || plan->rem_example.empty()) {
        assert(plan->rem_example.empty());
        result.clear();
        for (auto* assignment: plan->term_list) {
            result.push_back(assignment->term.begin()->second);
        }
        return true;
    }
    TimeCheck(guard);
    auto next_assignment = getNextAssignment(plan, n, rem_branch);
    std::sort(next_assignment.begin(), next_assignment.end(), AssignmentCmp(plan->info));
    for (auto* info: next_assignment) {
        int next_size = calculateRandomTime(rem_branch - 1, n);
        auto* next_plan = buildTermPlan(plan, info, next_size);
        if (visited_plan.find(next_plan) == visited_plan.end()) {
            visited_plan.insert(next_plan);
            if (search(next_plan, result, n, rem_branch - 1)) return true;
        }
    }
    return false;
}

ProgramList PolyGenTermSolver::getTerms(int n, int k) {
    visited_plan.clear();
    std::vector<int> full_example;
    auto* start = cache[solver_id]->buildTermPlan(n, {});
    extendStart(start, calculateRandomTime(k, n));
    ProgramList result;
    search(start, result, n, k);
    return result;
}

namespace {
    ProgramList _reorderTerms(const ProgramList& program_list, const std::string& name, const ExampleList& example_list, ExampleSpace* example_space) {
        std::vector<std::pair<int, PProgram>> info_list;
        for (const auto& program: program_list) {
            int num = 0;
            auto ctx = semantics::buildSingleContext(name, program);
            for (const auto& example: example_list) {
                if (example_space->satisfyExample(ctx, example)) ++num;
            }
            info_list.emplace_back(num, program);
        }
        std::sort(info_list.begin(), info_list.end());
        ProgramList result;
        for (const auto& info: info_list) result.push_back(info.second);
        return result;
    }
}

ProgramList PolyGenTermSolver::getTerms() {
    std::set<std::tuple<int, int, int>> calculated_set;
    std::vector<int> progress(1, 0);
    int si_limit = 1;
    while (1) {
        bool is_progress = false;
        for (int si = 0; si < si_limit; ++si){
            if (si == domain_solver_list.size()) {
                auto* relaxed_solver = solver::relaxSolver(domain_solver_list[si - 1]);
                if (relaxed_solver) {
                    domain_solver_list.push_back(relaxed_solver);
                    cache.push_back(new TermSolverCache());
                    progress.push_back(0);
                } else {
                    TimeCheck(guard);
                    continue;
                }
            }
            progress[si] += 1;
            int n_limit = std::min(KMaxExampleNum, progress[si]);
            int k_limit = std::min(KMaxTermNum, int(example_list.size()));
            for (int n = 1; n <= n_limit; ++n) {
                for (int k = 1; k <= k_limit; ++k) {
                    TimeCheck(guard);
                    if (calculated_set.find({si, n, k}) != calculated_set.end()) continue;
                    is_progress = true;
                    calculated_set.insert({si, n, k});
                    solver_id = si;
                    auto result = getTerms(n, k);
                    if (!result.empty()) return _reorderTerms(result, spec->info_list[0]->name, example_list, spec->example_space.get());
                }
            }
        }
        if (domain_solver_list.size() >= si_limit) si_limit += 1;
        if (!is_progress) KMaxExampleNum += 1;
    }
}

namespace {
    bool _checkContinue(const ExampleList& example_list, const ExampleList& new_example_list) {
        if (example_list.size() > new_example_list.size()) return false;
        for (int i = 0; i < example_list.size(); ++i) {
            if (example_list[i] != new_example_list[i]) {
                return false;
            }
        }
        return true;
    }
}

ProgramList PolyGenTermSolver::synthesisTerms(const ExampleList &new_example_list, TimeGuard *_guard) {
    guard = _guard;
    if (!_checkContinue(example_list, new_example_list)) {
        for (int i = 0; i < cache.size(); ++i) {
            delete cache[i]; cache[i] = new TermSolverCache();
        }
    }
    example_list = new_example_list;
    return getTerms();
}

PolyGenTermSolver::~PolyGenTermSolver() {
    for (auto* c: cache) {
        delete c;
    }
    delete term_spec;
}

const std::string solver::polygen::KMaxExampleNumName = "PolyGen@MaxExample";
const std::string solver::polygen::KMaxTermNumName = "PolyGen@MaxTermNum";
const std::string solver::polygen::KRandomFactorName = "PolyGen@RandomFactor";