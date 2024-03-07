//
// Created by pro on 2022/1/4.
//

#include "istool/solver/stun/eusolver.h"
#include "istool/solver/enum/enum_util.h"
#include "istool/basic/bitset.h"
#include "glog/logging.h"
#include <unordered_set>
#include <cmath>

EuTermSolver::EuTermSolver(Specification *spec, const PSynthInfo& info): TermSolver(spec, info) {}
EuUnifier::EuUnifier(Specification *spec, const PSynthInfo& info): Unifier(spec, info), io_space(dynamic_cast<IOExampleSpace*>(spec->example_space.get())) {
    if (!io_space) LOG(FATAL) << "EuUnifier supports only IOExampleSpace";
}
EuSolver::EuSolver(Specification *_spec, const PSynthInfo &tg, const PSynthInfo &cg):
    STUNSolver(_spec, tg, cg, DefaultSTUNBuilder(EuTermSolver, _spec, tg), DefaultSTUNBuilder(EuUnifier, _spec, cg)) {
    if (spec->info_list[0]->name != tg->name || spec->info_list[0]->name != cg->name) {
        LOG(INFO) << "The names of terms and conditions should be equal to the target name";
    }
}

namespace {
    class EuVerifier: public Verifier {
    public:
        ExampleList example_list;
        ExampleSpace* example_space;
        std::unordered_set<std::string> feature_set;
        std::vector<bool> is_satisfied;
        ProgramList term_list;
        EuVerifier(const ExampleList& _example_list, ExampleSpace* _example_space):
            example_list(_example_list), example_space(_example_space), is_satisfied(_example_list.size(), false) {
        }
        virtual bool verify(const FunctionContext& info, Example* counter_example) {
            if (counter_example) {
                LOG(INFO) << "EuVerifier is a dummy verifier for EuSolver. It cannot generate counterexamples.";
            }
            assert(info.size() == 1);
            bool is_finished = true;
            std::string feature;
            for (int i = 0; i < example_list.size(); ++i) {
                bool flag = example_space->satisfyExample(info, example_list[i]);
                feature += flag ? '1' : '0';
                if (!is_satisfied[i]) {
                    if (flag) is_satisfied[i] = true; else is_finished = false;
                }
            }
            if (feature_set.find(feature) == feature_set.end()) {
                term_list.push_back(info.begin()->second);
                feature_set.insert(feature);
            }
            return is_finished;
        }
    };
}

ProgramList EuTermSolver::synthesisTerms(const ExampleList &example_list, TimeGuard *guard) {
    auto* verifier = new EuVerifier(example_list, spec->example_space.get());
    auto* optimizer = new TrivialOptimizer();

    EnumConfig c(verifier, optimizer, guard);
    auto enum_res = solver::enumerate({term_info}, c);
    if (enum_res.empty()) {
        LOG(FATAL) << "EuTermSolver: Synthesis Failed";
    }
    auto res = verifier->term_list;
    delete verifier; delete optimizer;
    // for (const auto& term: res) LOG(INFO) << "term: " << term->toString();
    return res;
}

namespace {
    struct UnifyInfo {
        PProgram prog;
        Bitset info;
        UnifyInfo(const PProgram& _prog, const Bitset& _info): prog(_prog), info(_info) {
        }
    };

    class PredicateVerifier: public Verifier {
    public:
        ExampleList example_list;
        std::unordered_set<std::string> feature_set;
        std::vector<UnifyInfo> info_list;
        Env* env;
        PredicateVerifier(const ExampleList& _example_list, Env* _env): example_list(_example_list), env(_env) {}
        virtual bool verify(const FunctionContext& res, Example* counter_example) {
            if (counter_example) {
                LOG(INFO) << "PredicateVerifier is a dummy verifier for EuSolver. It cannot generate counterexamples.";
            }
            assert(res.size() == 1); auto prog = res.begin()->second;
            Bitset info;
            for (int i = 0; i < example_list.size(); ++i) {
                info.append(env->run(prog.get(), example_list[i]).isTrue());
            }

            auto feature = info.toString();
            if (feature_set.find(feature) == feature_set.end()) {
                info_list.emplace_back(prog, info);
                feature_set.insert(feature);
                return true;
            }
            return false;
        }
    };

    const double KDoubleINF = 1e100;
    const double KDoubleEps = 1e-8;

    class DTLearner {
    public:
        std::vector<UnifyInfo> pred_info_list, term_info_list;
        int n;
        PSemantics ite;
        DTLearner(const std::vector<UnifyInfo>& _info_list, const std::vector<UnifyInfo>& _term_list, const PSemantics& _ite):
            pred_info_list(_info_list), term_info_list(_term_list), n(_term_list[0].info.size()), ite(_ite) {
        }

        std::pair<std::vector<int>, std::vector<int>> dividePtsByPredicate(const std::vector<int>& pts, int id) {
            std::vector<int> true_pts, false_pts;
            for (int pts_id: pts) {
                if (pred_info_list[id].info[pts_id]) {
                    true_pts.push_back(pts_id);
                } else false_pts.push_back(pts_id);
            }
            return {true_pts, false_pts};
        }

        double calcEntropy(const std::vector<int>& pts, const std::vector<int>& terms) {
            double ans = 0;
            std::unordered_map<int, int> total_cnt;
            std::vector<std::vector<int>> covered_list;
            for (int tid: terms) {
                std::vector<int> covered;
                for (int pid: pts) if (term_info_list[tid].info[pid]) {
                    covered.push_back(pid);
                }
                for (int pid: covered) total_cnt[pid] += int(covered.size());
                covered_list.push_back(covered);
            }
            for (int i = 0; i < terms.size(); ++i) {
                auto& covered = covered_list[i];
                double prob = 0, top = covered.size();
                for (auto pid: covered) prob += top / double(total_cnt[pid]);
                if (prob > KDoubleEps) {
                    prob /= double(pts.size());
                    ans += -prob * std::log(prob);
                }
            }
            return ans;
        }

        int selectPredicate(const std::vector<int>& pts, const std::vector<int>& preds) {
            double best_info_gain = KDoubleINF; int res = preds[0];
            std::vector<int> related_terms;
            for (int i = 0; i < term_info_list.size(); ++i) {
                bool is_related = false;
                for (int id: pts) {
                    if (term_info_list[i].info[id]) {
                        is_related = true; break;
                    }
                }
                if (is_related) related_terms.push_back(i);
            }

            for (auto pid: preds) {
                auto divide_res = dividePtsByPredicate(pts, pid);
                double entropy_true = calcEntropy(divide_res.first, related_terms);
                double entropy_false = calcEntropy(divide_res.second, related_terms);
                auto info_gain = (entropy_true * divide_res.first.size() + entropy_false * divide_res.second.size()) / pts.size();
                if (info_gain < best_info_gain) {
                    best_info_gain = info_gain; res = pid;
                }
            }
            return res;
        }

        PProgram learn(const std::vector<int>& pts, const std::vector<int>& preds) {
            Bitset need_cov(n, false);
            for (auto id: pts) need_cov.set(id, true);
            for (const auto& term_info: term_info_list) {
                if (term_info.info.checkCover(need_cov)) return term_info.prog;
            }
            if (preds.empty()) return nullptr;
            int pred_id = selectPredicate(pts, preds);
            if (pred_id == -1) return nullptr;
            auto divide_res = dividePtsByPredicate(pts, pred_id);
            std::vector<int> remain_preds;
            for (int id: preds) if (pred_id != id) remain_preds.push_back(id);
            auto true_branch = learn(divide_res.first, remain_preds);
            if (!true_branch) return nullptr;
            auto false_branch = learn(divide_res.second, remain_preds);
            if (!false_branch) return nullptr;
            ProgramList sub_list = {pred_info_list[pred_id].prog, true_branch, false_branch};
            return std::make_shared<Program>(ite, sub_list);
        }

        PProgram learn() {
            std::vector<int> pts;
            for (int i = 0; i < n; ++i) pts.push_back(i);
            std::vector<int> preds;
            for (int i = 0; i < pred_info_list.size(); ++i) preds.push_back(i);
            return learn(pts, preds);
        }
        ~DTLearner() = default;
    };
}

PProgram EuUnifier::unify(const ProgramList &term_list, const ExampleList &example_list, TimeGuard *guard) {
    if (term_list.size() == 1) return term_list[0];
    auto* verifier = new PredicateVerifier(example_list, spec->env.get());
    auto* optimizer = new TrivialOptimizer();
    EnumConfig c(verifier, optimizer, guard);
    std::vector<UnifyInfo> term_info_list;
    auto func_name = spec->info_list[0]->name;
    for (const auto& term: term_list) {
        Bitset info;
        for (const auto& example: example_list) {
            info.append(spec->example_space->satisfyExample(semantics::buildSingleContext(func_name, term), example));
        }
        term_info_list.emplace_back(term, info);
    }

    while (1) {
        TimeCheck(guard);
        auto enum_res = solver::enumerate({unify_info}, c);
        if (enum_res.empty()) LOG(FATAL) << "EuUnifier: synthesis failed";
        DTLearner learner(verifier->info_list, term_info_list, spec->env->getSemantics("ite"));
        auto res = learner.learn();
        if (res) return res;
    }
}