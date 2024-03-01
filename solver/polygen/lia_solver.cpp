//
// Created by pro on 2022/1/4.
//

#include "istool/solver/polygen/lia_solver.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/solver/enum/enum_util.h"
#include "gurobi_c++.h"
#include "glog/logging.h"
#include <unordered_set>
#include <algorithm>
#include <cassert>

const std::string solver::lia::KConstIntMaxName = "LIA@ConstIntMax";
const std::string solver::lia::KTermIntMaxName = "LIA@TermIntMax";
const std::string solver::lia::KMaxCostName = "LIA@MaxCost";
const std::string solver::lia::KDefaultGurobiTimeOutName = "LIA@GurobiTimeout";

namespace {
    const int KDefaultConstValue = 2;
    const int KDefaultTermValue = 2;
    const int KDefaultMaxCost = 10;
    const int KDefaultGurobiTimeOut = 1;

    int _getDefaultConstMax(const PSynthInfo& info) {
        int c_max = KDefaultConstValue;
        for (auto* symbol: info->grammar->symbol_list) {
            for (auto* rule: symbol->rule_list) {
                auto* cs = grammar::getConstSemantics(rule);
                if (!cs) continue;
                auto* iv = dynamic_cast<IntValue*>(cs->w.get());
                if (iv) c_max = std::max(c_max, std::abs(iv->w));
            }
        }
        return c_max;
    }

    int _getDefaultTermMax(const PSynthInfo& info) {
        if (info->inp_type_list.size() < 8) return KDefaultTermValue; else return 1;
    }
}

LIAResult::LIAResult(LIAResult::Status _status): status(_status), c_val(0) {}
LIAResult::LIAResult(const std::vector<int> &_param_list, int _c_val): status(LIAResult::Status::SUCCESS), c_val(_c_val), param_list(_param_list) {
}

namespace {
    PSynthInfo _buildLIAInfo(const PSynthInfo &info) {
        auto *grammar = grammar::copyGrammar(info->grammar);
        grammar->indexSymbol();
        std::string name = grammar::getFreeName(grammar);
        auto *start = new NonTerminal(name, grammar->start->type);
        for (auto *rule: grammar->start->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "Current implementation of LIASolver requires ConcreteRule";
            if (cr->semantics->name == "+" || cr->semantics->name == "-") continue;
            NTList sub_list = rule->param_list;
            start->rule_list.push_back(new ConcreteRule(cr->semantics, std::move(sub_list)));
        }
        int n = grammar->symbol_list.size();
        grammar->symbol_list.push_back(start);
        std::swap(grammar->symbol_list[0], grammar->symbol_list[n]);
        grammar->start = start;
        return std::make_shared<SynthInfo>(info->name, info->inp_type_list, info->oup_type, grammar);
    }
}

BaseLIASolver::BaseLIASolver(Specification *_spec, const ProgramList &_program_list):
    PBESolver(_spec), program_list(_program_list) {
    if (spec->info_list.size() > 1) {
        LOG(FATAL) << "LIA Solver can only synthesize a single program";
    }
    io_example_space = dynamic_cast<IOExampleSpace*>(spec->example_space.get());
    if (!io_example_space) {
        LOG(FATAL) << "LIA solver supports only IOExampleSpace";
    }
    auto term_info = spec->info_list[0];
    if (!dynamic_cast<TInt*>(term_info->oup_type.get())) {
        LOG(FATAL) << "LIA solver supports only integers";
    }
    auto* c_max_data = spec->env->getConstRef(solver::lia::KConstIntMaxName);
    if (c_max_data->isNull()) {
        spec->env->setConst(solver::lia::KConstIntMaxName,
                            BuildData(Int, _getDefaultConstMax(spec->info_list[0])));
    }
    KConstIntMax = theory::clia::getIntValue(*c_max_data);
    auto* t_max_data = spec->env->getConstRef(solver::lia::KTermIntMaxName);
    if (t_max_data->isNull()) {
        spec->env->setConst(solver::lia::KTermIntMaxName,
                            BuildData(Int, _getDefaultTermMax(spec->info_list[0])));
    }
    KTermIntMax = theory::clia::getIntValue(*t_max_data);
    auto* cost_data = spec->env->getConstRef(solver::lia::KMaxCostName);
    if (cost_data->isNull()) KMaxCost = KDefaultMaxCost; else KMaxCost = theory::clia::getIntValue(*cost_data);
    KRelaxTimeLimit = 0.1;

    info = _buildLIAInfo(spec->info_list[0]);
}

PProgram BaseLIASolver::trivialSolve(const ExampleList &example_list) {
    if (example_list.empty()) {
        return program::buildConst(BuildData(Int, 0));
    }
    IOExampleList io_example_list;
    for (const auto& example: example_list) {
        io_example_list.push_back(io_example_space->getIOExample(example));
    }

    {
        bool is_constant = true;
        for (int i = 1; i < io_example_list.size(); ++i) {
            if (!(io_example_list[i].second == io_example_list[0].second)) {
                is_constant = false;
                break;
            }
        }
        if (is_constant) {
            int w = theory::clia::getIntValue(io_example_list[0].second);
            if (w <= KConstIntMax) return program::buildConst(io_example_list[0].second);
        }
    }
    return nullptr;
}

std::pair<IOExampleList, ProgramList> BaseLIASolver::initializeExamples(const ExampleList &example_list) {
    std::unordered_set<std::string> cache;
    ProgramList considered_program_list;
    IOExampleList direct_example_list, wrapped_example_list;
    for (auto& example: example_list) direct_example_list.push_back(io_example_space->getIOExample(example));
    for (auto& io_example: direct_example_list) {
        wrapped_example_list.emplace_back(DataList(), io_example.second);
    }
    for (const auto& program: program_list) {
        DataList output_list;
        bool is_invalid = false;
        for (const auto& io_example: direct_example_list) {
            try {
                auto res = spec->env->run(program.get(), io_example.first);
                if (res.isNull()) {
                    is_invalid = true; break;
                }
                output_list.push_back(res);
            } catch (SemanticsError& e) {
                is_invalid = true; break;
            }
        }
        if (is_invalid) continue;
        auto feature = data::dataList2String(output_list);
        if (cache.find(feature) == cache.end()) {
            cache.insert(feature);
            for (int i = 0; i < output_list.size(); ++i) {
                wrapped_example_list[i].first.push_back(output_list[i]);
            }
            considered_program_list.push_back(program);
        }
    }
    return {wrapped_example_list, considered_program_list};
}

LIASolver::LIASolver(Specification *_spec, const ProgramList &_program_list):
    BaseLIASolver(_spec, _program_list), env(true) {

    auto* gurobi_timeout = spec->env->getConstRef(solver::lia::KDefaultGurobiTimeOutName, BuildData(Int, KDefaultGurobiTimeOut));
    KGurobiTimeOut = theory::clia::getIntValue(*gurobi_timeout);

    env.set("LogFile", "gurobi.log");
    env.set(GRB_DoubleParam_TimeLimit, KGurobiTimeOut);
    env.start();
}

namespace {
    PProgram _times(int c, const PProgram& y, Env* env) {
        if (c == 1) return y;
        ProgramList sub_list = {program::buildConst(BuildData(Int, c)), y};
        return std::make_shared<Program>(env->getSemantics("*"), sub_list);
    }

    PProgram _add(const PProgram& x, int c, const PProgram& y, Env* env) {
        if (c == 0) return x;
        if (!x) return _times(c, y, env);
        ProgramList sub_list = {x, _times(std::abs(c), y, env)};
        std::string name = c > 0 ? "+" : "-";
        return std::make_shared<Program>(env->getSemantics(name), sub_list);
    }
}

PProgram BaseLIASolver::buildProgram(const LIAResult &result, const ProgramList &considered_programs, Env *env) {
    PProgram res;
    auto plus = env->getSemantics("+"), times = env->getSemantics("*");
    if (result.c_val) res = program::buildConst(BuildData(Int, result.c_val));
    for (int i = 0; i < result.param_list.size(); ++i) {
        res = _add(res, result.param_list[i], considered_programs[i], env);
    }
    if (!res) res = program::buildConst(BuildData(Int, 0));
    return res;
}

BaseLIASolver *LIASolver::clone(Specification *spec, const ProgramList &program_list) {
    return new LIASolver(spec, program_list);
}

FunctionContext LIASolver::synthesis(const std::vector<Example> &example_list, TimeGuard *guard) {
    if (example_list.empty()) return semantics::buildSingleContext(io_example_space->func_name, program::buildConst(BuildData(Int, 0)));
    auto [wrapped_example_list, considered_program_list] = initializeExamples(example_list);
    while (1) {
        auto solve_res = solver::lia::solveLIA(env, wrapped_example_list,  KTermIntMax, KConstIntMax, KMaxCost,
                                               guard);
        switch (solve_res.status) {
            case LIAResult::Status::SUCCESS: {
                auto program = buildProgram(solve_res, considered_program_list, spec->env.get());
                return semantics::buildSingleContext(io_example_space->func_name, program);
            }
            case LIAResult::Status::INFEASIBLE: {
                return {};
            }
            case LIAResult::Status::TIMEOUT: {
                KGurobiTimeOut *= 1.5;
                env.set(GRB_DoubleParam_TimeLimit, KGurobiTimeOut);
                LOG(INFO) << "Current timeout " << env.get(GRB_DoubleParam_TimeLimit) << " " << wrapped_example_list.size() << " " << program_list.size();
            }
        }
    }
}

namespace {
    int _getIntValue(const GRBVar& var) {
        double val = var.get(GRB_DoubleAttr_X);
        int w = int(val);
        while (w < val - 0.5) ++w;
        while (w > val + 0.5) --w;
        return w;
    }
}

Data LIAResult::run(const Example &example) const {
    int res = c_val;
    for (int i = 0; i < example.size(); ++i) {
        res += param_list[i] * theory::clia::getIntValue(example[i]);
    }
    return BuildData(Int, res);
}

std::string LIAResult::toString() const {
    std::string res;
    for (int i = 0; i < param_list.size(); ++i) {
        if (!param_list[i]) continue;
        std::string cur = std::to_string(param_list[i]) + "*x" + std::to_string(i);
        if (res.length()) {
            if (param_list[i] > 0) res += "+";
            res += cur;
        } else res = cur;
    }
    if (c_val == 0) {
        if (res.empty()) res = "0";
    } else {
        if (res.length() && c_val > 0) res += "+";
        res += std::to_string(c_val);
    }
    return res;
}

namespace {
    int _getCost(const std::vector<int>& A, int c) {
        int sum = std::abs(c);
        for (int i = 0; i < A.size(); ++i) if (A[i]) ++sum;
        return sum;
    }
}

LIAResult solver::lia::solveLIA(GRBEnv& env, const std::vector<IOExample> &example_list, int t_max, int c_max, int cost_max, TimeGuard *guard) {
    TimeCheck(guard);
    int n = example_list[0].first.size();
    GRBModel model = GRBModel(env);

    model.set(GRB_IntParam_OutputFlag, 0);
    std::vector<GRBVar> var_list;
    std::vector<GRBVar> neg_bound_list;
    std::vector<GRBVar> bound_list;
    for (int i = 0; i <= n; ++i) {
        std::string name_var = "var" + std::to_string(i);
        std::string name_bound = "bound" + std::to_string(i);
        int bound = i < n ? t_max : c_max;
        var_list.push_back(model.addVar(-bound, bound, 0.0, GRB_INTEGER, name_var));
        if (i == n) {
            bound_list.push_back(model.addVar(0, 1, 0.0, GRB_INTEGER, name_bound));
            model.addConstr(var_list[i] <= bound * bound_list[i], "rbound" + std::to_string(i));
            model.addConstr(var_list[i] >= -bound * bound_list[i], "lbound" + std::to_string(i));
        } else {
            bound_list.push_back(model.addVar(0, bound, 0.0, GRB_INTEGER, name_bound));
            neg_bound_list.push_back(model.addVar(0, 1, 0.0, GRB_BINARY, "neg_bound" + std::to_string(i)));
            model.addConstr(var_list[i] <= bound_list[i], "rbound" + std::to_string(i));
            model.addConstr(var_list[i] >= -bound_list[i], "lbound" + std::to_string(i));
            model.addConstr(var_list[i] >= -bound * neg_bound_list[i]);
        }
    }
    int id = 0;
    for (auto& example: example_list) {
        GRBLinExpr expr = var_list[n];
        for (int i = 0; i < n; ++i) expr += theory::clia::getIntValue(example.first[i]) * var_list[i];
        model.addConstr(expr == theory::clia::getIntValue(example.second), "cons" + std::to_string(id++));
    }
    GRBLinExpr target = 0;
    for (auto bound_var: bound_list) {
        target += bound_var;
    }
    for (auto neg_bound: neg_bound_list) {
        target += neg_bound;
    }
    model.addConstr(target <= cost_max, "cost-max");
    model.setObjective(target, GRB_MINIMIZE);

    model.optimize();
    int status = model.get(GRB_IntAttr_Status);
    if (status == GRB_INFEASIBLE) return {LIAResult::Status::INFEASIBLE};
    if (status != GRB_OPTIMAL) return {LIAResult::Status::TIMEOUT};
    int c_val = _getIntValue(var_list[n]);
    std::vector<int> t_val_list;
    for (int i = 0; i < n; ++i) {
        t_val_list.push_back(_getIntValue(var_list[i]));
    }
    LIAResult res(t_val_list, c_val);
#ifdef DEBUG
    for (const auto& example: example_list) {
        auto res_output = res.run(example.first);
        if (!(res_output == example.second)) {
            LOG(INFO) << res.toString() << " " << res_output.toString() << " " << example.second.toString() << std::endl;
            for (const auto& e: example_list) {
                std::cout << "  " << example::ioExample2String(e) << std::endl;
            }
            assert(0);
        }
    }
#endif
    return {t_val_list, c_val};
}

namespace {
    Program* _getNextOperand(Program* program) {
        if (program->semantics->name == "*") return program->sub_list[0].get();
        return program;
    }

    bool _isDuplicated(Program* program) {
        // (p1 + p2) * p3 is equivalent to p1 * p3 and p2 * p3;
        if (program->semantics->name == "+") return true;
        if (program->semantics->name == "-") return true;
        // c * p1 is equivalent to p1.
        if (dynamic_cast<ConstSemantics*>(program->semantics.get())) return true;
        if (program->semantics->name == "*") {
            // (p1 * p2) * p3 is equivalent to p1 * (p2 * p3)
            if (program->sub_list[0]->semantics->name == "*") return true;
            // p1 * (p2 * p3) /\ p1 > p2 is equivalent to p2 * (p1 * p3)
            if (program->sub_list[0]->toString() > _getNextOperand(program->sub_list[1].get())->toString()) return true;
            for (const auto& sub: program->sub_list) {
                if (_isDuplicated(sub.get())) return true;
            }
            return false;
        }
        return false;
    }

    class LIARelaxVerifier: public Verifier {
    public:
        virtual bool verify(const FunctionContext& info, Example* counter_example) {
            assert(!counter_example && info.size() == 1);
            auto program = info.begin()->second;
            if (_isDuplicated(program.get())) {
                return false;
            }
            return true;
        }
        virtual ~LIARelaxVerifier() = default;
    };

    ProgramList _getConsideredTerms(const PSynthInfo& info, int num, double time_out) {
        auto* verifier = new LIARelaxVerifier();
        auto* tmp_guard = new TimeGuard(time_out);
        EnumConfig c(verifier, nullptr, tmp_guard);
        std::vector<FunctionContext> res_list;
        solver::collectAccordingNum({info}, num, res_list, c);
        delete verifier; delete tmp_guard;

        ProgramList next_program_list;
        for (auto& res: res_list) {
            next_program_list.push_back(res.begin()->second);
        }
        return next_program_list;
    }
}

void* BaseLIASolver::relax(TimeGuard* guard) {
    // LOG(INFO) << "relax " << std::endl;
    int next_num = int(program_list.size()) * 2;
    double time_out = KRelaxTimeLimit;
    if (guard) time_out = std::min(time_out, guard->getRemainTime());

    auto next_program_list = _getConsideredTerms(info, next_num, time_out);
    // LOG(INFO) << "Next program list " << next_program_list.size() << " " << next_program_list[next_program_list.size() - 1]->toString() << " " << program_list.size();
    if (next_program_list.size() == program_list.size()) {
        KRelaxTimeLimit *= 2;
        return nullptr;
    }

    return clone(spec, next_program_list);
}

const std::string solver::lia::KIsGurobiName = "IsGurobiAvailable";


BaseLIASolver *solver::lia::getLIASolver(Specification *spec) {
    auto* d = spec->env->getConstRef(KIsGurobiName, BuildData(Bool, true));
    auto info = spec->info_list[0];
    ProgramList program_list = _getConsideredTerms(info, std::max(4, int(info->inp_type_list.size())), 0.1);
    if (d->isTrue()) {
        return new LIASolver(spec, program_list);
    } else return new GreedyLIASolver(spec, program_list);
}