//
// Created by pro on 2022/9/23.
//

#include "istool/incre/analysis/incre_instru_runtime.h"
#include "glog/logging.h"
#include "istool/incre/language/incre_lookup.h"
#include <iostream>
#include <mutex>
#include <thread>
#include <queue>

using namespace incre;

IncreExampleData::IncreExampleData(int _tau_id, const std::unordered_map<std::string, Data> &_local,
                                   const std::unordered_map<std::string, Data> &_global, const Data &_oup):
                                   tau_id(_tau_id), local_inputs(_local), global_inputs(_global), oup(_oup) {
}
std::string IncreExampleData::toString() const {
    std::string res = "(" + std::to_string(tau_id) + ") {";
    bool flag = false;
    for (const auto& [name, t]: local_inputs) {
        if (flag) res += ","; res += (name + ": " + t.toString()); flag = true;
    }
    res += "} @ {"; flag = false;
    for (const auto& [name, t]: global_inputs) {
        if (flag) res += ","; res += (name + ": " + t.toString()); flag = true;
    }
    return res + "} -> " + oup.toString();
}
IncreExampleCollector::~IncreExampleCollector() {
}
void IncreExampleCollector::add(int tau_id, const std::unordered_map<std::string, Data> &local, const Data &oup) {
    assert(example_pool.size() > tau_id);
    example_pool[tau_id].push_back(std::make_shared<IncreExampleData>(tau_id, local, current_global, oup));
}
void IncreExampleCollector::clear() {
    for (int i = 0; i < example_pool.size(); ++i) {
        example_pool[i].clear();
    }
}

void IncreExamplePool::insertExample(int pos, const IncreExample& example) {
    while (pos >= example_pool.size()) {
        example_pool.emplace_back();

    }
}


void IncreExamplePool::merge(IncreExampleCollector *collector, TimeGuard* guard) {
    int count = 0;
    for (int i = 0; i < collector->example_pool.size(); ++i) {
        for (auto& example: collector->example_pool[i]) {
            insertExample(i, example);
            ++count;
            if (count % 100 == 0 && guard && guard->getRemainTime() < 0) {
                return;
            }
        }
    }
}

namespace {

    std::pair<Term, bool> _collectSubst(const Term& x, const std::string& name, const Term& y, IncreExampleCollector* collector);

#define SubstHead(ty) std::pair<Term, bool> _collectSubst(Tm ## ty *x, const Term& _x, const std::string& name, const Term& y, IncreExampleCollector* collector)
#define SubstCase(ty) return _collectSubst(dynamic_cast<Tm ## ty*>(x.get()), x, name, y, collector)
#define SubstRes(field) auto [field ## _res, field ##_flag] = _collectSubst(x->field, name, y, collector)
#define SubstRes2(field) auto [field ## _res, field ## _flag] = _collectSubst(field, name, y, collector)

    SubstHead(Var) {
        if (x->name == name) return {y, true};
        return {_x, false};
    }
    SubstHead(Value) {
        return {_x, false};
    }
    SubstHead(Match) {
        SubstRes(def);
        bool flag = def_flag;
        std::vector<std::pair<Pattern, Term>> cases;
        for (const auto& [pt, branch]: x->cases) {
            if (incre::isUsed(pt, name)) {
                cases.emplace_back(pt, branch);
            } else {
                SubstRes2(branch);
                flag |= branch_flag;
                cases.emplace_back(pt, branch_res);
            }
        }
        if (!flag) return {_x, false};
        return {std::make_shared<TmMatch>(def_res, cases), true};
    }
    SubstHead(Tuple) {
        bool flag = false; TermList fields;
        for (const auto& field: x->fields) {
            SubstRes2(field);
            flag |= field_flag; fields.push_back(field_res);
        }
        if (!flag) return {_x, false};
        return {std::make_shared<TmTuple>(fields), true};
    }
    SubstHead(LabeledLabel) {
        assert(x); SubstRes(content);
        if (!content_flag) return {_x, false};
        return {std::make_shared<TmLabeledLabel>(content_res, x->id), true};
    }
    SubstHead(UnLabel) {
        SubstRes(content);
        if (!content_flag) return {_x, false};
        return {std::make_shared<TmUnLabel>(content_res), true};
    }
    SubstHead(Abs) {
        if (x->name == name) return {_x, false};
        SubstRes(content);
        if (!content_flag) return {_x, false};
        return {std::make_shared<TmAbs>(x->name, x->type, content_res), true};
    }
    SubstHead(App) {
        SubstRes(func); SubstRes(param);
        if (!func_flag && !param_flag) return {_x, false};
        return {std::make_shared<TmApp>(func_res, param_res), true};
    }
    SubstHead(Fix) {
        SubstRes(content);
        if (!content_flag) return {_x, false};
        return {std::make_shared<TmFix>(content_res), true};
    }
    SubstHead(If) {
        SubstRes(c); SubstRes(t); SubstRes(f);
        if (!c_flag && !t_flag && !f_flag) return {_x, false};
        return {std::make_shared<TmIf>(c_res, t_res, f_res), true};
    }
    SubstHead(Let) {
        SubstRes(def);
        if (x->name == name) {
            if (!def_flag) return {_x, false};
            return {std::make_shared<TmLet>(x->name, def_res, x->content), true};
        }
        SubstRes(content);
        if (!def_flag && !content_flag) return {_x, false};
        return {std::make_shared<TmLet>(x->name, def_res, content_res), true};
    }
    SubstHead(Proj) {
        SubstRes(content);
        if (!content_flag) return {_x, false};
        return {std::make_shared<TmProj>(content_res, x->id), true};
    }
    SubstHead(LabeledAlign) {
        assert(x);
        SubstRes(content);
        auto& cared_val = collector->cared_vars[x->id];
        if (!content_flag && cared_val.find(name) == cared_val.end()) return {_x, false};
        auto res = std::make_shared<TmLabeledAlign>(content_res, x->id, x->subst_info);
        if (cared_val.find(name) != cared_val.end()) res->addSubst(name, incre::run(y, nullptr));
        return {res, true};
    }

    std::pair<Term, bool> _collectSubst(const Term& x, const std::string& name, const Term& y, IncreExampleCollector* collector) {
        // LOG(INFO) << "subst " << x->toString() << " " << name << " " << y->toString();
        switch (x->getType()) {
            case TermType::VAR: SubstCase(Var);
            case TermType::MATCH: SubstCase(Match);
            case TermType::VALUE: SubstCase(Value);
            case TermType::IF: SubstCase(If);
            case TermType::TUPLE: SubstCase(Tuple);
            case TermType::ABS: SubstCase(Abs);
            case TermType::APP: SubstCase(App);
            case TermType::LET: SubstCase(Let);
            case TermType::PROJ: SubstCase(Proj);
            case TermType::LABEL: SubstCase(LabeledLabel);
            case TermType::UNLABEL: SubstCase(UnLabel);
            case TermType::ALIGN: SubstCase(LabeledAlign);
            case TermType::FIX: SubstCase(Fix);
            case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << x->toString();
        }
    }

    Term collectSubst(const Term& x, const std::string& name, const Term& y, IncreExampleCollector* collector) {
        return _collectSubst(x, name, y, collector).first;
    }

    Data _collectExample(const Term &term, Context *ctx, IncreExampleCollector* collector);

#define CollectHead(name) Data _collectExample(Tm ## name* term, const Term& _term, Context* ctx, IncreExampleCollector* collector)
#define CollectCase(name) return _collectExample(dynamic_cast<Tm ## name*>(term.get()), term, ctx, collector)
#define CollectSub(name) auto name = _collectExample(term->name, ctx, collector)
#define CollectSub2(name) auto name ## _res = _collectExample(name, ctx, collector)

    CollectHead(Value) {
        return term->data;
    }
    CollectHead(Proj) {
        CollectSub(content);
        auto* tv = dynamic_cast<VTuple*>(content.get());
        assert(tv);
        return tv->elements[term->id - 1];
    }
    CollectHead(Let) {
        CollectSub(def);
        auto content = collectSubst(term->content, term->name, std::make_shared<TmValue>(def), collector);
        CollectSub2(content);
        return content_res;
    }
    CollectHead(Abs) {
        return Data(std::make_shared<VAbsFunction>(_term));
    }

    Data _runCollectFunc(VFunction* func, const Term& param, Context* ctx, IncreExampleCollector* collector) {
        auto* fa = dynamic_cast<VAbsFunction*>(func);
        if (fa) {
            auto name = fa->term->name; auto content = fa->term->content;
            auto c = collectSubst(content, name, param, collector);
            CollectSub2(c);
            return c_res;
        }
        return func->run(param, ctx);
    }

    CollectHead(App) {
        CollectSub(func); CollectSub(param);
        auto* av = dynamic_cast<VFunction*>(func.get());
        assert(av);
        return _runCollectFunc(av, std::make_shared<TmValue>(param), ctx, collector);
    }
    CollectHead(Fix) {
        CollectSub(content);
        auto* av = dynamic_cast<VFunction*>(content.get());
        assert(av);
        return _runCollectFunc(av, _term, ctx, collector);
    }
    CollectHead(Match) {
        CollectSub(def);
        /*LOG(INFO) << term->toString();
        LOG(INFO) << def.toString();*/
        for (auto& [pt, branch]: term->cases) {
            if (isMatch(def, pt)) {
                auto binds = bindPattern(def, pt);
                auto res = branch;
                for (int i = int(binds.size()) - 1; i >= 0; --i) {
                    auto& [name, bind] = binds[i];
                    res = collectSubst(res, name, bind, collector);
                }
                CollectSub2(res);
                return res_res;
            }
        }
        throw SemanticsError();
    }
    CollectHead(Tuple) {
        DataList fields;
        for (auto& field: term->fields) {
            CollectSub2(field);
            fields.push_back(field_res);
        }
        return Data(std::make_shared<VTuple>(fields));
    }
    CollectHead(Var) {
        auto def = ctx->getTerm(term->name);
        CollectSub2(def);
        return def_res;
    }
    CollectHead(If) {
        CollectSub(c);
        auto* bv = dynamic_cast<VBool*>(c.get());
        if (bv->w) {
            CollectSub(t); return t;
        } else {
            CollectSub(f); return f;
        }
    }
    CollectHead(LabeledAlign) {
        std::unordered_map<std::string, Data> inp = term->subst_info;
        CollectSub(content);
        collector->add(term->id, inp, content);
        return content;
    }
    CollectHead(LabeledLabel) {
        CollectSub(content);
        return Data(std::make_shared<VLabeledCompress>(content, term->id));
    }
    CollectHead(UnLabel) {
        CollectSub(content);
        auto* cv = dynamic_cast<VCompress*>(content.get());
        assert(cv); return cv->content;
    }

    Data _collectExample(const Term &term, Context *ctx, IncreExampleCollector* collector) {
        // std::cout << std::endl;
        // LOG(INFO) << "Collect " << term->toString();
        switch (term->getType()) {
            case TermType::VALUE: CollectCase(Value);
            case TermType::PROJ: CollectCase(Proj);
            case TermType::LET: CollectCase(Let);
            case TermType::ABS: CollectCase(Abs);
            case TermType::APP: CollectCase(App);
            case TermType::LABEL: CollectCase(LabeledLabel);
            case TermType::UNLABEL: CollectCase(UnLabel);
            case TermType::ALIGN: CollectCase(LabeledAlign);
            case TermType::FIX: CollectCase(Fix);
            case TermType::MATCH: CollectCase(Match);
            case TermType::TUPLE: CollectCase(Tuple);
            case TermType::VAR: CollectCase(Var);
            case TermType::IF: CollectCase(If);
            case TermType::WILDCARD: LOG(FATAL) << "Unknown WILDCARD: " << term->toString();
        }
    }
}

void SubstituteBasedExampleCollector::collect(const Term &start, const std::unordered_map<std::string, Data> &_global) {
    current_global = _global;
    for (auto& [name, val]: current_global) {
        ctx->addBinding(name, std::make_shared<TmValue>(val));
    }
    _collectExample(start, ctx, this);
}

IncreExamplePool::~IncreExamplePool() {
    delete generator;
}

namespace {
    void _buildCollectContext(const Command& command, Context* ctx, IncreExampleCollector* collector) {
        switch (command->getType()) {
            case CommandType::DEF_IND: {
                incre::run(command, ctx); return;
            }
            case CommandType::BIND: {
                auto* cb = dynamic_cast<CommandBind*>(command.get());
                switch (cb->binding->getType()) {
                    case BindingType::TYPE: {
                        incre::run(command, ctx);
                        return;
                    }
                    case BindingType::TERM: {
                        auto *term_bind = dynamic_cast<TermBinding *>(cb->binding.get());
                        auto ty = incre::getType(term_bind->term, ctx);
                        // LOG(INFO) << "collect for " << term_bind->term->toString();
                        Data res = _collectExample(term_bind->term, ctx, collector);
                        ctx->addBinding(cb->name, std::make_shared<TmValue>(res), ty);
                        return;
                    }
                    case BindingType::VAR: {
                        auto* var_bind = dynamic_cast<VarTypeBinding*>(cb->binding.get());
                        ctx->addBinding(cb->name, var_bind->type);
                        return;
                    }
                }
            }
            case CommandType::IMPORT: {
                auto* ci = dynamic_cast<CommandImport*>(command.get());
                for (auto& c: ci->commands) _buildCollectContext(c, ctx, collector);
                return;
            }
        }
    }

    Context* _buildCollectContext(ProgramData* program, IncreExampleCollector* collector) {
        auto* ctx = new Context();
        for (auto& command: program->commands) {
            _buildCollectContext(command, ctx, collector);
        }
        return ctx;
    }

    bool _isCompressRelevant(TyData* type) {
        match::MatchTask task;
        task.type_matcher = [](TyData* type, const match::MatchContext& ctx) -> bool {
            return type->getType() == TyType::COMPRESS;
        };
        return match::match(type, task);
    }

    const int KDefaultThreadNum = 8;
}

SubstituteBasedExampleCollector::SubstituteBasedExampleCollector(
        const std::vector<std::unordered_set<std::string>> &_cared_vars, ProgramData *_program):
        IncreExampleCollector(_cared_vars, _program) {
    ctx = _buildCollectContext(_program, this);
}
SubstituteBasedExampleCollector::~SubstituteBasedExampleCollector() noexcept {
    delete ctx;
}

IncreExampleCollector::IncreExampleCollector(const std::vector<std::unordered_set<std::string>> &_cared_vars,
                                             ProgramData* _program): cared_vars(_cared_vars), example_pool(_cared_vars.size()) {
}

IncreExamplePool::IncreExamplePool(const IncreProgram& _program, Env* env,
                                   const std::vector<std::unordered_set<std::string>> &_cared_vars, const CollectorBuilder& _builder):
                                   program(_program), cared_vars(_cared_vars), example_pool(_cared_vars.size()),
                                   is_already_finished(_cared_vars.size(), false), builder(_builder) {
    generator = new FixedPoolFunctionGenerator(env);

    auto label_rule = [](const Term& term, EnvAddress* env, AddressHolder* holder, const ExternalEnvRuleMap& map) -> Data {
        auto* lt = dynamic_cast<TmLabeledLabel*>(term.get()); assert(lt);
        auto res = incre::envRun(lt->content, env, holder, map);
        return Data(std::make_shared<VLabeledCompress>(res, lt->id));
    };
    ExternalEnvRuleMap tmp_ext;
    tmp_ext[TermType::LABEL] = (ExternalEnvRunRule){label_rule};

    ctx = incre::envRun(program.get(), tmp_ext);
    auto* tmp_ctx = new Context();
    for (int command_id = 0; command_id < program->commands.size(); ++command_id) {
        auto& command = program->commands[command_id];
        incre::run(command, tmp_ctx);

        if (command->getType() == CommandType::BIND) {
            auto* cb = dynamic_cast<CommandBind*>(command.get());
            auto type = tmp_ctx->getType(cb->name);
            auto* tmp_type_ctx = new TypeContext(tmp_ctx);
            type = incre::unfoldBasicType(type, tmp_type_ctx);
            delete tmp_type_ctx;
            if (cb->isDecoratedWith(CommandDecorate::INPUT)) {
                if (_isCompressRelevant(type.get())) {
                    LOG(FATAL) << "The input should not be compressed, but get " << type->toString();
                }
                input_list.emplace_back(cb->name, type);
            }
            if (cb->isDecoratedWith(CommandDecorate::START) || (command_id + 1 == program->commands.size() && start_list.empty())) {
                if (_isCompressRelevant(type.get())) {
                    LOG(FATAL) << "The output should not be compressed, but get " << type->toString();
                }
                TyList param_list;
                auto* ty = dynamic_cast<TyArrow*>(type.get());
                while (ty) {
                    param_list.push_back(ty->source);
                    ty = dynamic_cast<TyArrow*>(ty->target.get());
                }
                start_list.emplace_back(cb->name, param_list);
            }
        }
    }

    type_ctx = new TypeContext(tmp_ctx);
    delete tmp_ctx;

    start_dist = std::uniform_int_distribution<int>(0, int(start_list.size()) - 1);
    auto data = env->getConstRef(incre::KExampleThreadName, BuildData(Int, KDefaultThreadNum));
    thread_num = theory::clia::getIntValue(*data);
}

DefaultIncreExamplePool::DefaultIncreExamplePool(const IncreProgram &_program, Env *env,
                                                 const std::vector<std::unordered_set<std::string>> &_cared_vars, const CollectorBuilder& _builder):
                                                 IncreExamplePool(_program, env, _cared_vars, _builder) {
}
void DefaultIncreExamplePool::insertExample(int pos, const IncreExample &new_example) {
    assert(pos < example_pool.size());
    example_pool[pos].push_back(new_example);
}

NoDuplicatedIncreExamplePool::NoDuplicatedIncreExamplePool(const IncreProgram &_program, Env *env,
                                                           const std::vector<std::unordered_set<std::string>> &_cared_vars, const CollectorBuilder& _builder):
        IncreExamplePool(_program, env, _cared_vars, _builder), existing_example_set(_cared_vars.size()) {
}

void NoDuplicatedIncreExamplePool::insertExample(int pos, const IncreExample &new_example) {
    assert(pos < example_pool.size() && pos < existing_example_set.size());
    auto feature = new_example->toString();
    if (existing_example_set[pos].find(feature) == existing_example_set[pos].end()) {
        existing_example_set[pos].insert(feature);
        example_pool[pos].push_back(new_example);
    }
}

#include "istool/basic/config.h"

std::pair<Term, std::unordered_map<std::string, Data>> IncreExamplePool::generateStart() {
    std::unordered_map<std::string, Data> global;
    for (auto& [inp_name, inp_ty]: input_list) {
        auto input_data = generator->getRandomData(inp_ty);
        // ctx->addBinding(inp_name, std::make_shared<TmValue>(input_data));
        global[inp_name] = input_data;
    }
    auto& [start_name, params] = start_list[start_dist(generator->env->random_engine)];
    Term term = std::make_shared<TmVar>(start_name);
    for (auto& param_type: params) {
        auto input_data = generator->getRandomData(param_type);
        term = std::make_shared<TmApp>(term, std::make_shared<TmValue>(input_data));
    }
    return {term, global};
}

namespace {
    const int KMaxFailedAttempt = 500;

    class _MultiThreadCollectorHolder {
    public:
        IncreExamplePool* pool;
        std::mutex input_lock, res_lock;
        int tau_id, target_num, thread_num;
        bool is_finished = false;
        int attempt_num = 0;
        CollectorBuilder builder;
        std::queue<std::pair<Term, std::unordered_map<std::string, Data>>> input_queue;
        TimeGuard* guard;

        _MultiThreadCollectorHolder(IncreExamplePool* _pool, int _tau_id, int _target_num, const CollectorBuilder& _builder, TimeGuard* _guard):
            pool(_pool), tau_id(_tau_id), target_num(_target_num), guard(_guard), builder(_builder), thread_num(pool->thread_num) {
        }
        void collect() {
            std::vector<std::thread> thread_list;

            auto single_thread = [&](IncreExampleCollector* collector, int id) {
                int previous_num = 0;
                while (!guard || guard->getRemainTime() > 0) {
                    /*LOG(INFO) << "Remain time " << guard->getRemainTime();*/
                    if (res_lock.try_lock()) {
                        int pre_size = pool->example_pool[tau_id].size();

                        int total_size = 0;
                        for (auto& example_list: collector->example_pool) {
                            total_size += example_list.size();
                        }
                        pool->merge(collector, guard);
                        collector->clear();
                        if (pool->example_pool[tau_id].size() == pre_size) {
                            attempt_num += previous_num;
                            if (attempt_num >= KMaxFailedAttempt) {
                                is_finished = true;
                                pool->is_already_finished[tau_id] = true;
                            }
                        } else attempt_num = 0;
                        if (pool->example_pool[tau_id].size() >= target_num || is_finished) {
                            res_lock.unlock(); break;
                        }
                        res_lock.unlock();
                    }

                    input_lock.lock();
                    if (input_queue.empty()) {
                        int generate_num = thread_num * 100;
                        for (int i = 0; i < generate_num; ++i) {
                            input_queue.push(pool->generateStart());
                        }
                    }
                    auto [start, global] = input_queue.front();
                    input_queue.pop();
                    input_lock.unlock();

                    int pre_size = collector->example_pool[tau_id].size();
                    collector->collect(start, global);
                    if (collector->example_pool[tau_id].size() != pre_size)
                        attempt_num++;
                }
                return;
            };

            std::vector<IncreExampleCollector*> collector_list;

            for (int i = 0; i < thread_num; ++i) {
                auto* collector = builder(pool->cared_vars, pool->program.get());
                collector_list.push_back(collector);
                thread_list.emplace_back(single_thread, collector, i);
            }
            for (int i = 0; i < thread_num; ++i) {
                thread_list[i].join(); delete collector_list[i];
            }
        }

    };
}

void IncreExamplePool::generateSingleExample() {
    auto [term, global] = generateStart();
    auto* collector = builder(cared_vars, program.get());

    global::recorder.start("collect");
    collector->collect(term, global);
    merge(collector, nullptr);
    global::recorder.end("collect");
}

void IncreExamplePool::generateBatchedExample(int tau_id, int target_num, TimeGuard *guard) {
    if (is_already_finished[tau_id] || target_num < example_pool[tau_id].size()) return;
    if (guard) LOG(INFO) << "Start generate " << guard->getRemainTime();
    auto* holder = new _MultiThreadCollectorHolder(this, tau_id, target_num, builder, guard);
    global::recorder.start("collect");
    holder->collect();
    global::recorder.end("collect");
    if (example_pool[tau_id].size() < target_num) is_already_finished[tau_id] = true;
    if (guard) LOG(INFO) << "Generate finished";
    delete holder;
}

const std::string incre::KExampleThreadName = "KIncreExampleCollectNum";

CollectorBuilder incre::getCollectorBuilder(const CollectorType &type) {
    switch (type) {
        case CollectorType::SUBSTITUE:
            return [](const std::vector<std::unordered_set<std::string>>& cared_vars, ProgramData* program) {
                return new SubstituteBasedExampleCollector(cared_vars, program);
            };
        case CollectorType::ENV:
            return [](const std::vector<std::unordered_set<std::string>>& cared_vars, ProgramData* program) {
                return new EnvBasedExampleCollector(cared_vars, program);
            };
    }
}

DataList incre::evaluateAll(const std::vector<FullExample> &examples, const IncreProgram &program) {
    DataList result(examples.size());
    int current_pos = 0, n = examples.size();
    std::mutex pos_lock;

    auto single_thread = [&]() {
        auto* holder = new AddressHolder();
        auto* ctx = new EnvContext(holder);
        ctx->start = holder->extend(ctx->start, "al_inf", BuildData(Int, 100));
        for (auto& command: program->commands) {
            incre::envRun(command, ctx);
        }
        std::queue<int> task_queue;
        std::vector<std::pair<int, Data>> local_result;
        while (1) {
            if (task_queue.empty()) {
               pos_lock.lock();
               if (current_pos == n) {
                   pos_lock.unlock(); break;
               }
               int pre = current_pos; current_pos = std::min(n, current_pos + 100);
               pos_lock.unlock();
               for (int i = pre; i < current_pos; ++i) {
                   task_queue.push(i);
               }
            }
            int k = task_queue.front(); task_queue.pop();
            auto& [term, global] = examples[k];
            ctx->initGlobal(global);
            int pre_size = holder->address_list.size();
            result[k] = incre::envRun(term, ctx->start, holder);
            holder->recover(pre_size);
        }
        return;
    };

    std::vector<std::thread> thread_list;

    for (int i = 0; i < KDefaultThreadNum; ++i) {
        thread_list.emplace_back(single_thread);
    }
    for (int i = 0; i < KDefaultThreadNum; ++i) {
        thread_list[i].join();
    }
    return result;
}