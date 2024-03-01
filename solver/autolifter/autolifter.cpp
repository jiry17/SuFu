//
// Created by pro on 2022/1/22.
//

#include "istool/solver/autolifter/autolifter.h"
#include "istool/solver/autolifter/basic/occam_verifier.h"
#include "istool/ext/deepcoder/data_util.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "glog/logging.h"
#include <queue>

AutoLifter::AutoLifter(LiftingTask *task, const SFSolverBuilder &_sf_builder, const SolverBuilder& _sc_builder):
    LiftingSolver(task), sf_builder(_sf_builder), sc_builder(_sc_builder) {
}

bool AutoLifter::isOccur(const PProgram &p, const ProgramList& pool) {
    // todo: add a semantics check
    auto feature = p->toString();
    for (const auto& comp: pool) {
        if (comp->toString() == feature) return true;
    }
    return false;
}

namespace {
    void _missCacheError(Program* p) {
        LOG(FATAL) << "Program " << p->toString() << " is not cached";
    }

    Data _constructInp(Type* type, const Data& base, const DataList& h_inp_list, const DataList& f_inp_list) {
        auto* pt = dynamic_cast<TProduct*>(type);
        if (pt) {
            std::vector<ProductValue*> h_value_list, f_value_list;
            for (const auto& h_data: h_inp_list) {
                auto* pv = dynamic_cast<ProductValue*>(h_data.get());
                if (!pv || pv->elements.size() != pt->sub_types.size()) {
                    LOG(FATAL) << h_data.toString() << " does not match functor " << type->getName();
                }
                h_value_list.push_back(pv);
            }
            for (const auto& f_data: f_inp_list) {
                auto* pv = dynamic_cast<ProductValue*>(f_data.get());
                if (!pv || pv->elements.size() != pt->sub_types.size()) {
                    LOG(FATAL) << f_data.toString() << " does not match functor " << type->getName();
                }
                f_value_list.push_back(pv);
            }
            auto* bv = dynamic_cast<ProductValue*>(base.get());
            if (!bv || bv->elements.size() != pt->sub_types.size()) {
                LOG(FATAL) << base.toString() << " does not match functor " << type->getName();
            }
            DataList elements;
            for (int i = 0; i < pt->sub_types.size(); ++i) {
                DataList sub_h_list, sub_f_list;
                for (auto* h_value: h_value_list) sub_h_list.push_back(h_value->elements[i]);
                for (auto* f_value: f_value_list) sub_f_list.push_back(f_value->elements[i]);
                elements.push_back(_constructInp(pt->sub_types[i].get(), bv->elements[i], sub_h_list, sub_f_list));
            }
            return BuildData(Product, elements);
        }
        auto* vt = dynamic_cast<TVar*>(type);
        if (vt) {
            auto l = BuildData(Product, h_inp_list);
            auto r = BuildData(Product, f_inp_list);
            DataList elements = {l, r};
            return BuildData(Product, elements);
        }
        for (const auto& h_inp: h_inp_list) {
            if (!(h_inp == base)) LOG(FATAL) << h_inp.toString() << " does not match base " << base.toString();
        }
        for (const auto& f_inp: f_inp_list) {
            if (!(f_inp == base)) LOG(FATAL) << f_inp.toString() << " does not match base " << base.toString();
        }
        return base;
    }

    DataList _getAllComponent(const Data& d) {
        auto* pv = dynamic_cast<ProductValue*>(d.get());
        if (!pv) return {d};
        DataList res;
        assert(pv);
        for (auto& sub_d: pv->elements) {
            for (const auto& sub_res: _getAllComponent(sub_d)) {
                res.push_back(sub_res);
            }
        }
        return res;
    }

    class _SCExampleGenerator: public ExampleGenerator {
    public:
        PartialLiftingTask* task;
        DataList* oup_cache;
        std::vector<std::pair<Program*, DataList*>> h_inp_list, f_inp_list;
        int id = 0;
        _SCExampleGenerator(PartialLiftingTask* _task, const PProgram& f): task(_task) {
            oup_cache = task->info->getModCache(task->p.get());
            if (!oup_cache) _missCacheError(task->p.get());
            for (const auto& h_comp: ext::ho::splitProduct(task->h)) {
                auto cache = task->info->getFMapCache(h_comp.get());
                if (!cache) _missCacheError(h_comp.get());
                h_inp_list.emplace_back(h_comp.get(), cache);
            }
            for (const auto& f_comp: ext::ho::splitProduct(f)) {
                auto cache = task->info->getFMapCache(f_comp.get());
                if (!cache) _missCacheError(f_comp.get());
                f_inp_list.emplace_back(f_comp.get(), cache);
            }
        }
        virtual ExampleList generateExamples(TimeGuard* guard) {
            ++id;
            int size = task->info->example_space->extendExampleList(id, guard);
            if (size < id) throw TimeOutError();
            assert(task->info->extendModCache(task->p.get(), oup_cache, id) == id);
            for (auto& h_inp: h_inp_list) {
                assert(task->info->extendFMapCache(h_inp.first, h_inp.second, id) == id);
            }
            for (auto& f_inp: f_inp_list) {
                assert(task->info->extendFMapCache(f_inp.first, f_inp.second, id) == id);
            }
            int pos = id - 1;
            auto oup = oup_cache->at(pos);
            if (oup.isNull()) return {};

            DataList base_h_list, base_f_list;
            for (auto& info: h_inp_list) base_h_list.push_back(info.second->at(pos));
            for (auto& info: f_inp_list) base_f_list.push_back(info.second->at(pos));
            auto inp = _constructInp(task->info->F.get(), task->info->example_space->getExample(pos)[0],
                    base_h_list, base_f_list);
            auto example = _getAllComponent(inp);
            example.push_back(oup);
            return {example};
        }
        virtual ~_SCExampleGenerator() = default;
    };

    PSynthInfo _buildCombinatorInfo(Grammar* g, const TypeList& inp_type) {
        auto oup_type = g->start->type;
        for (auto* symbol: g->symbol_list) {
            for (auto* rule: symbol->rule_list) {
                auto* ps = grammar::getParamSemantics(rule);
                if (!ps) continue;
                assert(ps->id < inp_type.size() && type::equal(ps->oup_type, inp_type[ps->id]));
            }
        }
        return std::make_shared<SynthInfo>("c", (TypeList){inp_type}, oup_type, g);
    }

    PStreamedExampleSpace _buildCombinatorExampleSpace(const PExampleGenerator& g, SynthInfo* info, Env* env) {
        ProgramList inp_list;
        for (int i = 0; i < info->inp_type_list.size(); ++i) inp_list.push_back(program::buildParam(i, info->inp_type_list[i]));
        auto oup = program::buildParam(info->inp_type_list.size(), info->oup_type);

        auto l = std::make_shared<Program>(std::make_shared<InvokeSemantics>(info->name, env), inp_list);
        ProgramList sub_list({l, oup});
        auto cons_program = std::make_shared<Program>(env->getSemantics("="), sub_list);
        return std::make_shared<StreamedIOExampleSpace>(cons_program, info->name, inp_list, oup, g, env);
    }

    std::pair<TypeList, ProgramList> _splitType(const PType& type, const PProgram& root) {
        auto* pt = dynamic_cast<TProduct*>(type.get());
        if (!pt) return {{type}, {root}};
        TypeList res_type; ProgramList res_prog;
        for (int i = 0; i < pt->sub_types.size(); ++i) {
            auto sub_prog = std::make_shared<Program>(std::make_shared<AccessSemantics>(i), (ProgramList){root});
            auto sub_res = _splitType(pt->sub_types[i], sub_prog);
            for (auto& t: sub_res.first) res_type.push_back(t);
            for (auto& p: sub_res.second) res_prog.push_back(p);
        }
        return {res_type, res_prog};
    }

    std::pair<TypeList, ProgramList> _splitType(const PType& type) {
        PProgram root = program::buildParam(0, type);
        return _splitType(type, root);
    }
}

PProgram AutoLifter::synthesisCombinatorForPartial(PartialLiftingTask *task, const PProgram& f, TimeGuard *guard) {
    LOG(INFO) << "Merge for " << task->p->toString() <<"  "<< task->h->toString() << " " << f->toString();
    auto inp_type = solver::autolifter::getCInputType(task->info->F, task->h.get(), f.get(), task->env.get());

    auto split_info = _splitType(inp_type);
    auto component_type_list = split_info.first; auto component_prog_list = split_info.second;

    auto* g = task->info->c_builder->buildGrammar(task->p.get(), component_type_list);
    auto info = _buildCombinatorInfo(g, component_type_list);

    auto example_generator = std::make_shared<_SCExampleGenerator>(task, f);
    auto example_space = _buildCombinatorExampleSpace(example_generator, info.get(), task->env.get());
    auto* spec = new Specification({info}, task->env, example_space);
    auto* v = new OccamVerifier(example_space.get());
    auto* sc_solver = sc_builder(spec, v);
    auto c = sc_solver->synthesis(guard)["c"];
    c = ext::ho::removeAccessProd(program::rewriteParam(c, component_prog_list));
    delete spec;
    delete v;
    delete sc_solver;
    return c;
}

SingleLiftingRes AutoLifter::synthesisSinglePartial(PartialLiftingTask *task, TimeGuard* guard) {
    auto* sf_solver = sf_builder(task);
    auto res = sf_solver->synthesis(guard);
    auto f = res.second;
    delete sf_solver;

    task->h = res.first;
    auto c = synthesisCombinatorForPartial(task, f, guard);
    LiftingResInfo res_info(task->info->F, c, task->info->m);
    return {task->p, task->h, f, res_info};
}

LiftingRes AutoLifter::synthesisPartial(const PProgram &p, const PProgram &h, TimeGuard *guard) {
    std::vector<SingleLiftingRes> sub_res_list;
    ProgramList pool = ext::ho::splitProduct(h), f_comp_list;
    for (const auto& info: task->info_list) {
        TimeCheck(guard);
        auto* partial_task = new PartialLiftingTask(info, p, ext::ho::buildProduct(pool), task->f_info, task->env);
        auto sub_res = synthesisSinglePartial(partial_task, guard);
        for (auto& f_comp: ext::ho::splitProduct(sub_res.f)) {
            if (!isOccur(f_comp, pool)) {
                pool.push_back(f_comp); f_comp_list.push_back(f_comp);
            }
        }
        sub_res_list.push_back(sub_res);
        // todo: Figure out why cannot delete partial task;
        // delete partial_task;
    }

    auto f = ext::ho::mergeProduct(f_comp_list);
    std::vector<LiftingResInfo> res_info;
    for (const auto& sub_res: sub_res_list) {
        auto new_c = solver::autolifter::rewriteCombinator(sub_res.info.F.get(), sub_res.h, sub_res.f, sub_res.info.c, h, f);
        res_info.emplace_back(sub_res.info.F, new_c, sub_res.info.m);
    }
    return {p, h, f, res_info, task->env};
}

LiftingRes AutoLifter::synthesis(TimeGuard *guard) {
    ProgramList f_component, target_list, pool;
    target_list = ext::ho::splitProduct(task->p);
    int p_component_num = target_list.size();
    pool = ext::ho::splitProduct(task->h);

    std::vector<LiftingRes> sub_res_list;

    for (int i = 0; i < target_list.size(); ++i) {
        TimeCheck(guard);
        auto partial_res = synthesisPartial(target_list[i], ext::ho::buildProduct(pool), guard);
        LOG(INFO) << "partial res " << i << ": " << partial_res.toString();
        for (auto& f_comp: ext::ho::splitProduct(partial_res.f)) {
            if (!isOccur(f_comp, pool)) {
                f_component.push_back(f_comp);
                target_list.push_back(f_comp);
                pool.push_back(f_comp);
            }
        }
        sub_res_list.push_back(partial_res);
    }

    auto f = f_component.size() != 1 ? ext::ho::buildProduct(f_component) : f_component[0];

    auto construct_c = [&](const ProgramList& comp_list) -> PProgram {
        PProgram l;
        if (!ext::ho::isProductProgram(task->p.get())) l = comp_list[0];
        else {
            ProgramList sub_list;
            for (int i = 0; i < p_component_num; ++i) sub_list.push_back(comp_list[i]);
            l = ext::ho::buildProduct(sub_list);
        }
        PProgram r;
        if (!ext::ho::isProductProgram(f.get())) r = comp_list[p_component_num];
        else {
            ProgramList sub_list;
            for (int i = p_component_num; i < comp_list.size(); ++i) sub_list.push_back(comp_list[i]);
            r = ext::ho::buildProduct(sub_list);
        }
        return ext::ho::buildProduct({l, r});
    };

    std::vector<LiftingResInfo> info_list;
    for (int i = 0; i < task->info_list.size(); ++i) {
        ProgramList comp_list;
        for (int j = 0; j < sub_res_list.size(); ++j) {
            auto& sub_res = sub_res_list[j];
            auto& info = sub_res.info_list[i];
            auto c_comp = solver::autolifter::rewriteCombinator(task->info_list[i]->F.get(), sub_res.h,
                    sub_res.f, info.c, task->h, f);
            comp_list.push_back(c_comp);
        }
        info_list.emplace_back(task->info_list[i]->F, construct_c(comp_list), task->info_list[i]->m);
    }

    return {task->p, task->h, f, info_list, task->env};
}