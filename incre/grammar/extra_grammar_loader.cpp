//
// Created by pro on 2023/5/7.
//

#include "istool/incre/grammar/incre_component_collector.h"
#include "istool/basic/config.h"
#include "istool/incre/io/incre_from_json.h"
#include "glog/logging.h"
#include "istool/incre/trans/incre_trans.h"
#include "istool/incre/language/incre_lookup.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/tmp_info.h"

using namespace incre::grammar;
using namespace incre;

namespace {
    void _clearCommandId(ComponentPool& pool) {
        for (auto& command: pool.compress_list) command->command_id = -1;
        for (auto& command: pool.comb_list) command->command_id = -1;
        for (auto& command: pool.align_list) command->command_id = -1;
    }

    std::string _getAutoLifterPath(bool is_syc) {
        if (is_syc) return config::KSourcePath + "incre-tests/autolifter/syc-autolifter-base.f";
        return config::KSourcePath + "incre-tests/autolifter/autolifter-base.f";
    }

    CommandList _loadAutoLifterExtraComponentInResult() {
        auto path = _getAutoLifterPath(false);
        auto program = incre::parseFromF(path, false);
        CommandList result;
        for (auto& command: program->commands) {
            if (command->isDecoratedWith(CommandDecorate::SYN_COMBINE) || command->isDecoratedWith(CommandDecorate::SYN_COMPRESS)) {
                result.push_back(command);
            }
        }
        return result;
    }

    ComponentPool _loadAutoLifterOperator(EnvContext* env_ctx, TypeContext* type_ctx, bool is_syc) {
        auto path = _getAutoLifterPath(is_syc);
        auto program = incre::parseFromF(path, false);
        auto component_pool = incre::grammar::collector::collectComponentFromLabel(env_ctx, type_ctx, program.get());
        _clearCommandId(component_pool);
        return component_pool;
    }

    class TypedTmpSemantics: public NormalSemantics {
    public:
        TypedTmpSemantics(const PType& type, const std::string& name): NormalSemantics(name, type, (TypeList){}) {
        }
        virtual Data run(DataList&& inp_list, ExecuteInfo* info) {
            auto* tinfo = dynamic_cast<TmpExecuteInfo*>(info);
            if (!tinfo) LOG(FATAL) << "TmpExecuteInfo is required";
            return tinfo->get(name, true);
        }
    };

    PType _getSemType(Semantics* sem) {
        auto* ts = dynamic_cast<TypedSemantics*>(sem);
        assert(ts); return ts->oup_type;
    }

    bool _isInclude(Type* type, const std::string& name) {
        auto i_type = incre::typeToIncre(type);
        incre::match::MatchTask task;
        task.type_matcher = [&](TyData* ty, const incre::match::MatchContext& ctx)-> bool {
            auto* ti = dynamic_cast<TyInductive*>(ty);
            return ti && ti->name == name;
        };
        return incre::match::match(i_type.get(), task);
    }

    void _extractComponents(const Data& data, DataList& res) {
        auto* vt = dynamic_cast<VTuple*>(data.get());
        if (vt) {
            for (auto& field: vt->elements) {
                _extractComponents(field, res);
            }
            return;
        }
        auto* tu = dynamic_cast<VUnit*>(data.get());
        if (tu) return;
        res.push_back(data);
    }

    DataList _extractComponents(const Data& data) {
        DataList res; _extractComponents(data, res);
        return res;
    }

    void _extractComponents(const Ty& type, TyList& res) {
        switch (type->getType()) {
            case TyType::ARROW:  LOG(FATAL) << "Unexpected type " << type->toString();
            case TyType::UNIT: return;
            case TyType::VAR:
            case TyType::IND:
            case TyType::INT:
            case TyType::COMPRESS:
            case TyType::BOOL: {
                res.push_back(type); return;
            }
            case TyType::TUPLE: {
                auto* tt = dynamic_cast<TyTuple*>(type.get());
                for (auto& sub_type: tt->fields) {
                    _extractComponents(sub_type, res);
                }
                return;
            }
        }
    }

    TyList _extractComponents(const Ty& type) {
        TyList res; _extractComponents(type, res);
        return res;
    }

    SymbolContext _removeInclude(const SymbolContext& ctx, const std::string& name) {
        SymbolContext res;
        for (auto& sem: ctx) {
            auto sem_type = _getSemType(sem.get());
            if (_isInclude(sem_type.get(), name)) continue;
            res.push_back(sem);
        }
        return res;
    }

    std::vector<std::string> _assignTmpNames(const SymbolContext& ctx, const TyList& tmp_vars) {
        std::unordered_map<std::string, int> count_map;
        for (auto& sem: ctx) {
            auto* st = dynamic_cast<TypedTmpSemantics*>(sem.get());
            if (st) {
                auto in_type = incre::typeToIncre(st->oup_type.get());
                count_map[in_type->toString()]++;
            }
        }
        std::vector<std::string> name_list;
        for (auto& tmp_type: tmp_vars) {
            auto feature = tmp_type->toString();
            int id = count_map[feature]; count_map[feature] += 1;
            auto name = feature + "@" + std::to_string(id);
            name_list.push_back(name);
        }
        return name_list;
    }

    SymbolContext _insertTmps(const SymbolContext& ctx, const TyList& tmp_vars) {
        SymbolContext res = ctx; auto name_list = _assignTmpNames(ctx, tmp_vars);
        for (int i = 0; i < tmp_vars.size(); ++i) {
            res.push_back(std::make_shared<TypedTmpSemantics>(incre::typeFromIncre(tmp_vars[i]), name_list[i]));
        }
        return res;
    }

    class FoldSemantics: public Semantics {
    public:
        std::unordered_map<std::string, int> cons_map;
        std::vector<std::vector<std::string>> tmp_name_list;
        std::vector<std::vector<bool>> rec_param_list;
        FoldSemantics(const std::string& name, const std::unordered_map<std::string, int>& _cons_map,
                      const std::vector<std::vector<std::string>>& _tmp_name_list,
                      const std::vector<std::vector<bool>>& _rec_param_list):
                      Semantics(name), cons_map(_cons_map), tmp_name_list(_tmp_name_list), rec_param_list(_rec_param_list) {
        }
        Data run(VInductive* value, TmpExecuteInfo* info, const ProgramList& sub_list) {
            assert(value && cons_map.find(value->name) != cons_map.end());
            auto cons_id = cons_map[value->name];
            auto& tmp_name = tmp_name_list[cons_id];
            auto& rec_param = rec_param_list[cons_id];
            auto components = _extractComponents(value->content);
            assert(components.size() == tmp_name.size() && tmp_name.size() == rec_param.size());
            for (int i = 0; i < components.size(); ++i) {
                if (rec_param[i]) components[i] = run(dynamic_cast<VInductive*>(components[i].get()), info, sub_list);
            }
            for (int i = 0; i < tmp_name.size(); ++i) {
                info->set(tmp_name[i], components[i]);
            }
            auto res = sub_list[cons_id]->run(info);
            for (int i = 0; i < tmp_name.size(); ++i) {
                info->clear(tmp_name[i]);
            }
            return res;
        }
        virtual Data run(const std::vector<std::shared_ptr<Program>>& sub_list, ExecuteInfo* info) {
            assert(sub_list.size() == tmp_name_list.size() + 1);
            auto res = sub_list[tmp_name_list.size()]->run(info);
            auto *t_info = dynamic_cast<TmpExecuteInfo *>(info);
            assert(t_info);
            return run(dynamic_cast<VInductive*>(res.get()), t_info, sub_list);
        }
    };

    PSemantics _constructFoldSemantics(const std::vector<std::vector<std::string>>& tmp_name_list, TyInductive* ti, const Ty& oup_type) {
        std::unordered_map<std::string, int> cons_map;
        std::vector<std::vector<bool>> rec_param_list;
        auto name = "fold[" + ti->toString() + "->" + oup_type->toString() + "]";
        for (int i = 0; i < ti->constructors.size(); ++i) {
            auto &[cons_name, cons_ty] = ti->constructors[i];
            cons_map[cons_name] = i;
            auto components = _extractComponents(cons_ty);
            std::vector<bool> rec_param;
            for (int j = 0; j < components.size(); ++j) {
                rec_param.push_back(components[j]->getType() == TyType::VAR);
            }
            rec_param_list.push_back(rec_param);
        }
        return std::make_shared<FoldSemantics>(name, cons_map, tmp_name_list, rec_param_list);
    }

    class FoldComponent: public SynthesisComponent {
    public:
        Ty _type; TyInductive* type;
        TyList target_types;
        FoldComponent(const Ty& __type, const TyList& _targets): SynthesisComponent(-1, "fold"),
            _type(__type), target_types(_targets) {
            assert(__type->getType() == TyType::IND);
            type = dynamic_cast<TyInductive*>(_type.get());
        }
        virtual void insertComponent(const GrammarBuilder &builder) {
            for (auto& info: builder.info_list) {
                auto* ti = dynamic_cast<incre::TIncreInductive*>(info.type.get());
                if (!ti || ti->type->name != type->name) continue;
                for (auto& target: target_types) {
                    auto* res_symbol = builder.getSymbol(info.context, incre::typeFromIncre(target));
                    if (!res_symbol) continue;
                    std::vector<std::vector<std::string>> tmp_name_list;
                    NTList param_list; bool is_valid = true;
                    for (auto &[cons_name, cons_type]: type->constructors) {
                        auto param_type = subst(cons_type, type->name, target);
                        TyList components = _extractComponents(param_type);
                        auto removed_ctx = _removeInclude(info.context, type->name);
                        auto name_list = _assignTmpNames(removed_ctx, components);
                        tmp_name_list.push_back(name_list);
                        auto new_ctx = _insertTmps(removed_ctx, components);
                        auto* param_symbol = builder.getSymbol(new_ctx, incre::typeFromIncre(target));
                        if (!param_symbol) {
                            is_valid = false; break;
                        }
                        param_list.push_back(param_symbol);
                    }
                    param_list.push_back(info.symbol);
                    if (is_valid) {
                        auto sem = _constructFoldSemantics(tmp_name_list, type, target);
                        res_symbol->rule_list.push_back(new ConcreteRule(sem, param_list));
                    }
                }
            }
        }
        virtual void extendContext(GrammarBuilder& builder) {
            for (int ctx_id = 0; ctx_id < builder.contexts.size(); ++ctx_id) {
                auto ctx = builder.contexts[ctx_id];
                bool is_contain = false;
                for (auto& sem: ctx) {
                    auto sem_type = _getSemType(sem.get());
                    if (_isInclude(sem_type.get(), type->name)) {
                        is_contain = true; break;
                    }
                }
                if (!is_contain) continue;
                for (auto& target: target_types) {
                    for (auto &[_, cons_type]: type->constructors) {
                        auto param_type = subst(cons_type, type->name, target);
                        TyList components = _extractComponents(param_type);
                        auto new_ctx = _insertTmps(_removeInclude(ctx, type->name), components);
                        builder.insertContext(new_ctx);
                    }
                }
            }
        }
        virtual void extendNTMap(GrammarBuilder &builder) {
            for (auto& info: builder.info_list) {
                auto *ti = dynamic_cast<incre::TIncreInductive *>(info.type.get());
                if (!ti || ti->type->name != type->name) continue;
                for (auto& target: target_types) {
                    builder.insertInfo(info.context, incre::typeFromIncre(target));
                }
            }
        }
        virtual Term tryBuildTerm(const PSemantics& sem, const TermList& term_list) {
            return {};
        }
        virtual ~FoldComponent() = default;
    };

    bool _isMutualRec(TyData* type, const std::string& name, bool is_inside_other) {
        switch (type->getType()) {
            case TyType::VAR: {
                auto* tv = dynamic_cast<TyVar*>(type);
                return tv->name == name && is_inside_other;
            }
            case TyType::UNIT:
            case TyType::INT:
            case TyType::BOOL:
                return false;
            case TyType::ARROW:
                LOG(FATAL) << "Unexpected arrow type";
            case TyType::TUPLE: {
                auto* tt = dynamic_cast<TyTuple*>(type);
                for (int i = 0; i < tt->fields.size(); ++i) {
                    if (_isMutualRec(tt->fields[i].get(), name, is_inside_other)) return true;
                }
                return false;
            }
            case TyType::IND: {
                auto* ti = dynamic_cast<TyInductive*>(type);
                for (auto& [cons_name, cons_type]: ti->constructors) {
                    if (_isMutualRec(cons_type.get(), name, true)) return true;
                }
                return false;
            }
            case TyType::COMPRESS: {
                auto* tc = dynamic_cast<TyCompress*>(type);
                return _isMutualRec(tc->content.get(), name, is_inside_other);
            }
        }
    }

    bool _isMutualRec(TyInductive* type) {
        for (auto& [_, cons_type]: type->constructors) {
            if (_isMutualRec(cons_type.get(), type->name, false)) return true;
        }
        return false;
    }

    ComponentPool _loadFoldOperator(TypeContext *type_ctx) {
        TyList ind_list;
        std::unordered_set<std::string> ind_name_map;
        for (auto& [name, ty]: type_ctx->binding_map) {
            if (ty->getType() != TyType::IND) continue;
            auto full_type = unfoldBasicType(ty, type_ctx);
            assert(full_type->getType() == TyType::IND);
            auto* ti = dynamic_cast<TyInductive*>(full_type.get());
            if (_isMutualRec(ti)) continue;
            if (ind_name_map.find(ti->name) == ind_name_map.end()) {
                ind_name_map.insert(ti->name);
                ind_list.push_back(full_type);
            }
        }
        for (int i = 0; i < ind_list.size(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (isTypeEqual(ind_list[i], ind_list[j], type_ctx)) {
                    LOG(INFO) << "Exceptional equal type " << ind_list[i]->toString() << " " << ind_list[j]->toString();
                }
            }
        }
        ComponentPool res;

        TyList target_list = {std::make_shared<TyInt>(), std::make_shared<TyBool>()};
        LOG(INFO) << "Load fold operators";
        for (auto& ind_type: ind_list) {
            LOG(INFO) << "Load for " << ind_type->toString();
            res.align_list.push_back(std::make_shared<FoldComponent>(ind_type, target_list));
        }

        return res;
    }
}

CommandList collector::extractExtraComponentInResult(const std::string &extra_name) {
    if (extra_name == "AutoLifter" || extra_name == "DeepCoder") {
        return _loadAutoLifterExtraComponentInResult();
    }
    if (extra_name == "Fold" || extra_name == "SycAutoLifter") {
        return {};
    }
    LOG(FATAL) << "Unknown extra name";
}

ComponentPool collector::collectExtraOperators(EnvContext* env_ctx, TypeContext *type_ctx, const std::string &extra_name) {
    if (extra_name == "AutoLifter" || extra_name == "DeepCoder") {
        return _loadAutoLifterOperator(env_ctx, type_ctx, false);
    }
    if (extra_name == "SycAutoLifter") {
        return _loadAutoLifterOperator(env_ctx, type_ctx, true);
    }
    if (extra_name == "Fold") {
        return _loadFoldOperator(type_ctx);
    }
    LOG(FATAL) << "Unknown extra grammar " << extra_name;
}

namespace {
    void _loadAutoLifterOperators(EnvContext* env_ctx, TypeContext* type_ctx, bool is_syc) {
        auto path = _getAutoLifterPath(is_syc);
        auto program = parseFromF(path, false);
        auto* extra_ctx = run(program);
        LOG(INFO) << "Loading AutoLifter operators";
        for (auto& [name, binding]: extra_ctx->binding_map) {
            if (type_ctx->binding_map.find(name) != type_ctx->binding_map.end()) {
                LOG(WARNING) << "Duplicated operator " << name;
            } else {
                auto* tb = dynamic_cast<TermBinding*>(binding.get());
                if (!tb) continue;
                type_ctx->bind(name, tb->type);
            }
        }
        incre::envRun(program.get(), env_ctx);
    }
}

void collector::loadExtraOperator(EnvContext* env_ctx, TypeContext *type_ctx, Env* env, const std::string &extra_name) {
    if (extra_name == "AutoLifter" || extra_name == "DeepCoder") {
        _loadAutoLifterOperators(env_ctx, type_ctx, false); return;
    }
    if (extra_name == "SycAutoLifter") {
        _loadAutoLifterOperators(env_ctx, type_ctx, true); return;
    }
    if (extra_name == "Fold") {
        ext::ho::registerTmpExecuteInfo(env); return;
    }
    LOG(FATAL) << "Unknown extra grammar " << extra_name;
}