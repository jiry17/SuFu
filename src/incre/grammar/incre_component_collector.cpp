//
// Created by pro on 2023/4/5.
//

#include "istool/incre/grammar/incre_component_collector.h"
#include <iostream>
#include "glog/logging.h"

using namespace incre;
using namespace incre::grammar;

SymbolInfo::SymbolInfo(const SymbolContext &_context, const PType &_type, NonTerminal *_symbol):
  context(_context), type(_type), symbol(_symbol) {
}

GrammarBuilder::GrammarBuilder(const SymbolContext &init_context): contexts({init_context}) {
}

std::vector<SymbolInfo> GrammarBuilder::getSymbols(const std::function<bool(const SymbolInfo &)> &filter) const {
    std::vector<SymbolInfo> res;
    for (auto& info: info_list) {
        if (filter(info)) res.push_back(info);
    }
    return res;
}

namespace {
    std::string _context2String(const SymbolContext& context) {
        std::vector<std::string> name_list;
        for (auto& sem: context) {
            name_list.push_back(sem->getName());
        }
        std::sort(name_list.begin(), name_list.end());
        std::string res = "[";
        for (int i = 0; i < name_list.size(); ++i) {
            if (i) res += ",";
            res += name_list[i];
        }
        return res + "]";
    }
}

std::vector<SymbolInfo> GrammarBuilder::getSymbols(const PType &type) const {
    auto filter = [type](const SymbolInfo& info) {
        return type->getName() == info.type->getName();
    };
    auto res = getSymbols(filter);
    return res;
}

NonTerminal *GrammarBuilder::getSymbol(const SymbolContext &context, const PType &type) const {
    auto feature = _context2String(context) + "@" + type->getName();
    auto it = info_map.find(feature);
    if (it == info_map.end()) return nullptr;
    return info_list[it->second].symbol;
}

void GrammarBuilder::insertContext(const SymbolContext &context) {
    auto feature = _context2String(context);
    for (auto& existing: contexts) {
        if (_context2String(existing) == feature) return;
    }
    contexts.push_back(context);
}

void GrammarBuilder::insertInfo(const SymbolContext &context, const PType &type) {
    auto feature = _context2String(context) + "@" + type->getName();
    if (info_map.find(feature) == info_map.end()) {
        auto* symbol = new NonTerminal(feature, type);
        info_map[feature] = info_list.size();
        info_list.emplace_back(context, type, symbol);
    }
}

void GrammarBuilder::insertTypeForAllContext(const PType &type) {
    for (auto& context: contexts) {
        insertInfo(context, type);
    }
}

SynthesisComponent::SynthesisComponent(int _command_id, const std::string& _name): command_id(_command_id), name(_name){
}

ContextFreeSynthesisComponent::ContextFreeSynthesisComponent(int command_id, const std::string& _name): SynthesisComponent(command_id, _name) {
}
void ContextFreeSynthesisComponent::extendContext(GrammarBuilder &builder) {
    return;
}

#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/ext/deepcoder/data_type.h"

IncreComponent::IncreComponent( const std::string &_name, const PType &_type, const Data &_data, const Term& _term, int _command_id, bool _is_partial, bool _is_parallel):
        ContextFreeSynthesisComponent(_command_id, _name), data(_data), term(_term), is_partial(_is_partial), is_parallel(_is_parallel) {
    res_type = _type;
    while (1) {
        auto* ta = dynamic_cast<TArrow*>(res_type.get());
        if (!ta) break; assert(ta->inp_types.size() == 1);
        param_types.push_back(ta->inp_types[0]);
        res_type = ta->oup_type;
    }
}
void IncreComponent::extendNTMap(GrammarBuilder &builder) {
    builder.insertTypeForAllContext(res_type);
    if (is_partial) {
        auto partial_output = res_type;
        for (int i = int(param_types.size()) - 1; i >= 0; --i) {
            partial_output = std::make_shared<TArrow>((TypeList){param_types[i]}, partial_output);
            builder.insertTypeForAllContext(partial_output);
        }
    } else {
        auto full_type = res_type;
        for (int i = int(param_types.size()) - 1; i >= 0; --i) {
            full_type = std::make_shared<TArrow>((TypeList){param_types[i]}, full_type);
        }
        builder.insertTypeForAllContext(full_type);
    }
}

class IncreOperatorSemantics: public FullExecutedSemantics {
public:
    Data base;
    AddressHolder* holder;
    IncreOperatorSemantics(const std::string& name, const Data& _base, bool is_parallel):
        FullExecutedSemantics(name), base(_base) {
        if (!is_parallel) holder = new AddressHolder(); else holder = nullptr;
    }
    virtual Data run(DataList&& inp_list, ExecuteInfo* info) {
        Data current = base;
        auto* tmp_holder = holder ? holder : new AddressHolder();
        for (auto& param: inp_list) {
            current = incre::runApp(current, param, tmp_holder);
        }
        if (holder) {
            if (holder->address_list.size() >= 1e5) holder->recover(0);
        } else {
            delete tmp_holder;
        }
        return current;
    }
    virtual ~IncreOperatorSemantics() {
        delete holder;
    }
};

void IncreComponent::insertComponent(const GrammarBuilder &builder) {
    auto sem = std::make_shared<IncreOperatorSemantics>(name, data, is_parallel);
    {
        auto full_type = res_type;
        for (int i = int(param_types.size()) - 1; i >= 0; --i) {
            full_type = std::make_shared<TArrow>((TypeList) {param_types[i]}, full_type);
        }
        // LOG(INFO) << "full type " << name << " " << full_type->getName();
        auto info_list = builder.getSymbols(full_type);
        for (auto& info: info_list) {
            info.symbol->rule_list.push_back(new ConcreteRule(sem, {}));
        }
    }
    if (is_partial) {
        for (int i = 0; i < param_types.size(); ++i) {
            auto partial_res_type = res_type;
            for (int j = int(param_types.size()) - 1; j > i; --j) {
                partial_res_type = std::make_shared<TArrow>((TypeList){param_types[j]}, partial_res_type);
            }
            auto info_list = builder.getSymbols(partial_res_type);
            for (auto& info: info_list) {
                NTList param_list; bool is_valid = true;
                for (int j = 0; j <= i; ++j) {
                    auto* symbol = builder.getSymbol(info.context, param_types[j]);
                    if (!symbol) {
                        is_valid = false; break;
                    }
                    param_list.push_back(symbol);
                }
                if (is_valid) {
                    info.symbol->rule_list.push_back(new ConcreteRule(sem, param_list));
                }
            }
        }
    } else {
        if (param_types.empty()) return;
        auto info_list = builder.getSymbols(res_type);
        for (auto& info: info_list) {
            NTList param_list; bool is_valid = true;
            for (auto &param_type: param_types) {
                auto* param_symbol = builder.getSymbol(info.context, param_type);
                if (!param_symbol) {
                    is_valid = false; break;
                }
                param_list.push_back(param_symbol);
            }
            if (is_valid) {
                assert(param_list.size() == param_types.size());
                info.symbol->rule_list.push_back(new ConcreteRule(sem, param_list));
            }
        }
    }
}
Term IncreComponent::tryBuildTerm(const PSemantics &sem, const TermList &term_list) {
    /*auto* cs = dynamic_cast<ConstSemantics*>(sem.get());
    if (!cs || sem->getName() != name) return nullptr;
    return term;*/
    auto* os = dynamic_cast<IncreOperatorSemantics*>(sem.get());
    if (!os || sem->getName() != name) return nullptr;
    auto res = term;
    for (auto& param: term_list) res = std::make_shared<TmApp>(res, param);
    return res;
}

ConstComponent::ConstComponent(const PType &_type, const DataList &_const_list,
                               const std::function<bool(Value *)> &_is_inside):
                               type(_type), const_list(_const_list), is_inside(_is_inside), ContextFreeSynthesisComponent(-1, _type->getName()) {
}

void ConstComponent::extendNTMap(GrammarBuilder &builder) {
    builder.insertTypeForAllContext(type);
}

void ConstComponent::insertComponent(const GrammarBuilder &builder) {
    for (auto& data: const_list) {
        auto sem = semantics::buildConstSemantics(data);
        auto info_list = builder.getSymbols(type);
        for (auto& info: info_list) {
            info.symbol->rule_list.push_back(new ConcreteRule(sem, {}));
        }
    }
}
Term ConstComponent::tryBuildTerm(const PSemantics &sem, const TermList &term_list) {
    auto* cs = dynamic_cast<ConstSemantics*>(sem.get());
    if (!cs || !is_inside(cs->w.get())) return nullptr;
    return std::make_shared<TmValue>(cs->w);
}

BasicOperatorComponent::BasicOperatorComponent(const std::string &_name, const PSemantics &__sem):
    ContextFreeSynthesisComponent(-1, _name), _sem(__sem){
    sem = dynamic_cast<TypedSemantics*>(_sem.get()); assert(sem);
}

namespace {
    PType _replace(const PType& type) {
        if (dynamic_cast<TVar*>(type.get())) return theory::clia::getTInt();
        return type;
    }
}

void BasicOperatorComponent::extendNTMap(GrammarBuilder &builder) {
    builder.insertTypeForAllContext(_replace(sem->oup_type));
}

void BasicOperatorComponent::insertComponent(const GrammarBuilder &builder) {
    auto info_list = builder.getSymbols(_replace(sem->oup_type));
    for (auto& info: info_list) {
        NTList param_list; bool is_valid = true;
        for (auto& inp_type: sem->inp_type_list) {
            auto* symbol = builder.getSymbol(info.context, _replace(inp_type));
            if (!symbol) {
                is_valid = false; break;
            }
            param_list.push_back(symbol);
        }
        info.symbol->rule_list.push_back(new ConcreteRule(_sem, param_list));
    }
}

Term BasicOperatorComponent::tryBuildTerm(const PSemantics &current_sem, const TermList &term_list) {
    if (current_sem->getName() != name && current_sem->getName() != _sem->getName()) return nullptr;
    auto res = incre::getOperator(name);
    for (int i = 0; i < term_list.size(); ++i) {
        res = std::make_shared<TmApp>(res, term_list[i]);
    }
    return res;
}

IteComponent::IteComponent(): ContextFreeSynthesisComponent(-1, "ite") {
}

void IteComponent::extendNTMap(GrammarBuilder &builder) {
}
void IteComponent::insertComponent(const GrammarBuilder& builder) {
    auto ti = theory::clia::getTInt(), tb = type::getTBool();
    auto info_list = builder.getSymbols(ti);
    for (auto& info: info_list) {
        auto* cond = builder.getSymbol(info.context, tb);
        if (cond) info.symbol->rule_list.push_back(new ConcreteRule(std::make_shared<IteSemantics>(), {cond, info.symbol, info.symbol}));
    }
}
Term IteComponent::tryBuildTerm(const PSemantics& sem, const TermList &term_list) {
    if (!dynamic_cast<IteSemantics*>(sem.get())) return nullptr;
    assert(term_list.size() == 3);
    return std::make_shared<TmIf>(term_list[0], term_list[1], term_list[2]);
}

class TmAppSemantics: public FullExecutedSemantics {
public:
    Context* ctx;
    TmAppSemantics(Context* _ctx): FullExecutedSemantics("app"), ctx(_ctx) {}
    virtual Data run(DataList&& inp_list, ExecuteInfo* info) {
        Data current = inp_list[0];
        for (int i = 1; i < inp_list.size(); ++i) {
            auto* fv = dynamic_cast<VFunction*>(current.get());
            current = fv->run(std::make_shared<TmValue>(inp_list[i]), ctx);
        }
        return current;
    }
    virtual ~TmAppSemantics() = default;
};

ApplyComponent::ApplyComponent(Context* _ctx, bool _is_only_full): is_only_full(_is_only_full),
    ContextFreeSynthesisComponent(-1, "apply"), ctx(_ctx) {
}
Term ApplyComponent::tryBuildTerm(const PSemantics& sem, const TermList &term_list) {
    if (!dynamic_cast<TmAppSemantics*>(sem.get())) return nullptr;
    auto current = term_list[0];
    for (int i = 1; i < term_list.size(); ++i) {
        current = std::make_shared<TmApp>(current, term_list[i]);
    }
    return current;
}

void ApplyComponent::extendNTMap(GrammarBuilder &builder) {
    if (is_only_full) {
        int pre_size = builder.info_list.size();
        for (int i = 0; i < pre_size; ++i) {
            auto current = builder.info_list[i].type;
            while (1) {
                auto* ta = dynamic_cast<TArrow*>(current.get());
                if (!ta) {
                    builder.insertInfo(builder.info_list[i].context, current);
                    break;
                }
                assert(ta->inp_types.size() == 1);
                current = ta->oup_type;
            }
        }
    } else {
        int pre_size = builder.info_list.size();
        for (int i = 0; i < pre_size; ++i) {
            auto current = builder.info_list[i].type;
            while (1) {
                builder.insertInfo(builder.info_list[i].context, current);
                auto* ta = dynamic_cast<TArrow*>(current.get());
                if (!ta) break;
                builder.insertInfo(builder.info_list[i].context, ta->inp_types[0]);
                assert(ta->inp_types.size() == 1);
                current = ta->oup_type;
            }
        }
    }
}
void ApplyComponent::insertComponent(const GrammarBuilder &builder) {
    auto semantics = std::make_shared<TmAppSemantics>(ctx);
    if (is_only_full) {
        for (auto& info: builder.info_list) {
            auto current = info.type;
            NTList param_list = {info.symbol};
            while (1) {
                auto* ta = dynamic_cast<TArrow*>(current.get());
                if (!ta) break; assert(ta->inp_types.size() == 1);
                param_list.push_back(builder.getSymbol(info.context, ta->inp_types[0]));
                current = ta->oup_type;
            }
            if (param_list.size() == 1) continue;
            auto* res = builder.getSymbol(info.context, current);

            bool is_valid = res;
            for (auto* param: param_list) {
                if (!param) is_valid = false;
            }
            if (is_valid) res->rule_list.push_back(new ConcreteRule(semantics, param_list));
        }
    } else {
        for (auto& info: builder.info_list) {
            auto current = info.type;
            auto* ta = dynamic_cast<TArrow*>(current.get());
            if (!ta) continue; assert(ta->inp_types.size() == 1);
            auto param = builder.getSymbol(info.context, ta->inp_types[0]);
            auto res = builder.getSymbol(info.context, ta->oup_type);
            if (param && res) res->rule_list.push_back(new ConcreteRule(semantics, {info.symbol, param}));
        }
    }
}

#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/incre/trans/incre_trans.h"

TupleComponent::TupleComponent(): ContextFreeSynthesisComponent( -1, "prod") {
}

void TupleComponent::extendNTMap(GrammarBuilder &builder) {
    for (int i = 0; i < builder.info_list.size(); ++i) {
        auto* tp = dynamic_cast<TProduct*>(builder.info_list[i].type.get());
        if (tp) {
            for (auto& sub_type: tp->sub_types) {
                builder.insertInfo(builder.info_list[i].context, sub_type);
            }
        }
    }
}
void TupleComponent::insertComponent(const GrammarBuilder &builder) {
    auto sem = std::make_shared<ProductSemantics>();
    for (auto& info: builder.info_list) {
        auto* tp = dynamic_cast<TProduct*>(info.type.get());
        if (!tp) continue;
        NTList param_list;
        for (auto& sub_type: tp->sub_types) {
            param_list.push_back(builder.getSymbol(info.context, sub_type));
        }
        bool is_valid = true;
        for (auto* param: param_list) {
            if (!param) is_valid = false;
        }
        if (is_valid) info.symbol->rule_list.push_back(new ConcreteRule(sem, param_list));
    }
}
Term TupleComponent::tryBuildTerm(const PSemantics& sem, const TermList &term_list) {
    if (!dynamic_cast<ProductSemantics*>(sem.get())) return nullptr;
    return std::make_shared<TmTuple>(term_list);
}

ProjComponent::ProjComponent(): ContextFreeSynthesisComponent(-1, "proj") {
}

void ProjComponent::extendNTMap(GrammarBuilder &builder) {
    for (int i = 0; i < builder.info_list.size(); ++i) {
        auto* tp = dynamic_cast<TProduct*>(builder.info_list[i].type.get());
        if (tp) {
            for (auto& sub_type: tp->sub_types) {
                builder.insertInfo(builder.info_list[i].context, sub_type);
            }
        }
    }
}
void ProjComponent::insertComponent(const GrammarBuilder &builder) {
    for (auto& info: builder.info_list) {
        auto* tp = dynamic_cast<TProduct*>(info.type.get());
        if (!tp) continue;
        for (int i = 0; i < tp->sub_types.size(); ++i) {
            auto sem = std::make_shared<AccessSemantics>(i);
            auto* target = builder.getSymbol(info.context, tp->sub_types[i]);
            if (target) target->rule_list.push_back(new ConcreteRule(sem, {info.symbol}));
        }
    }
}
Term ProjComponent::tryBuildTerm(const PSemantics &sem, const TermList &term_list) {
    auto* as = dynamic_cast<AccessSemantics*>(sem.get());
    if (!as) return nullptr;
    assert(term_list.size() == 1);
    return std::make_shared<TmProj>(term_list[0], as->id + 1);
}

ComponentPool::ComponentPool(const SynthesisComponentList &_align_list, const SynthesisComponentList &_compress_list,
                             const SynthesisComponentList &_comb_list):
                             align_list(_align_list), compress_list(_compress_list), comb_list(_comb_list) {
}
ComponentPool::ComponentPool() {
}

void ComponentPool::print() const {
    std::vector<std::pair<std::string, SynthesisComponentList>> all_components = {
            {"compress", compress_list}, {"align", align_list}, {"comb", comb_list}
    };
    for (auto& [name, comp_list]: all_components) {
        std::cout << "Components for " << name << ":" << std::endl;
        for (auto& comp: comp_list) {
            auto* uc = dynamic_cast<IncreComponent*>(comp.get());
            if (uc) std::cout << "  " << uc->term->toString() << " " << type::typeList2String(uc->param_types) << " -> " << uc->res_type->getName() << " " << uc->command_id << std::endl;
            auto* bc = dynamic_cast<BasicOperatorComponent*>(comp.get());
            if (bc) std::cout << "  " << bc->_sem->getName() << " " << type::typeList2String(bc->sem->inp_type_list) << " " << bc->sem->oup_type->getName() << std::endl;
        }
        std::cout << std::endl;
    }
}

TypeLabeledDirectSemantics::TypeLabeledDirectSemantics(const PType &_type): NormalSemantics(_type->getName(), _type, {_type}), type(_type) {
}
Data TypeLabeledDirectSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    return inp_list[0];
}

namespace {
    Grammar* _buildGrammar(const TypeList& inp_list, const SynthesisComponentList& component_list, const std::function<bool(Type*)>& is_oup, const PType& single_oup) {
        SymbolContext init_context;
        for (int i = 0; i < inp_list.size(); ++i) {
            init_context.push_back(semantics::buildParamSemantics(i, inp_list[i]));
        }
        GrammarBuilder builder(init_context);

        while (true) {
            int pre_size = builder.contexts.size();
            for (auto& component: component_list) component->extendContext(builder);
            if (builder.contexts.size() == pre_size) break;
        }

        for (auto& context: builder.contexts) {
            for (auto& sem: context) {
                auto* ts = dynamic_cast<TypedSemantics*>(sem.get());
                assert(ts);
                builder.insertInfo(context, ts->oup_type);
                auto* symbol = builder.getSymbol(context, ts->oup_type);
                symbol->rule_list.push_back(new ConcreteRule(sem, {}));
            }
        }

        int pre_size;
        if (single_oup) builder.insertInfo(init_context, single_oup);
        do {
            pre_size = builder.info_list.size();
            for (auto& component: component_list) {
                component->extendNTMap(builder);
            }
        } while (pre_size < builder.info_list.size());

        for (auto& component: component_list) {
            component->insertComponent(builder);
        }

        auto init_symbols = builder.getSymbols([&](const SymbolInfo& info) {
            return _context2String(info.context) == _context2String(init_context);
        });
        NTList start_list, symbol_list;
        for (auto& info: builder.info_list) symbol_list.push_back(info.symbol);
        for (auto& info: init_symbols) {
            if (is_oup(info.type.get())) start_list.push_back(info.symbol);
        }
        if (start_list.empty()) {
            auto* dummy_symbol = new NonTerminal("start", type::getTBool());
            return new Grammar(dummy_symbol, {dummy_symbol});
        }
        if (single_oup) {
            assert(start_list.size() == 1);
            auto* grammar = new Grammar(start_list[0], symbol_list, true);
            return grammar;
        }
        auto* start_symbol = new NonTerminal("start", type::getTVarA());
        symbol_list.push_back(start_symbol);
        for (auto* possible: start_list) {
            auto sem = std::make_shared<TypeLabeledDirectSemantics>(possible->type);
            start_symbol->rule_list.push_back(new ConcreteRule(sem, {possible}));
        }
        auto* grammar = new Grammar(start_symbol, symbol_list, true);
        return grammar;
    }
    bool _isPrimaryType(Type* type) {
        return dynamic_cast<TBool*>(type) || dynamic_cast<TInt*>(type);
        /*auto* tp = dynamic_cast<TProduct*>(type);
        if (!tp) return false;
        for (auto& sub: tp->sub_types) if (!_isPrimaryType(sub.get())) return false;
        return true;*/
    }
    bool _isCompressType(Type* type) {
        return dynamic_cast<TCompress*>(type);
    }
    bool _isNonFunctionalType(Type* type) {
        if (dynamic_cast<TArrow*>(type)) return false;
        auto* tt = dynamic_cast<TProduct*>(type);
        if (tt) {
            for (auto& sub_type: tt->sub_types) {
                if (_isNonFunctionalType(sub_type.get())) return false;
            }
        }
        return true;
    }
    bool _isGeneralCompressType(Type* type) {
        return _isNonFunctionalType(type) && !dynamic_cast<TProduct*>(type);
    }
    bool _isCompressOrPrimaryType(Type* type) {
        return _isPrimaryType(type) || _isCompressType(type);
    }
}

Grammar *ComponentPool::buildAlignGrammar(const TypeList &inp_list, bool _is_only_prime) {
    if (_is_only_prime) {
        return _buildGrammar(inp_list, align_list, _isPrimaryType, nullptr);
    } else {
        return _buildGrammar(inp_list, align_list, _isNonFunctionalType, nullptr);
    }
}

Grammar *ComponentPool::buildCompressGrammar(const TypeList &inp_list, int command_id) {
    SynthesisComponentList component_list;
    for (auto& component: compress_list) {
        if (component->command_id < command_id) {
            component_list.push_back(component);
        }
    }
    return _buildGrammar(inp_list, component_list, _isCompressOrPrimaryType, nullptr);
}

Grammar *ComponentPool::buildCombinatorGrammar(const TypeList &inp_list, const PType &oup_type, int command_id) {
    LOG(INFO) << "comb grammar";
    SynthesisComponentList component_list;
    for (auto& component: comb_list) {
        if (component->command_id < command_id) {
            component_list.push_back(component);
        }
    }
    return _buildGrammar(inp_list, component_list, [&](Type* type){return type::equal(type, oup_type.get());}, oup_type);
}

#include <unordered_set>

namespace {
    void _merge(SynthesisComponentList& x, const SynthesisComponentList& y) {
        std::unordered_set<std::string> component_set;
        for (auto& existing_component: x) {
            auto* ic = dynamic_cast<IncreComponent*>(existing_component.get());
            if (ic) component_set.insert(ic->name);
        }
        for (auto& new_component: y) {
            auto* ic = dynamic_cast<IncreComponent*>(new_component.get());
            if (ic) {
                if (component_set.find(ic->name) == component_set.end()) {
                    component_set.insert(ic->name);
                    x.push_back(new_component);
                }
            } else x.push_back(new_component);
        }
    }
}

void ComponentPool::merge(const ComponentPool &pool) {
    _merge(comb_list, pool.comb_list); _merge(align_list, pool.align_list);
    _merge(compress_list, pool.compress_list);
}

const std::string incre::grammar::collector::KCollectMethodName = "Collector@CollectMethodName";

namespace {
    const ComponentCollectorType default_type = ComponentCollectorType::LABEL;
}

#include "istool/sygus/theory/basic/string/str.h"

Grammar *incre::grammar::builder::buildGrammar(const TypeList &inp_list, const SynthesisComponentList &component_list,
                                               const PType &oup) {
    LOG(INFO) << "Oup type " << oup->getName();
    auto func = [&](Type* type) {return type::equal(type, oup.get());};
    return _buildGrammar(inp_list, component_list, func, oup);
}

ComponentPool incre::grammar::collectComponent(EnvContext* env_ctx, TypeContext* ctx, Env* env, ProgramData* program) {
    auto* ref = env->getConstRef(collector::KCollectMethodName, BuildData(Int, default_type));
    auto collector_type = static_cast<ComponentCollectorType>(theory::clia::getIntValue(*ref));

    ComponentPool base_res = collector::getBasicComponentPool(env), source_res, extra_res;
    switch (collector_type) {
        case ComponentCollectorType::LABEL: {
            source_res = collector::collectComponentFromLabel(env_ctx, ctx, program);
            break;
        }
        case ComponentCollectorType::SOURCE: {
            source_res = collector::collectComponentFromSource(env_ctx, ctx, program);
            break;
        }
    }

    auto extra = env->getConstRef(incre::config_name::KExtraGrammarName, BuildData(String, "Fold"));
    auto name = theory::string::getStringValue(*extra);
    if (!name.empty()) {
        collector::loadExtraOperator(env_ctx, ctx, env, name);
        extra_res = collector::collectExtraOperators(env_ctx, ctx, name);
    }

    auto* is_fold = env->getConstRef(incre::config_name::KIsEnableFoldName, BuildData(Bool, false));
    if (is_fold->isTrue()) {
        collector::loadExtraOperator(env_ctx, ctx, env, "Fold");
        extra_res.merge(collector::collectExtraOperators(env_ctx, ctx, "Fold"));
    }

    base_res.merge(extra_res);
    base_res.merge(source_res);
    return base_res;
}

#define RegisterComponent(type, comp) basic.type ## _list.push_back(comp)
#define RegisterAll(comp) RegisterComponent(comb, comp), RegisterComponent(align, comp), RegisterComponent(compress, comp)

ComponentPool incre::grammar::collector::getBasicComponentPool(Env* env) {
    ComponentPool basic;
    // insert basic operator
    std::vector<std::string> op_list = {"+", "-", "=", "<", "<=", "and", "or", "!"};

    const std::unordered_set<std::string> all_used_op = {"+", "-"};

    for (auto op_name: op_list) {
        auto sem = env->getSemantics(op_name);
        auto comp = std::make_shared<BasicOperatorComponent>(op_name, sem);
        if (all_used_op.find(op_name) != all_used_op.end()) RegisterAll(comp); else RegisterComponent(comb, comp);
    }

    {
        auto time_sem = env->getSemantics("*");
        PSynthesisComponent comp = std::make_shared<BasicOperatorComponent>("*", time_sem);
        auto* is_use_time = env->getConstRef(incre::config_name::KIsNonLinearName, BuildData(Bool, false));
        if (!is_use_time->isTrue()) comp->command_id = 1e9;
        RegisterComponent(comb, comp);
    }

    // insert const operator
    auto ic_align = std::make_shared<ConstComponent>(theory::clia::getTInt(), (DataList){BuildData(Int, 0)},
                                                        [](Value* value)->bool {return dynamic_cast<IntValue*>(value);});
    RegisterComponent(align, ic_align);
    auto ic = std::make_shared<ConstComponent>(theory::clia::getTInt(), (DataList){BuildData(Int, 0), BuildData(Int, 1)},
                                                        [](Value* value)->bool {return dynamic_cast<IntValue*>(value);});
    RegisterComponent(compress, ic);
    RegisterComponent(comb, ic);
    auto ib = std::make_shared<ConstComponent>(type::getTBool(), (DataList){},
                                               [](Value* value) -> bool {return dynamic_cast<BoolValue*>(value);});
    RegisterAll(ib);

    // insert language constructs
    RegisterAll(std::make_shared<IteComponent>());
    RegisterAll(std::make_shared<ProjComponent>());
    RegisterAll(std::make_shared<TupleComponent>());
    // RegisterAll(std::make_shared<ApplyComponent>(ctx, is_full_apply));
    return basic;
}