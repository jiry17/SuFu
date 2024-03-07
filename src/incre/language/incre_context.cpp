//
// Created by pro on 2022/9/16.
//

#include "istool/incre/language/incre_context.h"
#include "glog/logging.h"
#include "glog/logging.h"

using namespace incre;

BindingType BindingData::getType() const {return type;}
BindingData::BindingData(const BindingType &_type): type(_type) {}

TypeBinding::TypeBinding(const Ty &_type): BindingData(BindingType::TYPE), type(_type) {}
std::string TypeBinding::toString() const {
    return type->toString();
}
TermBinding::TermBinding(const Term &_term, const Ty& _ty): BindingData(BindingType::TERM), term(_term), type(_ty) {}
std::string TermBinding::toString() const {
    return term->toString();
}
VarTypeBinding::VarTypeBinding(const Ty&_type): BindingData(BindingType::VAR), type(_type) {}
std::string VarTypeBinding::toString() const {
    return type->toString();
}

void Context::addBinding(const std::string &name, const Ty &type) {
    // LOG(INFO) << "binding " << name << " : " << type->toString();
    binding_map[name] = std::make_shared<TypeBinding>(type);
}

void Context::addBinding(const std::string &name, const Term &term, const Ty& type) {
    // if (type) LOG(INFO) << "binding " << name << " : " << type->toString();
    binding_map[name] = std::make_shared<TermBinding>(term, type);
}

Ty Context::getType(const std::string &name) {
    if (binding_map.find(name) == binding_map.end()) {
        LOG(FATAL) << "Context error: name " << name << " not found.";
    }
    auto* binding = binding_map[name].get();
    auto* ty_binding = dynamic_cast<TypeBinding*>(binding);
    if (ty_binding) return ty_binding->type;
    auto* term_binding = dynamic_cast<TermBinding*>(binding);
    if (!term_binding) LOG(FATAL) << "Unknown binding " << term_binding->toString();
    if (!term_binding->type) LOG(FATAL) << "The type of " << term_binding->term->toString() << " is unknown.";
    return term_binding->type;
}

Term Context::getTerm(const std::string& name) {
    if (binding_map.find(name) == binding_map.end()) {
        LOG(FATAL) << "Context error: name " << name << " not found.";
    }
    auto binding = dynamic_cast<TermBinding*>(binding_map[name].get());
    if (!binding) {
        LOG(FATAL) << "Context error: expected a term bound to " << name << ", but found a type.";
    }
    return binding->term;
}

TypeContext::BindLog TypeContext::bind(const std::string &name, const Ty &type) {
    BindLog log({name, nullptr});
    if (binding_map.find(name) != binding_map.end()) {
        log.binding = binding_map[name];
    }
    binding_map[name] = type;
    return log;
}

void TypeContext::cancelBind(const BindLog &log) {
    if (!log.binding) binding_map.erase(log.name);
    else binding_map[log.name] = log.binding;
}
TypeContext::~TypeContext() {
}

Ty TypeContext::lookup(const std::string &name) {
    if (binding_map.find(name) == binding_map.end()) {
        LOG(FATAL) << "Unknown type variable " << name;
    }
    return binding_map[name];
}

TypeContext::TypeContext(Context *ctx)  {
    for (const auto& [name, _]: ctx->binding_map) {
        binding_map[name] = ctx->getType(name);
    }
}