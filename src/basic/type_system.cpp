//
// Created by pro on 2021/12/4.
//

#include "istool/basic/type_system.h"
#include "glog/logging.h"

namespace {
    const std::string KTypeSystemName = "TypeSystem";
}

BasicValueTypeInfo::BasicValueTypeInfo(): bot_type(std::make_shared<TBot>()), bool_type(std::make_shared<TBool>()) {
}
bool BasicValueTypeInfo::isMatch(Value *value) {
    return dynamic_cast<NullValue*>(value) || dynamic_cast<BoolValue*>(value);
}
PType BasicValueTypeInfo::getType(Value *value) {
    if (dynamic_cast<NullValue*>(value)) return bot_type;
    if (dynamic_cast<BoolValue*>(value)) return bool_type;
}

TypeSystem::TypeSystem(TypeExtension *_ext): ext(_ext) {}
BasicTypeSystem::BasicTypeSystem(TypeExtension *_ext): TypeSystem(_ext) {}

ValueTypeInfo * TypeExtension::getTypeInfo(Value *value) {
    for (auto* type_info: type_info_list) {
        if (type_info->isMatch(value)) return type_info;
    }
    LOG(FATAL) << "Unknown value " << value->toString();
}

PType BasicTypeSystem::intersect(const PType &x, const PType &y) {
    if (dynamic_cast<TVar*>(x.get())) return y;
    if (dynamic_cast<TVar*>(y.get())) return x;
    if (type::equal(x, y)) return x;
    return nullptr;
}
PType BasicTypeSystem::getType(Program *p) {
    auto* cs = dynamic_cast<ConstSemantics*>(p->semantics.get());
    if (cs) {
        auto* info = ext->getTypeInfo(cs->w.get());
        return info->getType(cs->w.get());
    }
    auto* ts = dynamic_cast<TypedSemantics*>(p->semantics.get());
    if (!ts) throw TypeError(); else return ts->oup_type;
}

TypeExtension::TypeExtension() {
    type_system = new BasicTypeSystem(this);
    type_info_list.push_back(new BasicValueTypeInfo());
}
void TypeExtension::registerTypeSystem(TypeSystem *_type_system) {
    delete type_system;
    type_system = _type_system;
}
void TypeExtension::registerTypeInfo(ValueTypeInfo *info) {
    type_info_list.push_back(info);
}
PType TypeExtension::join(const PType &x, const PType &y) {
    return type_system->intersect(x, y);
}
PType TypeExtension::getType(Program *p) {
    return type_system->getType(p);
}
PType TypeExtension::getType(Value *value) {
    auto* type_info = getTypeInfo(value);
    return type_info->getType(value);
}
TypeExtension::~TypeExtension() {
    delete type_system;
    for (auto* info: type_info_list) delete info;
}

TypeExtension * type::getTypeExtension(Env *env) {
    auto* type_system = env->getExtension(KTypeSystemName);
    if (!type_system) {
        type_system = new TypeExtension();
        env->registerExtension(KTypeSystemName, type_system);
    }
    return dynamic_cast<TypeExtension*>(type_system);
}
void type::registerTypeSystem(TypeSystem *type_system, TypeExtension* ext) {
    ext->registerTypeSystem(type_system);
}