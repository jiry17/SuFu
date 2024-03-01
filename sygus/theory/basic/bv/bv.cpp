//
// Created by pro on 2022/2/12.
//

#include "istool/sygus/theory/basic/bv/bv.h"
#include "istool/sygus/theory/basic/bv/bv_semantics.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/clia/clia_semantics.h"
#include "istool/basic/type_system.h"
#include "glog/logging.h"

namespace {
    const std::string KBVLengthName = "BV@Len";
}

void theory::bv::setBitVectorLength(Env* env, int length) {
    env->setConst(KBVLengthName, BuildData(Int, length));
}

Data * theory::bv::getBitVectorLengthData(Env *env) {
    return env->getConstRef(KBVLengthName);
}

int theory::bv::getBitVectorLength(Env *env) {
    Data* size_data = getBitVectorLengthData(env);
    if (size_data->isNull()) {
        LOG(FATAL) << "Access BV@Len before setting";
    }
    return theory::clia::getIntValue(*size_data);
}

#define LoadBVSemantics(name, sem) env->setSemantics(name, std::make_shared<sem ## Semantics>(size))
void theory::loadBVTheory(Env *env) {
    int size = bv::getBitVectorLength(env);
    LoadBVSemantics("bvnot", BVNot);
    LoadBVSemantics("bvneg", BVNeg);
    LoadBVSemantics("bvand", BVAnd);
    LoadBVSemantics("bvor", BVOr);
    LoadBVSemantics("bvadd", BVAdd);
    LoadBVSemantics("bvsub", BVSub);
    LoadBVSemantics("bvlshr", BVLShr);
    LoadBVSemantics("bvshl", BVShl);
    LoadBVSemantics("bvxor", BVXor);
    LoadBVSemantics("bvashr", BVAShr);
    LoadSemantics("ite", Ite);
    LoadSemantics("=", Eq);
    LoadSemantics("!=", Neq);

    auto* ext = type::getTypeExtension(env);
    ext->registerTypeInfo(new BitVectorTypeInfo());
}