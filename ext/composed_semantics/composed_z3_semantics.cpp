//
// Created by pro on 2022/1/18.
//

#include "istool/ext/composed_semantics/composed_z3_semantics.h"
#include "istool/ext/composed_semantics/composed_semantics.h"
#include "istool/ext/z3/z3_extension.h"

ComposedZ3SemanticsManager::ComposedZ3SemanticsManager(Z3Extension *_ext): ext(_ext) {
}
bool ComposedZ3SemanticsManager::isMatch(Semantics *semantics) {
    return dynamic_cast<ComposedSemantics*>(semantics);
}
Z3EncodeRes ComposedZ3SemanticsManager::encodeZ3ExprForSemantics(Semantics *semantics,
        const std::vector<Z3EncodeRes> &inp_list, const Z3EncodeList &param_list) {
    auto* cs = dynamic_cast<ComposedSemantics*>(semantics);
    assert(cs);
    return ext->encodeZ3ExprForProgram(cs->body.get(), inp_list);
}

void ext::z3::registerComposedManager(Z3Extension *ext) {
    ext->registerSemanticsManager(new ComposedZ3SemanticsManager(ext));
}