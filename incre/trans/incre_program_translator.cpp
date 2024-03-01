//
// Created by pro on 2022/9/25.
//

#include "istool/incre/trans/incre_trans.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"

using namespace incre;

// TODO: deal with || and &&
Term incre::termToIncre(Program *program, const TermList& inps) {
    auto* sem = program->semantics.get();
    {
        auto* cs = dynamic_cast<ConstSemantics*>(sem);
        if (cs) return std::make_shared<TmValue>(cs->w);
    }
    {
        auto* ps = dynamic_cast<ParamSemantics*>(sem);
        if (ps) {
            assert(0 <= ps->id && ps->id < inps.size());
            return inps[ps->id];
        }
    }
    {
        auto* ps = dynamic_cast<ProductSemantics*>(sem);
        if (ps) {
            TermList fields;
            for (const auto& sub: program->sub_list) {
                fields.push_back(termToIncre(sub.get(), inps));
            }
            return std::make_shared<TmTuple>(fields);
        }
    }
    {
        auto* as = dynamic_cast<AccessSemantics*>(sem);
        if (as) {
            int id = as->id;
            auto content = termToIncre(program->sub_list[0].get(), inps);
            return std::make_shared<TmProj>(content, id + 1);
        }
    }
    auto name = sem->getName();
    if (name == "ite") {
        auto c = incre::termToIncre(program->sub_list[0].get(), inps);
        auto t = incre::termToIncre(program->sub_list[1].get(), inps);
        auto f = incre::termToIncre(program->sub_list[2].get(), inps);
        return std::make_shared<TmIf>(c, t, f);
    }
    auto res = incre::isBasicOperator(name) ? incre::getOperator(name) : std::make_shared<TmVar>(name);
    for (auto& sub: program->sub_list) {
        auto param = incre::termToIncre(program, inps);
        res = std::make_shared<TmApp>(res, param);
    }
    return res;
}