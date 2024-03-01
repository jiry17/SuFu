//
// Created by pro on 2022/1/18.
//

#include "istool/ext/composed_semantics/composed_witness.h"
#include "istool/ext/composed_semantics/composed_semantics.h"
#include "glog/logging.h"

bool ComposedWitnessManager::isMatch(Semantics *semantics) {
    return dynamic_cast<ComposedSemantics*>(semantics);
}

namespace {
    bool _joinWitnessTerm(const WitnessTerm& x, const WitnessTerm& y, WitnessValueJoinOp* op, WitnessTerm& res) {
        assert(x.size() == y.size()); res.resize(x.size());
        for (int i = 0; i < x.size(); ++i) {
            auto join = op->getJoin(x[i], y[i]);
            if (!join) return false;
            res[i] = join;
        }
        return true;
    }

    WitnessList _mergeWitnessList(const WitnessList& x, const WitnessList& y, WitnessValueJoinOp* op) {
        WitnessList res; WitnessTerm term;
        for (auto& x_term: x) {
            for (auto& y_term: y) {
                if (_joinWitnessTerm(x_term, y_term, op, term)) {
                    res.push_back(term);
                }
            }
        }
        return res;
    }

    void _extendWitnessList(WitnessList& x, const WitnessList& y) {
        for (const auto& y_term: y) x.push_back(y_term);
    }
}

WitnessList ComposedWitnessManager::getWitness(Program *program, const WitnessData &oup, int inp_num) {
    auto* ps = dynamic_cast<ParamSemantics*>(program->semantics.get());
    if (ps) {
        WitnessTerm res(inp_num);
        for (int i = 0; i < inp_num; ++i) {
            if (i == ps->id) res[i] = oup;
            else res[i] = std::make_shared<TotalWitnessValue>();
        }
        return {res};
    }
    WitnessList res;
    auto direct_wit = ext->getWitness(program->semantics.get(), oup, {});
    for (const auto& term: direct_wit) {
        WitnessTerm init_term(inp_num, std::make_shared<TotalWitnessValue>());
        WitnessList now(1, init_term);
        for (int i = 0; i < term.size(); ++i) {
            auto sub_res = getWitness(program->sub_list[i].get(), term[i], inp_num);
            now = _mergeWitnessList(now, sub_res, join_op);
        }
        _extendWitnessList(res, now);
    }
    return res;
}

WitnessList ComposedWitnessManager::getWitness(Semantics *semantics, const WitnessData &oup, const DataList &inp_list) {
    auto* cs = dynamic_cast<ComposedSemantics*>(semantics);
    auto res = getWitness(cs->body.get(), oup, cs->param_num);
    /*LOG(INFO) << "Witness " << semantics->getName() << " " << oup->toString() << std::endl;
    for (auto& term: res) {
        for (auto& v: term) std::cout << " " << v->toString() << " "; std::cout << std::endl;
    }
    int kk; std::cin >> kk;*/
    return res;
}

ComposedWitnessManager::ComposedWitnessManager(WitnessValueJoinOp *_join_op, VSAExtension *_ext): join_op(_join_op), ext(_ext) {
}
ComposedWitnessManager::~ComposedWitnessManager() {
    delete join_op;
}

void ext::vsa::registerComposedManager(VSAExtension *ext, WitnessValueJoinOp *op) {
    auto* m = new ComposedWitnessManager(op, ext);
    ext->registerWitnessManager(m);
}
void ext::vsa::registerDefaultComposedManager(VSAExtension *ext) {
    auto* op = new DefaultSyGuSWitnessValueJoinOp();
    registerComposedManager(ext, op);
}