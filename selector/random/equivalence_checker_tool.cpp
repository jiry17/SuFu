//
// Created by pro on 2022/5/19.
//

#include "istool/selector/random/equivalence_checker_tool.h"

using namespace selector::random;

std::string FiniteEquivalenceCheckerTool::getFeature(Program* p) {
    DataList res;
    for (const auto& inp: inp_pool) res.push_back(env->run(p, inp));
    return data::dataList2String(res);
}
FiniteEquivalenceCheckerTool::FiniteEquivalenceCheckerTool(Env* _env, FiniteIOExampleSpace* fio): env(_env) {
        for (auto& example: fio->example_space) {
            auto io_example = fio->getIOExample(example);
            inp_pool.push_back(io_example.first);
        }
}
PProgram FiniteEquivalenceCheckerTool::insertProgram(const PProgram& p) {
    auto feature = getFeature(p.get());
    if (feature_map.count(feature) == 0) feature_map[feature] = p;
    return feature_map[feature];
}
PProgram FiniteEquivalenceCheckerTool::queryProgram(const PProgram& p) {
    auto feature = getFeature(p.get());
    if (feature_map.count(feature) == 0) return {};
    return feature_map[feature];
}
Data FiniteEquivalenceCheckerTool::getConst(Program* p) {
    auto res = env->run(p, inp_pool[0]);
    for (auto& inp: inp_pool) {
        auto now = env->run(p, inp);
        if (!(now == res)) return {};
    }
    return res;
}

DiffTreeInternalNode::DiffTreeInternalNode(const Example& _inp): inp(_inp) {}
DiffTreeInternalNode::~DiffTreeInternalNode() {
    for (auto& info: children) delete info.second;
}
DiffTreeLeaf::DiffTreeLeaf(const PProgram& _prog): prog(_prog) {}

bool Z3EquivalenceCheckerTool::checkEqual(Program* x, Program* y, Example* inp) {
    z3::solver s(ext->ctx);
    auto x_encode_res = ext->encodeZ3ExprForProgram(x, ext::z3::z3Vector2EncodeList(param_list));
    auto y_encode_res = ext->encodeZ3ExprForProgram(y, ext::z3::z3Vector2EncodeList(param_list));
    assert(x_encode_res.cons_list.empty() && y_encode_res.cons_list.empty());
    s.add(x_encode_res.res != y_encode_res.res);
    auto res = s.check();
    if (res == z3::unsat) return true;
    if (inp) {
        Example example;
        auto model = s.get_model();
        for (int i = 0; i < inp_types.size(); ++i) {
            example.push_back(ext->getValueFromModel(model, param_list[i], inp_types[i].get()));
        }
        (*inp) = example;
    }
    return false;
}

selector::random::DiffTreeNode* Z3EquivalenceCheckerTool::insertProgram(DiffTreeNode* node, const PProgram& p, PProgram& res) {
    auto* i_node = dynamic_cast<DiffTreeInternalNode*>(node);
    if (i_node) {
        auto oup = env->run(p.get(), i_node->inp);
        auto feature = oup.toString();
        auto it = i_node->children.find(feature);
        if (it == i_node->children.end()) {
            i_node->children[feature] = new DiffTreeLeaf(p);
            res = p; return node;
        }
        it->second = insertProgram(it->second, p, res);
        return node;
    }
    auto* l_node = dynamic_cast<DiffTreeLeaf*>(node);
    if (l_node) {
        Example inp;
        if (checkEqual(p.get(), l_node->prog.get(), &inp)) {
            res = l_node->prog; return node;
        }
        auto* x = new DiffTreeInternalNode(inp);
        auto oup1 = env->run(p.get(), inp);
        auto oup2 = env->run(l_node->prog.get(), inp);
        x->children[oup1.toString()] = new DiffTreeLeaf(p);
        x->children[oup2.toString()] = node;
        res = p; return x;
    }
    assert(0);
}

Data Z3EquivalenceCheckerTool::getConst(Program* p) {
    auto feature = p->toString();
    if (const_cache.count(feature)) return const_cache[feature];
    Data res = env->run(p, random_example[0]);
    for (int i = 1; i < random_example.size(); ++i) {
        if (!(env->run(p, random_example[i]) == res)) {
            return const_cache[feature] = {};
        }
    }
    z3::solver s(ext->ctx);
    auto encode_res = ext->encodeZ3ExprForProgram(p, ext::z3::z3Vector2EncodeList(param_list));
    assert(encode_res.cons_list.empty());
    s.add(encode_res.res != ext->buildConst(res));
    if (s.check() == z3::sat) return const_cache[feature] = {};
    return const_cache[feature] = res;
}

PProgram Z3EquivalenceCheckerTool::insertProgram(const PProgram& p) {
    auto feature = p->toString();
    if (query_cache.count(feature)) return query_cache[feature];
    PProgram res;
    root = insertProgram(root, p, res);
    return query_cache[feature] = res;
}

PProgram Z3EquivalenceCheckerTool::queryProgram(const PProgram& p) {
    auto feature = p->toString();
    if (query_cache.count(feature)) return query_cache[feature];
    return {};
}

Z3EquivalenceCheckerTool::Z3EquivalenceCheckerTool(Env* _env, const TypeList& _inp_types, const PExampleGenerator &_sampler, int KRandomTestNum = 20):
    inp_types(_inp_types), env(_env), ext(ext::z3::getExtension(env)), param_list(ext->ctx), sampler(_sampler) {
    for (int i = 0; i < inp_types.size(); ++i) param_list.push_back(ext->buildVar(inp_types[i].get(), "Param" + std::to_string(i)));
    while (random_example.size() < KRandomTestNum) {
        auto new_example_list = sampler->generateExamples(nullptr);
        for (const auto& example: new_example_list) random_example.push_back(example);
    }
    root = new DiffTreeInternalNode(random_example[0]);
}
Z3EquivalenceCheckerTool::~Z3EquivalenceCheckerTool() {
    delete root;
}
