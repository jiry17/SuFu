//
// Created by pro on 2022/1/27.
//

#include "istool/solver/component/tree_encoder.h"
#include "istool/selector/samplesy/vsa_seed_generator.h"
#include "glog/logging.h"

namespace {
    void _collectAllNode(VSANode* node, std::vector<VSANode*>& node_list) {
        if (node_list[node->id]) return;
        node_list[node->id] = node;
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                _collectAllNode(sub_node, node_list);
            }
        }
    }

    std::vector<double> _mul(const std::vector<double>& x, const std::vector<double>& y) {
        int n = x.size(), m = y.size();
        std::vector<double> res(n + m, 0.0);
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                res[i + j] += x[i] * y[j];
            }
        }
        while (!res.empty() && res[int(res.size()) - 1] < 0.5) res.pop_back();
        return res;
    }

    std::vector<double> _sum(const std::vector<double>& x, const std::vector<double>& y) {
        int n = x.size(), m = y.size();
        std::vector<double> res(std::max(n, m), 0.0);
        for (int i = 0; i < std::max(n, m); ++i) {
            res[i] = (i < x.size() ? x[i] : 0.0) + (i < y.size() ? y[i] : 0.0);
        }
        while (!res.empty() && res[int(res.size()) - 1] < 0.5) res.pop_back();
        return res;
    }

    int _getSample(const std::vector<double>& A, Env* env) {
        std::uniform_real_distribution<double> d(0.0, 1.0);
        double w = d(env->random_engine);
        double sum = 0.0;
        for (int i = 0; i < A.size(); ++i) sum += A[i];
        assert(sum >= 0.5);
        for (int i = 0; i < A.size(); ++i) {
            double k = A[i] / sum;
            if (k + 1e-8 >= w) return i;
            w -= k;
        }
        assert(0);
    }

    void _mergeIntStorage(int pos, const std::vector<std::vector<int>>& A, std::vector<int>& tmp, std::vector<std::vector<int>>& res) {
        if (pos == A.size()) {
            res.push_back(tmp); return;
        }
        for (int w: A[pos]) {
            tmp[pos] = w;
            _mergeIntStorage(pos + 1, A, tmp, res);
        }
    }

    std::vector<std::vector<int>> _mergeIntStorage(const std::vector<std::vector<int>>& A) {
        std::vector<std::vector<int>> result;
        std::vector<int> tmp(A.size());
        _mergeIntStorage(0, A, tmp, result);
        return result;
    }

    std::string sizeList2String(const std::vector<double>& s) {
        std::string res = "[";
        for (int i = 0; i < s.size(); ++i) {
            if (i) res += ","; res += std::to_string(s[i]);
        }
        return res + "]";
    }
}

VSASizeBasedSampler::VSASizeBasedSampler(Env *_env): env(_env) {
}
std::vector<double> VSASizeBasedSampler::getEdgeSize(const VSAEdge &edge) {
    std::vector<double> res(2, 0.0); res[1] = 1.0;
    for (auto* node: edge.node_list) {
        res = _mul(res, size_list[node->id]);
    }
    return res;
}
void VSASizeBasedSampler::calculateNodeSize(VSANode *node, std::vector<bool> &visited) {
    if (visited[node->id]) return;
    visited[node->id] = true;
    for (auto& edge: node->edge_list) {
        for (auto* sub_node: edge.node_list) calculateNodeSize(sub_node, visited);
        auto edge_size = getEdgeSize(edge);
        edge_size_pool[node->id].push_back(edge_size);
        size_list[node->id] = _sum(size_list[node->id], edge_size);
    }
}
void VSASizeBasedSampler::setRoot(VSANode *new_root) {
    root = new_root; int n = ext::vsa::indexVSANode(root);
    node_list.resize(n);
    for (int i = 0; i < node_list.size(); ++i) node_list[i] = nullptr;
    _collectAllNode(root, node_list);
    size_list.resize(n);
    for (int i = 0; i < n; ++i) size_list[i].clear();
    std::vector<bool> is_visited(n, false);
    edge_size_pool.clear(); edge_size_pool.resize(n);
    calculateNodeSize(root, is_visited);
    /*std::cout << "calculate node size" << std::endl;
    for (auto& size_list: size_list) {
        for (auto& w: size_list) std::cout << w << " "; std::cout << std::endl;
    }*/
}
PProgram VSASizeBasedSampler::sampleProgram(const VSAEdge &edge, int target_size) {
    std::vector<std::vector<int>> size_pool;
    for (auto* node: edge.node_list) {
        std::vector<int> possible_size;
        for (int i = 0; i < target_size && i < size_list[node->id].size(); ++i) {
            if (size_list[node->id][i] >= 0.5) possible_size.push_back(i);
        }
        size_pool.push_back(possible_size);
    }
    std::vector<std::vector<int>> possible_size_plan;
    std::vector<double> weight_list;
    for (auto& size_plan: _mergeIntStorage(size_pool)) {
        int total_size = 1;
        for (int size: size_plan) total_size += size;
        if (total_size != target_size) continue;
        double weight = 1.0;
        for (int i = 0; i < edge.node_list.size(); ++i) {
            weight *= size_list[edge.node_list[i]->id][size_plan[i]];
        }
        weight_list.push_back(weight);
        possible_size_plan.push_back(size_plan);
    }
    assert(!weight_list.empty());
    int plan_id = _getSample(weight_list, env);
    ProgramList sub_list;
    for (int i = 0; i < edge.node_list.size(); ++i) {
        sub_list.push_back(sampleProgram(edge.node_list[i], possible_size_plan[plan_id][i]));
    }
    return std::make_shared<Program>(edge.semantics, sub_list);
}
PProgram VSASizeBasedSampler::sampleProgram(VSANode *node, int target_size) {
    assert(target_size < size_list[node->id].size());
    std::vector<double> edge_size_list;
    for (int i = 0; i < node->edge_list.size(); ++i) {
        auto edge_size = edge_size_pool[node->id][i];
        if (target_size >= edge_size.size()) edge_size_list.push_back(0.0);
        else edge_size_list.push_back(edge_size[target_size]);
    }
    double total = 0.0;
    for (auto w: edge_size_list) total += w;
    int edge_id = _getSample(edge_size_list, env);
    return sampleProgram(node->edge_list[edge_id], target_size);
}
PProgram VSASizeBasedSampler::sampleNext() {
    std::vector<double> size_weight;
    for (double w: size_list[0]) {
        if (w > 0.5) size_weight.push_back(1.0); else size_weight.push_back(0.0);
    }
    int target_size = _getSample(size_weight, env);

    return sampleProgram(root, target_size);
}

VSASeedGenerator::VSASeedGenerator(const PVSABuilder& _builder, VSASampler* _sampler): builder(_builder), sampler(_sampler) {
    root = builder->buildFullVSA();
    if (!ext::vsa::isAcyclic(root)) {
        LOG(FATAL) << "VSASampleSy requires the grammar to be acyclic";
    }
}

void VSASeedGenerator::addExample(const IOExample &example) {
    auto next_root = builder->buildVSA(example.second, example.first, nullptr);
    root = builder->mergeVSA(root, next_root, nullptr);
}

ProgramList VSASeedGenerator::getSeeds(int num, double time_limit) {
    auto* guard = new TimeGuard(time_limit);
    sampler->setRoot(root);
    ProgramList res;
    while (guard->getRemainTime() >= 0. && res.size() < num) {
        res.push_back(sampler->sampleNext());
    }
    return res;
}

FiniteVSASeedGenerator::FiniteVSASeedGenerator(const PVSABuilder &_builder, VSASampler *_sampler, DifferentProgramGenerator* _g, FiniteIOExampleSpace *io_space):
    builder(_builder), sampler(_sampler), g(_g) {
    root = builder->buildFullVSA();
    for (const auto& example: io_space->example_space) {
        io_examples.push_back(io_space->getIOExample(example));
    }
}

namespace {
    bool isEquivalent(Program* x, Program* y, const IOExampleList& example_list, Env* env) {
        for (auto& example: example_list) {
            if (!(env->run(x, example.first) == env->run(y, example.first))) return false;
        }
        return true;
    }
}

void FiniteVSASeedGenerator::addExample(const IOExample &example) {
    auto next_root = builder->buildVSA(example.second, example.first, nullptr);
    root = builder->mergeVSA(root, next_root, nullptr);
    g->addExample(example);
}


ProgramList FiniteVSASeedGenerator::getSeeds(int num, double time_limit) {
    auto* guard = new TimeGuard(time_limit);
    sampler->setRoot(root);
    ProgramList res;
    while (res.size() < num) {
        res.push_back(sampler->sampleNext());
        if (guard->getRemainTime() < 0) break;
    }
    PProgram now = res[0]; int count = 0;
    for (const auto& p: res) {
        if (isEquivalent(now.get(), p.get(), io_examples, builder->env)) count++;
        else {
            count--;
            if (count == 0) {
                count = 1; now = p;
            }
        }
    }
    count = 0;
    for (const auto& p: res) {
        if (isEquivalent(now.get(), p.get(), io_examples, builder->env)) count++;
        if (guard->getRemainTime() < 0) break;
    }
    if (count + 100 > res.size() && guard->getRemainTime() > 0) {
        LOG(INFO) << "Start enhance from " << res.size() << " samples";
        for (auto& example: io_examples) {
            if (guard->getRemainTime() < 0) break;
            for (auto& p: g->getDifferentProgram(example, 10)) res.push_back(p);
        }
        LOG(INFO) << "Finish with " << res.size() << " samples";
    }
    return res;
}

namespace {
    int _getGrammarDepth(NonTerminal* symbol, std::vector<int>& depth) {
        int id = symbol->id;
        if (depth[id] == -2) LOG(FATAL) << "The grammar should be acyclic";
        if (depth[id] != -1) return depth[id];
        depth[id] = -2;
        int res = 0;
        for (auto* rule: symbol->rule_list) {
            for (auto* node: rule->param_list) {
                res = std::max(res, _getGrammarDepth(node, depth));
            }
        }
        return depth[id] = res + 1;
    }

    int _getGrammarDepth(Grammar* g) {
        g->indexSymbol();
        std::vector<int> depth(g->symbol_list.size(), -1);
        return _getGrammarDepth(g->start, depth);
    }

    TypeList _getParamTypeList(Grammar* g) {
        TypeList res;
        for (auto* symbol: g->symbol_list) {
            for (auto* rule: symbol->rule_list) {
                auto* ps = grammar::getParamSemantics(rule);
                if (ps) {
                    while (res.size() <= ps->id) res.emplace_back();
                    res[ps->id] = ps->oup_type;
                }
            }
        }
        for (int i = 0; i < res.size(); ++i) {
            if (!res[i]) LOG(FATAL) << "Param" << i << " is not used in the grammar";
        }
        return res;
    }
}

Z3VSASeedGenerator::Z3VSASeedGenerator(Specification* spec, const PVSABuilder &_builder, VSASampler *_sampler):
    builder(_builder), sampler(_sampler), ext(ext::z3::getExtension(spec->env.get())), cons_list(ext->ctx),
    param_list(ext->ctx), encode_output(ext->ctx) {
    root = builder->buildFullVSA();
    auto* g = spec->info_list[0]->grammar;
    encoder = new TreeEncoder(g, ext, _getGrammarDepth(g));
    cons_list = encoder->encodeStructure("x");
    io_space = dynamic_cast<Z3IOExampleSpace*>(spec->example_space.get());
    if (!io_space) {
        LOG(FATAL) << "Z3VSASeedGenerator requires Z3IOExampleSpace";
    }
    auto param_type_list = _getParamTypeList(g);
    for (int i = 0; i < param_type_list.size(); ++i) {
        param_list.push_back(ext->buildVar(param_type_list[i].get(), "Param" + std::to_string(i)));
    }
    auto encode_res = encoder->encodeExample(ext::z3::z3Vector2EncodeList(param_list), "x@var");
    for (const auto& cons: encode_res.cons_list) cons_list.push_back(cons);
    encode_output = encode_res.res;
}

void Z3VSASeedGenerator::addExample(const IOExample &example) {
    auto next_root = builder->buildVSA(example.second, example.first, nullptr);
    root = builder->mergeVSA(root, next_root, nullptr);

    z3::expr_vector inp_list(ext->ctx);
    for (auto& data: example.first) inp_list.push_back(ext->buildConst(data));
    auto oup = ext->buildConst(example.second);
    auto encode_res = encoder->encodeExample(ext::z3::z3Vector2EncodeList(inp_list), "v" + std::to_string(++example_count));
    cons_list.push_back(encode_res.res == oup);
    for (const auto& cons: encode_res.cons_list) cons_list.push_back(cons);
}

bool Z3VSASeedGenerator::checkEquivalent(Program* x, Program* y) {
    z3::solver s(ext->ctx);
    auto x_encode_res = ext->encodeZ3ExprForProgram(x, ext::z3::z3Vector2EncodeList(param_list));
    auto y_encode_res = ext->encodeZ3ExprForProgram(y, ext::z3::z3Vector2EncodeList(param_list));
    s.add(x_encode_res.cons_list);
    s.add(y_encode_res.cons_list);
    s.add(x_encode_res.res != y_encode_res.res);
    return s.check() == z3::unsat;
}

ProgramList Z3VSASeedGenerator::getSeeds(int num, double time_limit) {
    auto* guard = new TimeGuard(time_limit);
    sampler->setRoot(root);
    ProgramList res;
    while ( res.size() < num) {
        res.push_back(sampler->sampleNext());
        if (guard->getRemainTime() < 0) break;
    }
    PProgram now = res[0]; int count = 0;
    for (const auto& p: res) {
        if (checkEquivalent(now.get(), p.get())) count++;
        else {
            count--;
            if (count == 0) count = 1, now = p;
        }
        if (guard->getRemainTime() < 0) break;
    }
    if (count + 100 > res.size()) {
        LOG(INFO) << "Start enhance from " << res.size() << " samples for " << now->toString();
        z3::solver s(ext->ctx); s.add(cons_list);
        auto now_encode = ext->encodeZ3ExprForProgram(now.get(), ext::z3::z3Vector2EncodeList(param_list));
        s.add(now_encode.cons_list); s.add(now_encode.res != encode_output);
        for (int _ = 0; _ < 100; ++_) {
            auto rem = guard->getRemainTime();
            if (rem < 0) break;
            ext->setTimeOut(s, guard);
            auto check_res = s.check();
            if (check_res != z3::sat) break;
            auto model = s.get_model();
            auto new_p = encoder->programBuilder(model);
            res.push_back(new_p);

        }
        LOG(INFO) << "Finish with " << res.size() << " samples";
    }
    return res;
}