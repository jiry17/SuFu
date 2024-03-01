//
// Created by pro on 2022/5/11.
//

#include "istool/solver/component/tree_encoder.h"
#include "glog/logging.h"

TreeEncoder::TreeNode::TreeNode(int _ind, NonTerminal* _symbol, const std::unordered_map<std::string, std::vector<TreeEncoder::TreeNode *>> &_sub_map,
                                const std::vector<TreeEdge *> &_edge_list):
                                ind(_ind), symbol(_symbol), sub_map(_sub_map), edge_list(_edge_list) {
}
TreeEncoder::TreeNode::~TreeNode() {
    for (auto* edge: edge_list) delete edge;
    for (auto& info: sub_map) {
        for (auto* sub_node: info.second) delete sub_node;
    }
}

TreeEncoder::TreeEdge::TreeEdge(const PSemantics& _semantics, const std::vector<TreeNode *> &_node_list):
    semantics(_semantics), node_list(_node_list) {
}

namespace {
    z3::expr _buildIntVar(const std::string& name, z3::context& ctx) {
        return ctx.int_const(name.c_str());
    }

    TreeEncoder::TreeNode* _constructTree(NonTerminal* symbol, int depth_limit, int& ind) {
        if (depth_limit == 0) return nullptr;
        std::unordered_map<std::string, int> sub_num_map;
        NTList sub_node_list;
        for (auto* rule: symbol->rule_list) {
            std::unordered_map<std::string, int> usage_map;
            for (auto* sub_symbol: rule->param_list) {
                auto name = sub_symbol->name;
                if (sub_num_map.count(name) == 0) sub_node_list.push_back(sub_symbol);
                sub_num_map[name] = std::max(sub_num_map[name], ++usage_map[name]);
            }
        }
        std::unordered_map<std::string, std::vector<TreeEncoder::TreeNode*>> sub_map;
        for (auto* sub_symbol: sub_node_list) {
            auto* sub_node = _constructTree(sub_symbol, depth_limit - 1, ind);
            if (!sub_node) continue;
            auto name = sub_symbol->name;
            sub_map[name].push_back(sub_node);
            for (int i = 1; i < sub_num_map[name]; ++i) {
                sub_map[name].push_back(_constructTree(sub_symbol, depth_limit - 1, ind));
            }
        }
        std::vector<TreeEncoder::TreeEdge*> edge_list;
        for (auto* rule: symbol->rule_list) {
            std::unordered_map<std::string, int> usage_map;
            std::vector<TreeEncoder::TreeNode*> param_list;
            bool flag = true;
            for (auto* sub_symbol: rule->param_list) {
                auto name = sub_symbol->name;
                if (sub_map.find(name) == sub_map.end()) {
                    flag = false; break;
                }
                int current_ind = usage_map[name]++;
                auto* current_node = sub_map[name][current_ind];
                param_list.push_back(current_node);
            }
            if (!flag) continue;
            ConcreteRule* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "TreeEncoder supports only ConcreteRules";
            edge_list.push_back(new TreeEncoder::TreeEdge(cr->semantics, param_list));
        }
        if (edge_list.empty()) {
            for (auto& info: sub_map) {
                for (auto* sub_node: info.second) delete sub_node;
            }
            return nullptr;
        }
        return new TreeEncoder::TreeNode(ind++, symbol, sub_map, edge_list);
    }
}

void TreeEncoder::constructTree() {
    if (root) delete root;
    int ind = 0; selector_map.clear();
    root = _constructTree(base->start, depth, ind);
}

namespace {
    void _initSelector(TreeEncoder::TreeNode* node, std::unordered_map<int, z3::expr>& cache, const std::string& prefix, z3::context& ctx) {
        cache.insert({node->ind, _buildIntVar(prefix + "@" + std::to_string(node->ind), ctx)});
        for (auto& info: node->sub_map) {
            for (auto* sub_node: info.second) _initSelector(sub_node, cache, prefix, ctx);
        }
    }
}

void TreeEncoder::initSelectors(const std::string &prefix) {
    selector_map.clear(); assert(root);
    _initSelector(root, selector_map, prefix, ext->ctx);
}

TreeEncoder::TreeEncoder(Grammar *_grammar, Z3Extension* _ext, int _depth):
    Z3GrammarEncoder(_grammar, _ext), depth(_depth) {
    constructTree();
}

void TreeEncoder::enlarge() {
    ++depth; constructTree();
}

namespace {
    void _collectSelectorCons(TreeEncoder::TreeNode* node, const std::unordered_map<int, z3::expr>& cache, z3::expr_vector& cons_list) {
        auto selector = cache.find(node->ind)->second;
        cons_list.push_back(selector >= 0);
        cons_list.push_back(selector < int(node->edge_list.size()));
        for (auto& info: node->sub_map) {
            for (auto* sub_node: info.second) _collectSelectorCons(sub_node, cache, cons_list);
        }
    }
}

z3::expr_vector TreeEncoder::encodeStructure(const std::string &prefix) {
    z3::expr_vector cons_list(ext->ctx);
    initSelectors(prefix);
    _collectSelectorCons(root, selector_map, cons_list);
    return cons_list;
}

namespace {
    z3::expr _encodeValueCons(TreeEncoder::TreeNode* node, const std::string& prefix, const std::unordered_map<int, z3::expr>& selector_map,
            std::unordered_map<int, z3::expr>& output_map, Z3Extension* ext, z3::expr_vector& cons_list, const Z3EncodeList& inp_list) {
        auto it = output_map.find(node->ind);
        if (it != output_map.end()) return it->second;
        auto oup_var = ext->buildVar(node->symbol->type.get(), prefix + "@" + std::to_string(node->ind));
        auto selector = selector_map.find(node->ind)->second;
        for (int edge_id = 0; edge_id < node->edge_list.size(); ++edge_id) {
            std::vector<Z3EncodeRes> sub_res; auto* edge = node->edge_list[edge_id];
            for (auto* param: edge->node_list) {
                sub_res.emplace_back(_encodeValueCons(param, prefix, selector_map, output_map, ext, cons_list, inp_list), z3::expr_vector(ext->ctx));
            }
            auto z3_res = ext->encodeZ3ExprForSemantics(edge->semantics.get(), sub_res, inp_list);
            cons_list.push_back(z3::implies(selector == ext->ctx.int_val(edge_id), z3::mk_and(z3_res.cons_list)));
            cons_list.push_back(z3::implies(selector == ext->ctx.int_val(edge_id), z3_res.res == oup_var));
        }
        output_map.insert({node->ind, oup_var});
        return oup_var;
    }
}

Z3EncodeRes TreeEncoder::encodeExample(const Z3EncodeList &inp_list, const std::string &prefix) const {
    z3::expr_vector cons_list(ext->ctx);
    std::unordered_map<int, z3::expr> output_map;
    auto res = _encodeValueCons(root, prefix, selector_map, output_map, ext, cons_list, inp_list);
    return {res, cons_list};
}

namespace {
    int getIntValue(const z3::expr& expr, const z3::model& model) {
        auto res = model.eval(expr);
        if (res.is_int()) return res.get_numeral_int();
        return 0;
    }

    PProgram _buildProgram(TreeEncoder::TreeNode* node, const std::unordered_map<int, z3::expr>& selector_map, const z3::model& model) {
        auto selector = selector_map.find(node->ind)->second;
        int sv = getIntValue(selector, model);
        assert(sv >= 0 && sv < node->edge_list.size());
        auto* edge = node->edge_list[sv];
        ProgramList sub_list;
        for (auto* sub_node: edge->node_list) {
            sub_list.push_back(_buildProgram(sub_node, selector_map, model));
        }
        return std::make_shared<Program>(edge->semantics, sub_list);
    }
}

PProgram TreeEncoder::programBuilder(const z3::model &model) const {
    return _buildProgram(root, selector_map, model);
}

namespace {
    void _getBlockCons(TreeEncoder::TreeNode* node, const std::unordered_map<int, z3::expr>& selector_map, const z3::model& model,
            z3::expr_vector& res) {
        auto selector = selector_map.find(node->ind)->second;
        int sv = getIntValue(selector, model);
        res.push_back(selector != sv);
        assert(sv >= 0 && sv < node->edge_list.size());
        auto* edge = node->edge_list[sv];
        for (auto* sub_node: edge->node_list) {
            _getBlockCons(sub_node, selector_map, model, res);
        }
    }
}

z3::expr TreeEncoder::getBlockCons(const z3::model &model) const {
    z3::expr_vector cons_list(ext->ctx);
    _getBlockCons(root, selector_map, model, cons_list);
    return z3::mk_or(cons_list);
}

TreeEncoder::~TreeEncoder() noexcept {
    delete root;
}