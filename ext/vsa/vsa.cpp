//
// Created by pro on 2021/12/29.
//

#include "istool/ext/vsa/vsa.h"
#include "glog/logging.h"
#include <unordered_set>
#include <queue>
#include <iostream>

VSAEdge::VSAEdge(const PSemantics &_semantics, const VSANodeList &_node_list): semantics(_semantics), node_list(_node_list) {
}
std::string VSAEdge::toString() const {
    std::string res = semantics->getName() + "(";
    for (int i = 0; i < node_list.size(); ++i) {
        if (i) res += ",";
        res += node_list[i]->toString();
    }
    return res + ")";
}
VSANode::VSANode(NonTerminal *_symbol, int _example_num): symbol(_symbol), example_num(_example_num), id(0) {
}
std::string VSANode::toString() {
    return symbol->name + "@" + getWitnessString();
}


SingleVSANode::SingleVSANode(NonTerminal *_symbol, const WitnessData &_oup): VSANode(_symbol, 1), oup(_oup) {
}
MultiExampleVSANode::MultiExampleVSANode(VSANode* _l, VSANode* _r):
    l(_l), r(_r), VSANode(_l->symbol, _l->example_num + _r->example_num) {
    if (l->symbol->name != r->symbol->name) {
        LOG(FATAL) << "Two subnodes of a MultiExampleVSANode should come from the same symbol";
    }
}

namespace {
    void _indexVSANode(VSANode* node, int &n, std::unordered_set<VSANode*>& node_set) {
        if (node_set.find(node) != node_set.end()) return;
        node_set.insert(node); node->id = n++;
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                _indexVSANode(sub_node, n, node_set);
            }
        }
    }
}

int ext::vsa::indexVSANode(VSANode* root) {
    int n = 0;
    std::unordered_set<VSANode*> node_set;
    _indexVSANode(root, n, node_set);
    return n;
}

namespace {
    void _collectAllNodes(VSANode* node, VSANodeList& node_list) {
        if (node_list[node->id]) return;
        node_list[node->id] = node;
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                _collectAllNodes(sub_node, node_list);
            }
        }
    }
}

void ext::vsa::cleanUpVSA(VSANode* root) {
    int n = indexVSANode(root);
    VSANodeList node_list(n, nullptr);
    _collectAllNodes(root, node_list);
    std::vector<std::vector<int>> edge_index(n);
    std::vector<std::vector<std::pair<int, int>>> reversed_edge(n);
    std::vector<bool> is_empty(n, true);
    for (const auto& node: node_list) {
        int node_id = node->id;
        for (int edge_id = 0; edge_id < node->edge_list.size(); ++edge_id) {
            auto& edge = node->edge_list[edge_id];
            edge_index[node_id].push_back(edge.node_list.size());
            for (auto* sub_node: edge.node_list) {
                int sub_id = sub_node->id;
                reversed_edge[sub_id].emplace_back(node_id, edge_id);
            }
        }
    }
    std::queue<int> Q;
    auto insert = [&](int id) {
        if (!is_empty[id]) return;
        is_empty[id] = false; Q.push(id);
    };
    for (const auto& node: node_list) {
        for (auto index: edge_index[node->id]) {
            if (!index) {
                insert(node->id); break;
            }
        }
    }
    while (!Q.empty()) {
        int id = Q.front(); Q.pop();
        for (auto& re: reversed_edge[id]) {
            auto pre_node_id = re.first, pre_edge_id = re.second;
            edge_index[pre_node_id][pre_edge_id]--;
            if (!edge_index[pre_node_id][pre_edge_id]) insert(pre_node_id);
        }
    }
    for (auto& node: node_list) {
        int now = 0;
        for (auto& edge: node->edge_list) {
            bool is_empty_edge = false;
            for (auto *sub_node: edge.node_list) {
                if (is_empty[sub_node->id]) {
                    is_empty_edge = true;
                    break;
                }
            }
            if (!is_empty_edge) node->edge_list[now++] = edge;
        }
        while (node->edge_list.size() > now) node->edge_list.pop_back();
        assert(node->edge_list.empty() == is_empty[node->id]);
    }
}

namespace {
    bool _isAcyclic(VSANode* node, std::vector<bool>& in_stack, std::vector<bool>& visited) {
        if (visited[node->id]) return !in_stack[node->id];
        visited[node->id] = true; in_stack[node->id] = true;
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                if (!_isAcyclic(sub_node, in_stack, visited)) {
                    return false;
                }
            }
        }
        in_stack[node->id] = false;
        return true;
    }
}

bool ext::vsa::isAcyclic(VSANode *root, int n) {
    if (n == -1) n = indexVSANode(root);
    std::vector<bool> in_stack(n);
    std::vector<bool> visited(n);
    for (int i = 0; i < n; ++i) in_stack[i] = false, visited[i] = false;
    return _isAcyclic(root, in_stack, visited);
}

int ext::vsa::indexVSANodeByTopoSort(VSANode *root) {
    int n = indexVSANode(root); assert(isAcyclic(root, n));
    std::vector<VSANode*> node_list(n, nullptr);
    _collectAllNodes(root, node_list);
    std::vector<VSANode*> topo_list;
    std::vector<std::vector<VSANode*>> rev_edge_list(n);
    std::vector<int> out_deg_list(n, 0);

    for (auto* node: node_list) {
        for (const auto& edge: node->edge_list) {
            for (auto* sub_node: edge.node_list) {
                rev_edge_list[sub_node->id].push_back(node);
                out_deg_list[node->id]++;
            }
        }
    }

    std::queue<VSANode*> Q;
    for (int i = 0; i < n; ++i) {
        if (out_deg_list[i] == 0) Q.push(node_list[i]);
    }
    while (!Q.empty()) {
        auto* node = Q.front(); topo_list.push_back(node);
        for (auto* pre: rev_edge_list[node->id]) {
            out_deg_list[pre->id]--;
            if (out_deg_list[pre->id] == 0) {
                Q.push(pre);
            }
        }
        Q.pop();
    }

    std::reverse(topo_list.begin(), topo_list.end());
    for (int i = 0; i < topo_list.size(); ++i) {
        topo_list[i]->id = i;
    }
    return n;
}

std::string SingleVSANode::getWitnessString() {
    return oup->toString();
}

std::string MultiExampleVSANode::getWitnessString() {
    return l->toString() + "|" + r->toString();
}

void ext::vsa::printVSA(VSANode* root) {
    int n = indexVSANode(root); VSANodeList node_list(n, nullptr);
    _collectAllNodes(root, node_list);
    for (const auto& node: node_list) {
        std::cout << "node #" << node->id << ": " << node->toString() << std::endl;
        for (const auto& edge: node->edge_list) {
            std::cout << "  " << edge.semantics->getName() + "(";
            for (int i = 0; i < edge.node_list.size(); ++i) {
                if (i) std::cout <<","; std::cout << edge.node_list[i]->id;
            }
            std::cout << ")" << std::endl;
        }
    }
}

void ext::vsa::deleteVSA(VSANode *root) {
    int n = indexVSANode(root);
    VSANodeList node_list(n, nullptr); _collectAllNodes(root, node_list);
    for (auto* node: node_list) delete node;
}

namespace {
    int _getEdgeSize(VSANode* node, std::unordered_set<VSANode*>& cache) {
        if (cache.find(node) != cache.end()) return 0;
        cache.insert(node); int res = 0;
        for (const auto& edge: node->edge_list) {
            res += edge.node_list.size();
            for (auto* sub: edge.node_list) {
                res += _getEdgeSize(sub, cache);
            }
        }
        return res;
    }
}

int ext::vsa::getEdgeSize(VSANode *root) {
    std::unordered_set<VSANode*> cache;
    return _getEdgeSize(root, cache);
}

namespace {
    double _getProgramNum(VSANode* root, std::unordered_map<VSANode*, double>& cache) {
        if (cache.find(root) != cache.end()) return cache[root];
        double res = 0;
        for (auto& e: root->edge_list) {
            double edge_num = 1.0;
            for (auto* p: e.node_list) edge_num *= _getProgramNum(p, cache);
            res += edge_num;
        }
        return res;
    }
}

double ext::vsa::getProgramNum(VSANode *root) {
    std::unordered_map<VSANode*, double> cache;
    return _getProgramNum(root, cache);
}