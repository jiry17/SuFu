//
// Created by pro on 2022/1/13.
//

#include "istool/solver/maxflash/maxflash_vsa.h"

MaxFlashEdge::MaxFlashEdge(const std::vector<MaxFlashNode *> &_node_list, const PSemantics &_semantics, double _weight):
    node_list(_node_list), semantics(_semantics), weight(_weight) {
    updateLowerBound();
}
double MaxFlashEdge::updateLowerBound() {
    lower_bound = weight;
    for (auto* sub_node: node_list) {
        lower_bound += sub_node->lower_bound;
    }
    return lower_bound;
}
std::string MaxFlashEdge::toString() const {
    std::string res = "(" + std::to_string(weight) + ", >=" + std::to_string(lower_bound) + "): " +
            semantics->getName() + "(";
    for (int i = 0; i < node_list.size(); ++i) {
        if (i) res += ","; res += node_list[i]->toString();
    }
    return res + ")";
}
bool MaxFlashEdge::isFinished() const {
    for (auto* node: node_list) {
        if (!node->best_program) return false;
    }
    return true;
}
PProgram MaxFlashEdge::getBestProgram() const {
    ProgramList sub_list;
    for (auto* node: node_list) {
        assert(node->best_program);
        sub_list.push_back(node->best_program);
    }
    return std::make_shared<Program>(semantics, sub_list);
}
double MaxFlashEdge::getAverageRemain(double upper_bound) const {
    int unfinished_num = 0;
    for (auto* sub_node: node_list) {
        if (!sub_node->best_program) unfinished_num += 1;
    }
    assert(unfinished_num);
    return (upper_bound - lower_bound) / unfinished_num;
}

MaxFlashNode::MaxFlashNode(int _node_id, double _lower_bound, const std::string& _oup_string):
    node_id(_node_id), lower_bound(_lower_bound), is_build_edge(false), best_program(nullptr), oup_string(_oup_string) {
    name = std::to_string(node_id) + "@" + oup_string;
}
std::string MaxFlashNode::toString() const {
    return name;
}

SingleMaxFlashNode::SingleMaxFlashNode(int _node_id, double _lower_bound, const WitnessData &_oup):
    MaxFlashNode(_node_id, _lower_bound, _oup->toString()), oup(_oup) {
}

MultiMaxFlashNode::MultiMaxFlashNode(int _node_id, MaxFlashNode *_l, SingleMaxFlashNode *_r):
    MaxFlashNode(_node_id, std::max(_l->lower_bound, _r->lower_bound), _l->oup_string + "@" + _r->oup_string), l(_l), r(_r) {
}

namespace {
    const double KDoubleINF = 1e100;
}

double SingleMaxFlashNode::updateLowerBound() {
    if (!is_build_edge) return lower_bound;
    lower_bound = KDoubleINF;
    for (auto& edge: edge_list) {
        lower_bound = std::min(lower_bound, edge.updateLowerBound());
    }
    return lower_bound;
}

double MultiMaxFlashNode::updateLowerBound() {
    if (!is_build_edge) return lower_bound = std::max(l->lower_bound, r->lower_bound);
    lower_bound = KDoubleINF;
    for (auto& edge: edge_list) {
        lower_bound = std::min(lower_bound, edge.updateLowerBound());
    }
    return lower_bound = std::max(lower_bound, std::max(l->lower_bound, r->lower_bound));
}
