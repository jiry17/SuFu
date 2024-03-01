//
// Created by pro on 2022/1/13.
//

#ifndef ISTOOL_CONTEXTED_VSA_H
#define ISTOOL_CONTEXTED_VSA_H

#include <istool/basic/program.h>
#include "istool/basic/semantics.h"
#include "istool/ext/vsa/witness_value.h"

class MaxFlashNode;

class MaxFlashEdge {
public:
    PSemantics semantics;
    std::vector<MaxFlashNode*> node_list;
    double weight, lower_bound;
    double updateLowerBound();
    MaxFlashEdge(const std::vector<MaxFlashNode*>& _node_list, const PSemantics& _semantics, double _weight);
    std::string toString() const;
    bool isFinished() const;
    PProgram getBestProgram() const;
    double getAverageRemain(double upper_bound) const;
};

class MaxFlashNode {
public:
    std::string oup_string, name;
    int node_id;
    double lower_bound;
    std::vector<MaxFlashEdge> edge_list;
    bool is_build_edge;
    PProgram best_program;
    MaxFlashNode(int _node_id, double _lower_bound, const std::string& oup_string);
    std::string toString() const;
    virtual ~MaxFlashNode() = default;
    virtual double updateLowerBound() = 0;
};

class SingleMaxFlashNode: public MaxFlashNode {
public:
    WitnessData oup;
    SingleMaxFlashNode(int _node_id, double _lower_bound, const WitnessData& _oup);
    virtual ~SingleMaxFlashNode() = default;
    virtual double updateLowerBound();
};

class MultiMaxFlashNode: public MaxFlashNode {
public:
    MaxFlashNode *l;
    SingleMaxFlashNode *r;
    MultiMaxFlashNode(int _node_id, MaxFlashNode* _l, SingleMaxFlashNode* _r);
    virtual ~MultiMaxFlashNode() = default;
    virtual double updateLowerBound();
};

#endif //ISTOOL_CONTEXTED_VSA_H
