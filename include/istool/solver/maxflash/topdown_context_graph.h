//
// Created by pro on 2022/1/13.
//

#ifndef ISTOOL_TOPDOWN_CONTEXT_GRAPH_H
#define ISTOOL_TOPDOWN_CONTEXT_GRAPH_H

#include "istool/ext/vsa/top_down_model.h"

class TopDownGraphMatchStructure {
public:
    int edge_id;
    PProgram program;
    std::vector<TopDownGraphMatchStructure*> sub_list;
    TopDownGraphMatchStructure(int _edge_id, const PProgram& _program, const std::vector<TopDownGraphMatchStructure*>& _sub_list);
    ~TopDownGraphMatchStructure();
};

class TopDownContextGraph {
public:
    struct Edge {
        std::vector<int> v_list;
        double weight;
        int u;
        PSemantics semantics;
        Edge(int _u, const std::vector<int>& _v_list, double _weight, const PSemantics& _semantics);
    };

    struct Node {
        PTopDownContext context;
        NonTerminal* symbol;
        std::vector<Edge> edge_list;
        double lower_bound;
        std::string toString() const;
        Node(NonTerminal* _symbol, const PTopDownContext& _context);
        ~Node() = default;
    };

    std::vector<Node> node_list;
    ProbModelType prob_type;

    void initNodeLowerBound();
    TopDownContextGraph(Grammar* g, TopDownModel* model, ProbModelType _prob_type);
    void normalizeProbability();
    TopDownGraphMatchStructure* getProgramMatchStructure(const PProgram& p);
    ~TopDownContextGraph() = default;
    void print() const;
};

#endif //ISTOOL_TOPDOWN_CONTEXT_GRAPH_H
