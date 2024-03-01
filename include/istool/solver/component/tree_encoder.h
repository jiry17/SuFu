//
// Created by pro on 2022/5/11.
//

#ifndef ISTOOL_TREE_ENCODER_H
#define ISTOOL_TREE_ENCODER_H

#include "grammar_encoder.h"

class TreeEncoder: public Z3GrammarEncoder {
public:
    struct TreeNode;
    struct TreeEdge {
        PSemantics semantics;
        std::vector<TreeNode*> node_list;
        TreeEdge(const PSemantics& _semantics, const std::vector<TreeNode*>& _node_list);
        ~TreeEdge() = default;
    };
    struct TreeNode {
        int ind;
        NonTerminal* symbol;
        std::vector<TreeEdge*> edge_list;
        std::unordered_map<std::string, std::vector<TreeNode*>> sub_map;
        TreeNode(int _ind, NonTerminal* symbol, const std::unordered_map<std::string, std::vector<TreeNode*>>& _sub_map, const std::vector<TreeEdge*>& _edge_list);
        ~TreeNode();
    };
private:
    int depth = 1;
    TreeNode* root = nullptr;
    std::unordered_map<int, z3::expr> selector_map;
    void constructTree();
    void initSelectors(const std::string& prefix);
public:

    TreeEncoder(Grammar* _grammar, Z3Extension* _ext, int _depth = 1);
    virtual void enlarge();
    virtual z3::expr_vector encodeStructure(const std::string& prefix);
    virtual Z3EncodeRes encodeExample(const Z3EncodeList &inp_list, const std::string& prefix) const;
    virtual PProgram programBuilder(const z3::model& model) const;
    virtual z3::expr getBlockCons(const z3::model &model) const;
    virtual ~TreeEncoder();
};

#endif //ISTOOL_TREE_ENCODER_H
