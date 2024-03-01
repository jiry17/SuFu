//
// Created by pro on 2021/12/10.
//

#ifndef ISTOOL_LINEAR_ENCODER_H
#define ISTOOL_LINEAR_ENCODER_H

#include "grammar_encoder.h"

class LinearEncoder: public Z3GrammarEncoder {
public:
    struct Component {
        NonTerminal* nt;
        Rule* rule;
        z3::expr oup;
        std::vector<z3::expr> inp_list;
    };
private:
    std::vector<Component> component_list;

    Component buildComponent(NonTerminal* nt, Rule* r, const std::string& prefix);
    PProgram programBuilder(int id, const z3::model& model) const;
    void getBlockCons(int id, const z3::model& model, std::vector<bool>& cache, z3::expr_vector& res) const;

    int factor = 1;
    std::map<std::string, int> special_usage;
public:
    LinearEncoder(Grammar* _grammar, Z3Extension* _ext, int _factor = 1, const std::map<std::string, int>& _special_usage = {});
    virtual void enlarge();
    virtual z3::expr_vector encodeStructure(const std::string& prefix);
    virtual Z3EncodeRes encodeExample(const Z3EncodeList &inp_list, const std::string& prefix) const;
    virtual PProgram programBuilder(const z3::model& model) const;
    virtual z3::expr getBlockCons(const z3::model &model) const;
    virtual ~LinearEncoder() = default;
};


#endif //ISTOOL_LINEAR_ENCODER_H
