//
// Created by pro on 2021/12/10.
//

#include "istool/solver/component/linear_encoder.h"
#include "glog/logging.h"

LinearEncoder::LinearEncoder(Grammar *_grammar, Z3Extension *_ext, int _factor, const std::map<std::string, int> &_special_usage):
        Z3GrammarEncoder(_grammar, _ext), factor(_factor), special_usage(_special_usage) {
}
void LinearEncoder::enlarge() {
    factor += 1;
}

namespace {
    z3::expr buildIntVar(const std::string& name, z3::context& ctx) {
        return ctx.int_const(name.c_str());
    }

    int getIntValue(const z3::expr& expr, const z3::model& model) {
        auto res = model.eval(expr);
        if (res.is_int()) return res.get_numeral_int();
        return 0;
    }
}

LinearEncoder::Component LinearEncoder::buildComponent(NonTerminal *nt, Rule *r, const std::string& prefix) {
    std::string name_prefix = prefix + "@" + r->toString() + "@";
    auto oup = buildIntVar(name_prefix + "oup_ind", ext->ctx);
    std::vector<z3::expr> inp_list;
    for (int i = 0; i < r->param_list.size(); ++i) {
        auto inp = buildIntVar(name_prefix + "inp_ind@" + std::to_string(i), ext->ctx);
        inp_list.push_back(inp);
    }
    return {nt, r, oup, inp_list};
}

z3::expr_vector LinearEncoder::encodeStructure(const std::string &prefix) {
    component_list.clear();
    base->indexSymbol();
    for (int id = 0; id < base->symbol_list.size(); ++id) {
        auto* symbol = base->symbol_list[id];
        for (auto* rule: base->symbol_list[id]->rule_list) {
            auto* cr = dynamic_cast<ConcreteRule*>(rule);
            if (!cr) LOG(FATAL) << "LinearEncoder supports only ConcreteRule";
            std::string op_name = cr->semantics->name;
            int num = factor;
            if (special_usage.find(op_name) != special_usage.end()) {
                num = special_usage.find(op_name)->second;
            }
            for (int i = 0; i < num; ++i) {
                int component_id = component_list.size();
                auto component = buildComponent(symbol, rule, prefix + "@" + std::to_string(component_id));
                component_list.push_back(component);
            }
        }
    }

    z3::expr_vector cons_list(ext->ctx);

    //structure constraint
    z3::expr_vector oup_list(ext->ctx);
    for (const auto& cx: component_list) {
        int lim = component_list.size();
        if (cx.nt->id) lim -= 1;
        oup_list.push_back(cx.oup);
        cons_list.push_back(cx.oup >= 1 && cx.oup <= lim);
        for (int i = 0; i < cx.inp_list.size(); ++i) {
            auto* symbol = cx.rule->param_list[i];
            cons_list.push_back(cx.inp_list[i] >= 1 && cx.inp_list[i] < cx.oup);
            for (const auto& cy: component_list) {
                if (symbol->id != cy.nt->id) {
                    cons_list.push_back(cx.inp_list[i] != cy.oup);
                }
            }
        }
    }
    cons_list.push_back(z3::distinct(oup_list));
    return cons_list;
}

namespace {
    struct ValueComponent {
        z3::expr oup_value;
        z3::expr_vector inp_value_list;
    };

    ValueComponent buildValueComponent(const LinearEncoder::Component& component, const std::string& prefix, Z3Extension* ext) {
        std::string name_prefix = prefix + component.rule->toString() + "@";
        std::string oup_name = name_prefix + "oup_value";
        auto oup_value = ext->buildVar(component.nt->type.get(), oup_name);
        z3::expr_vector inp_value_list(ext->ctx);
        for (int i = 0; i < component.rule->param_list.size(); ++i) {
            std::string inp_name = name_prefix + "inp_value@" + std::to_string(i);
            auto inp_value = ext->buildVar(component.rule->param_list[i]->type.get(), inp_name);
            inp_value_list.push_back(inp_value);
        }
        return {oup_value, inp_value_list};
    }
}

Z3EncodeRes LinearEncoder::encodeExample(const Z3EncodeList &inp_list, const std::string &prefix) const {
    base->indexSymbol();
    // build value component
    std::vector<ValueComponent> value_component_list;
    for (int i = 0; i < component_list.size(); ++i) {
        value_component_list.push_back(buildValueComponent(component_list[i], prefix, ext));
    }
    auto res = ext->buildVar(base->start->type.get(), prefix + "res");

    z3::expr_vector cons_list(ext->ctx);
    // semantics constraint
    for (int i = 0; i < component_list.size(); ++i) {
        auto* cr = dynamic_cast<ConcreteRule*>(component_list[i].rule);
        if (!cr) LOG(FATAL) << "LinearEncoder supports only ConcreteRule";
        const auto& semantics = cr->semantics;
        const auto& vc = value_component_list[i];
        std::vector<Z3EncodeRes> sub_list;
        //std::cout << "Encode component " << i << " " << component_list[i].rule->semantics->getName() << std::endl;
        //std::cout << vc.inp_value_list << std::endl;
        for (const auto& expr: vc.inp_value_list) {
            z3::expr_vector empty_cons_list(ext->ctx);
            sub_list.emplace_back(expr, empty_cons_list);
        }
        auto encode_res = ext->encodeZ3ExprForSemantics(semantics.get(), sub_list, inp_list);
        cons_list.push_back(encode_res.res == vc.oup_value);
        for (const auto& cons: encode_res.cons_list) cons_list.push_back(cons);
    }

    // structure constraint
    for (int idx = 0; idx < component_list.size(); ++idx) {
        auto& cx = component_list[idx];
        auto& vcx = value_component_list[idx];
        for (int i = 0; i < cx.inp_list.size(); ++i) {
            auto* symbol = cx.rule->param_list[i];
            for (int idy = 0; idy < component_list.size(); ++idy) {
                if (idx == idy) continue;
                auto& cy = component_list[idy];
                auto& vcy = value_component_list[idy];
                if (cy.nt->id != symbol->id) continue;
                cons_list.push_back(z3::implies(cx.inp_list[i] == cy.oup, vcx.inp_value_list[i] == vcy.oup_value));
            }
        }
        if (cx.nt->id == 0) {
            cons_list.push_back(z3::implies(cx.oup == int(component_list.size()), res == vcx.oup_value));
        }
    }
    return {res, cons_list};
}

PProgram LinearEncoder::programBuilder(int id, const z3::model &model) const {
    int pos = -1;
    for (int i = 0; i < component_list.size(); ++i) {
        int oup_data = getIntValue(component_list[i].oup, model);
        if (oup_data == id) {
            assert(pos == -1); pos = i;
        }
    }
    assert(pos != -1);
    auto& c = component_list[pos];
    ProgramList sub_list;
    for (auto& inp_var: c.inp_list) {
        int inp_id = getIntValue(inp_var, model);
        assert(inp_id != -1);
        sub_list.push_back(programBuilder(inp_id, model));
    }
    return c.rule->buildProgram(std::move(sub_list));
}

PProgram LinearEncoder::programBuilder(const z3::model &model) const {
    return programBuilder(component_list.size(), model);
}

void LinearEncoder::getBlockCons(int id, const z3::model &model, std::vector<bool> &cache, z3::expr_vector &res) const {
    if (cache[id]) return; cache[id] = true;
    int pos = -1;
    for (int i = 0; i < component_list.size(); ++i) {
        int oup_data = getIntValue(component_list[i].oup, model);
        if (oup_data == id) {
            assert(pos == -1); pos = i;
        }
    }
    assert(pos != -1);
    auto& c = component_list[pos];
    res.push_back(c.oup != id);
    for (auto& inp_var: c.inp_list) {
        int inp_id = getIntValue(inp_var, model);
        assert(inp_id != -1);
        getBlockCons(inp_id, model, cache, res);
    }
}

z3::expr LinearEncoder::getBlockCons(const z3::model &model) const {
    std::vector<bool> cache(component_list.size() + 1, false);
    z3::expr_vector res(ext->ctx);
    getBlockCons(component_list.size(), model, cache, res);
    return z3::mk_and(res);
}
