//
// Created by pro on 2022/1/22.
//

#include "istool/solver/autolifter/basic/lifting_solver.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/basic/type_system.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/ext/deepcoder/higher_order_operator.h"
#include "istool/ext/deepcoder/data_util.h"
#include "glog/logging.h"
#include <iostream>

namespace {
    PProgram _buildTmp(const std::string& name) {
        return std::make_shared<Program>(std::make_shared<TmpSemantics>(name), (ProgramList){});
    }

    PProgram _buildComponentMap(const PType& F, std::unordered_map<std::string, PProgram>& m, const std::string& current_name, const std::string& prefix) {
        auto* pt = dynamic_cast<TProduct*>(F.get());
        if (pt) {
            ProgramList sub_list;
            for (int i = 0; i < pt->sub_types.size(); ++i) {
                sub_list.push_back(_buildComponentMap(pt->sub_types[i], m, current_name + "." + std::to_string(i), prefix + "_" + std::to_string(i)));
            }
            auto prog = ext::ho::buildProduct(sub_list);
            return m[current_name] = prog;
        }
        return m[current_name] = _buildTmp(prefix);
    }

    PProgram _buildComponentMap(const PType& F, TProduct* x, std::unordered_map<std::string, PProgram>& m,
            const std::string& current_name, int& a_id, int& p_id, int& f_id) {
        auto* pt = dynamic_cast<TProduct*>(F.get());
        if (pt) {
            ProgramList sub_list;
            for (int i = 0; i < pt->sub_types.size(); ++i) {
                sub_list.push_back(_buildComponentMap(pt->sub_types[i], x, m, current_name + "." + std::to_string(i),
                        a_id, p_id, f_id));
            }
            auto prog = ext::ho::buildProduct(sub_list);
            return m[current_name] = prog;
        }
        auto* xt = dynamic_cast<TVar*>(F.get());
        if (xt) {
            auto p_prog = _buildComponentMap(x->sub_types[0], m, current_name + "." + std::to_string(0), "p" + std::to_string(p_id++));
            auto f_prog = _buildComponentMap(x->sub_types[1], m, current_name + "." + std::to_string(1), "f" + std::to_string(f_id++));
            auto prog = ext::ho::buildProduct({p_prog, f_prog});
            return m[current_name] = prog;
        }
        return m[current_name] = _buildTmp("a" + std::to_string(a_id++));
    }

    int _getDepth(Type* type) {
        auto* pt = dynamic_cast<TProduct*>(type);
        if (dynamic_cast<TProduct*>(pt)) {
            int res = 0;
            for (const auto& element: pt->sub_types) {
                res = std::max(res, _getDepth(element.get()));
            }
            return res + 1;
        }
        return 0;
    }

    PProgram _rewriteCombinator(const PProgram& c, const std::unordered_map<std::string, PProgram>& m) {
        auto feature = c->toString();
        auto it = m.find(feature);
        if (it != m.end()) return it->second;
        ProgramList sub_list;
        for (auto& sub: c->sub_list) {
            sub_list.push_back(_rewriteCombinator(sub, m));
        }
        return std::make_shared<Program>(c->semantics, sub_list);
    }

    std::string _getOupList(const std::string& prefix, const PType& type, std::vector<std::pair<std::string, std::string>>& res, const PProgram& c) {
        auto* px = dynamic_cast<TProduct*>(type.get());
        if (px) {
            std::string name = "(";
            for (int i = 0; i < px->sub_types.size(); ++i) {
                if (i) name += ",";
                auto current_prog = ext::ho::buildAccess(c, {i});
                auto prog_name = ext::ho::removeAccessProd(current_prog)->toString();
                auto oup_name = prefix + std::to_string(i);
                name += oup_name;
                res.emplace_back(oup_name, prog_name);
            }
            return name + ")";
        }
        auto prog_name = ext::ho::removeAccessProd(c)->toString();
        auto oup_name = prefix;
        res.emplace_back(oup_name, prog_name);
        return oup_name;
    }

    void _styledPrintCombinator(int id, const PType& F, const PType& x, const PProgram& c) {
        auto* px = dynamic_cast<TProduct*>(x.get());
        if (!px || px->sub_types.size() != 2 || _getDepth(px) > 2) {
            LOG(FATAL) << "Unexpected x type " << x->getName();
        }
        std::unordered_map<std::string, PProgram> substitute_map;
        int a_id = 0, p_id = 0, c_id = 0;
        auto param_name = _buildComponentMap(F, px, substitute_map, "Param0", a_id, p_id, c_id)->toString();
        auto rewrite_c = _rewriteCombinator(c, substitute_map);

        std::vector<std::pair<std::string, std::string>> oup_list;
        std::string p_name = _getOupList("combined_p", px->sub_types[0], oup_list, ext::ho::buildAccess(rewrite_c, {0}));
        std::string f_name = _getOupList("combined_f", px->sub_types[1], oup_list, ext::ho::buildAccess(rewrite_c, {1}));
        std::cout << "c" << id << " " << param_name << " = (" << p_name << "," << f_name << ")" << std::endl;
        std::cout << "  where" << std::endl;
        for (const auto& info: oup_list) {
            std::cout << "  " << info.first << " = " << info.second << std::endl;
        }
    }
}

LiftingResInfo::LiftingResInfo(const PType &_F, const PProgram &_c, const PProgram &_m): F(_F), c(_c), m(_m) {
}
LiftingRes::LiftingRes(const PProgram &_p, const PProgram &_h, const PProgram &_f, const std::vector<LiftingResInfo> &_info_list, const PEnv& _env):
    p(_p), h(_h), f(_f), info_list(_info_list), env(_env){
}
SingleLiftingRes::SingleLiftingRes(const PProgram &_p, const PProgram &_h, const PProgram &_f, const LiftingResInfo &_info):
    p(_p), h(_h), f(_f), info(_info) {
}

std::string LiftingRes::toString() const {
    auto res = "f: " + f->toString() + "; c: ";
    for (int i = 0; i < info_list.size(); ++i) {
        if (i) res += ", "; res += info_list[i].c->toString();
    }
    return res;
}
void LiftingRes::styledPrint() const {
    auto styled_f = program::rewriteParam(f, {_buildTmp("l")});
    std::cout << "f l = " << styled_f->toString() << std::endl;
    auto f_type = type::getTypeExtension(env.get())->getType(f.get());
    auto p_type = type::getTypeExtension(env.get())->getType(p.get());
    auto x = std::make_shared<TProduct>((TypeList){p_type, f_type});
    for (int i = 0; i < info_list.size(); ++i) {
        _styledPrintCombinator(i, info_list[i].F, x, info_list[i].c);
    }
}
std::string SingleLiftingRes::toString() const {
    return "f: " + f->toString() + "; c: " + info.c->toString();
}

LiftingSolver::LiftingSolver(LiftingTask *_task): task(_task) {}
SFSolver::SFSolver(PartialLiftingTask *_task): task(_task) {}

