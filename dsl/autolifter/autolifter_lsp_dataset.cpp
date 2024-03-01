//
// Created by pro on 2022/3/8.
//

#include "istool/dsl/autolifter/autolifter_dataset.h"
#include "istool/dsl/autolifter/autolifter_dsl.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/limited_type/limited_int.h"
#include "istool/ext/limited_type/limited_ds.h"
#include "istool/sygus/theory/basic/clia/clia_type.h"
#include "istool/sygus/theory/basic/clia/clia_value.h"
#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/solver/polygen/lia_solver.h"
#include "istool/solver/polygen/polygen_term_solver.h"
#include "glog/logging.h"

using namespace dsl::autolifter;
#define TVARA type::getTVarA()
#define TINT theory::clia::getTInt()
#define TLISTINT ext::ho::getTIntList()
using theory::clia::getIntValue;

namespace {

    struct _TypeConfig {
        int int_min = -5, int_max = 5;
        int length_max = 5;
    };

    PType _getLimitedInt(const _TypeConfig &config) {
        return std::make_shared<LimitedTInt>(config.int_min, config.int_max);
    }

    PType _getLimitedListInt(const _TypeConfig &config) {
        auto content = _getLimitedInt(config);
        return std::make_shared<LimitedTList>(config.length_max, content);
    }

    PProgram _buildAccess(const PProgram &base, int id) {
        auto sem = std::make_shared<AccessSemantics>(id);
        return std::make_shared<Program>(sem, (ProgramList) {base});
    }

    std::vector<int> _getListContent(const Data &d) {
        std::vector<int> res;
        auto *lv = dynamic_cast<ListValue *>(d.get());
        for (const auto &c: lv->value) res.push_back(getIntValue(c));
        return res;
    }

    LiftingModConfigInfo _getModConfigInfoFoldR(Env *env, const _TypeConfig &config) {
        auto F = std::make_shared<TProduct>((TypeList) {TVARA, _getLimitedInt(config)});
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList) {TLISTINT, TINT}));
        auto m = std::make_shared<Program>(env->getSemantics("append"),
                                           (ProgramList) {_buildAccess(x, 0), _buildAccess(x, 1)});
        return {m, F};
    }

    LiftingModConfigInfo _getModConfigInfoTail(Env *env, const _TypeConfig &config) {
        PType F = std::make_shared<TProduct>((TypeList) {_getLimitedInt(config), TVARA});
        auto checker = [](Value *value) -> bool {
            auto *pv = dynamic_cast<ProductValue *>(value);
            assert(pv);
            int x = getIntValue(pv->elements[0]);
            auto y = _getListContent(pv->elements[1]);
            return !y.empty() && y[0] == x;
        };
        F = std::make_shared<RefinedType>(F, checker);
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList) {TINT, TLISTINT}));
        auto m = std::make_shared<Program>(env->getSemantics("tail"), (ProgramList) {_buildAccess(x, 1)});
        return {m, F};
    }

    typedef std::function<bool(int, int)> BinaryCmp;

    LiftingModConfigInfo _getModConfigInfoPartition(Env *env, const BinaryCmp &cmp, const _TypeConfig &config) {
        PType F = std::make_shared<TProduct>((TypeList) {TVARA, _getLimitedInt(config), TVARA});
        auto checker = [cmp](Value *value) {
            auto *pv = dynamic_cast<ProductValue *>(value);
            int x = getIntValue(pv->elements[1]);
            auto l = _getListContent(pv->elements[0]), r = _getListContent(pv->elements[2]);
            for (auto v: l) if (!cmp(v, x)) return false;
            for (auto v: r) if (cmp(x, v)) return false;
            return true;
        };
        F = std::make_shared<RefinedType>(F, checker);
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList) {TLISTINT, TINT, TLISTINT}));
        auto m = std::make_shared<Program>(env->getSemantics("append"),
                                           (ProgramList) {_buildAccess(x, 0), _buildAccess(x, 1)});
        m = std::make_shared<Program>(env->getSemantics("++"), (ProgramList) {m, _buildAccess(x, 2)});
        return {m, F};
    }

    PProgram _buildListProgram(const FullSemanticsFunction &ps, const std::string &name) {
        auto sem = std::make_shared<TypedAnonymousSemantics>(ps, (TypeList) {TLISTINT}, theory::clia::getTInt(), name);
        auto p = std::make_shared<Program>(sem, (ProgramList) {program::buildParam(0, TLISTINT)});
        return p;
    }

#define InfoStart(name, str_name) \
    LiftingConfigInfo _getConfigInfo ## name() { \
        auto env = std::make_shared<Env>(); \
        dsl::autolifter::prepareEnv(env.get()); \
        _TypeConfig type_config; \
        auto inp_type = _getLimitedListInt(type_config);

#define InfoEnd(name, str_name) \
        return {_buildListProgram(ps, str_name), inp_type, env, {mod_config}}; \
    }

    InfoStart(Page9T1, "page9-t1")
        auto mod_config = _getModConfigInfoFoldR(env.get(), type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto l(_getListContent(inp_list[0]));
            for (int i = 1; i < l.size(); ++i) {
                if (l[i - 1] >= l[i]) return BuildData(Int, 0);
            }
            return BuildData(Int, 1);
        };
    InfoEnd(Page9T1, "page9-t1")

    InfoStart(Page9T2, "page9-t2")
        auto mod_config = _getModConfigInfoFoldR(env.get(), type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto l(_getListContent(inp_list[0]));
            if (l.empty()) throw SemanticsError();
            for (auto v: l) if (v < l[0]) return BuildData(Int, 0);
            return BuildData(Int, 1);
        };
    InfoEnd(Page9T2, "page9-t2")

    InfoStart(Page10, "page10")
        auto mod_config = _getModConfigInfoFoldR(env.get(), type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto l(_getListContent(inp_list[0]));
            if (l.empty()) throw SemanticsError();
            for (auto v: l) if (v < l[0]) return BuildData(Int, 0);
            for (int i = 1; i < l.size(); ++i) {
                if (l[i] - l[i - 1] >= 2) return BuildData(Int, 0);
            }
            return BuildData(Int, 1);
        };
    InfoEnd(Page10, "page10")

    InfoStart(Page12, "page12")
        auto fold_mod_config = _getModConfigInfoFoldR(env.get(), type_config);
        auto tail_mod_config = _getModConfigInfoTail(env.get(), type_config);
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0]));
            int sum = 0;
            for (int i = 0; i < l.size(); ++i) {
                sum += l[i];
            }
            return BuildData(Int, sum <= 0);
        };
        return {_buildListProgram(ps, "page12"), inp_type, env, {fold_mod_config, tail_mod_config}};
    };//InfoEnd(Page12, "page12")

    InfoStart(Page20, "page20")
        auto mod_config = _getModConfigInfoPartition(env.get(), [](int x, int y){return x <= y;}, type_config);
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0]));
            int ans = 0;
            for (int i = 0; i < l.size() - ans; ++i) {
                int ma = l[i];
                for (int j = i; j < l.size(); ++j) {
                    ma = std::max(ma, l[j]);
                    if (ma < j - i + 1) ans = std::max(ans, j - i + 1);
                }
            }
            return BuildData(Int, ans);
        };
        env->setConst(solver::autolifter::KComposedNumName, BuildData(Int, 4));
    InfoEnd(Page20, "page20")

    InfoStart(Page21, "page21")
        auto mod_config = _getModConfigInfoPartition(env.get(), [](int x, int y){return x <= y;}, type_config);
        auto ps = [](DataList && inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0])); int ans = 0;
            for (int i = 0; i < l.size() - ans; ++i) {
                int ma = l[i];
                for (int j = i; j < l.size(); ++j) {
                    if (l[j] < l[i]) break;
                    ma = std::max(ma, l[j]);
                    if (ma == l[j]) ans = std::max(ans, j - i + 1);
                }
            }
            return BuildData(Int, ans);
        };
        auto min_pos = [](DataList&& inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0]));
            if (l.empty()) return BuildData(List, (DataList){});
            int mi = l[0]; DataList res;
            for (int i = 0; i < l.size(); ++i) {
                mi = std::min(mi, l[i]);
                if (mi == l[i]) res.push_back(BuildData(Int, i));
            }
            return BuildData(List, res);
        };
        auto min_pos_sem = std::make_shared<TypedAnonymousSemantics>(min_pos, (TypeList){TLISTINT}, TLISTINT, "min-pos");
        env->setConst(solver::lia::KConstIntMaxName, BuildData(Int, 2));
        return {_buildListProgram(ps, "psgae21"), inp_type, env, {mod_config}, {min_pos_sem}};
    } // InfoEnd(Page21, "page21")

    InfoStart(Page22T1, "page22-t1")
        auto mod_config = _getModConfigInfoPartition(env.get(), [](int x, int y){return x <= y;}, type_config);
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0])); int ans = 0;
            for (int i = 0; i < l.size() - ans; ++i) {
                int mi = l[i], ma = l[i];
                for (int j = i; j < l.size(); ++j) {
                    mi = std::min(mi, l[j]);
                    ma = std::max(ma, l[j]);
                    if (mi + ma < j - i + 1) ans = std::max(ans, j - i + 1);
                }
            }
            return BuildData(Int, ans);
        };
    InfoEnd(Page22T1, "page22-t1")

    InfoStart(Page22T2, "page22-t2")
        type_config.int_max = 10;
        inp_type = _getLimitedListInt(type_config);
        auto mod_config = _getModConfigInfoPartition(env.get(), [](int x, int y){return x >= y;}, type_config);
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            auto l(_getListContent(inp_list[0]));
            int ans = 0;
            for (int i = 0; i < l.size() - ans; ++i) {
                int mi = l[i], ma = l[i];
                for (int j = i; j < l.size(); ++j) {
                    mi = std::min(mi, l[j]);
                    ma = std::max(ma, l[j]);
                    if (mi + ma > j - i + 1) ans = std::max(ans, j - i + 1);
                }
            }
            return BuildData(Int, ans);
        };
        env->setConst(solver::polygen::KMaxTermNumName, BuildData(Int, 5));
        env->setConst(solver::autolifter::KOccamExampleNumName, BuildData(Int, 10000));
    InfoEnd(Page22T2, "page22-t2")


#define RegisterName(task_name, func_name) if (name == task_name) return _getConfigInfo ## func_name()
    dsl::autolifter::LiftingConfigInfo _getConfigInfo(const std::string& name) {
        RegisterName("page9-t1", Page9T1);
        RegisterName("page9-t2", Page9T2);
        RegisterName("page10", Page10);
        RegisterName("page12", Page12);
        RegisterName("page20", Page20);
        RegisterName("page21", Page21);
        RegisterName("page22-t1", Page22T1);
        RegisterName("page22-t2", Page22T2);
        LOG(FATAL) << "AutoLifter: Unknown task " << name;
    }
}

LiftingTask * dsl::autolifter::track::getLSPLiftingTask(const std::string &name) {
    auto info = _getConfigInfo(name);
    return buildLiftingTask(info);
}