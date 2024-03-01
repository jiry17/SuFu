//
// Created by pro on 2022/2/23.
//

#include "istool/dsl/autolifter/autolifter_dataset.h"
#include "istool/dsl/autolifter/autolifter_dsl.h"
#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/solver/polygen/polygen_term_solver.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/limited_type/limited_int.h"
#include "istool/ext/limited_type/limited_ds.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "glog/logging.h"

using namespace dsl::autolifter;
#define TVARA type::getTVarA()
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

    LiftingModConfigInfo _getModConfigInfoDaD(Env *env) {
        auto F = std::make_shared<TProduct>((TypeList) {TVARA, TVARA});
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList) {TLISTINT, TLISTINT}));
        auto m = std::make_shared<Program>(env->getSemantics("++"),
                                           (ProgramList) {_buildAccess(x, 0), _buildAccess(x, 1)});
        return {m, F};
    }

    ListValue *_getListValue(const Data &data) {
        auto *res = dynamic_cast<ListValue *>(data.get());
        assert(res);
        return res;
    }

    std::vector<int> _getListIntContent(const Data& data) {
        auto* x = _getListValue(data);
        std::vector<int> res(x->value.size());
        for (int i = 0; i < x->value.size(); ++i) res[i] = getIntValue(x->value[i]);
        return res;
    }

#define InfoStart(name, str_name) \
    LiftingConfigInfo _getConfigInfo ## name() { \
        auto env = std::make_shared<Env>(); \
        dsl::autolifter::prepareEnv(env.get()); \
        _TypeConfig type_config; \
        auto dad_mod_config = _getModConfigInfoDaD(env.get()); \
        auto inp_type = _getLimitedListInt(type_config);

#define InfoEnd(name, str_name) \
        return {_buildListProgram(ps, str_name), inp_type, env, {dad_mod_config}}; \
    }

    PProgram _buildListProgram(const FullSemanticsFunction &ps, const std::string &name) {
        auto sem = std::make_shared<TypedAnonymousSemantics>(ps, (TypeList) {TLISTINT}, theory::clia::getTInt(), name);
        auto p = std::make_shared<Program>(sem, (ProgramList) {program::buildParam(0, TLISTINT)});
        return p;
    }

    InfoStart(Sum, "sum")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            int res = 0;
            for (int i = 0; i < x->value.size(); ++i) {
                res += getIntValue(x->value[i]);
            }
            return BuildData(Int, res);
        };
    InfoEnd(Sum, "sum")

    InfoStart(Min, "min")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]);
            for (int i = 0; i < x->value.size(); ++i) {
                res = std::min(res, getIntValue(x->value[i]));
            }
            return BuildData(Int, res);
        };
    InfoEnd(Min, "min")

    InfoStart(Max, "max")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]);
            for (int i = 0; i < x->value.size(); ++i) {
                res = std::max(res, getIntValue(x->value[i]));
            }
            return BuildData(Int, res);
        };
    InfoEnd(Max, "max")

    InfoStart(Length, "length")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            return BuildData(Int, x->value.size());
        };
    InfoEnd(Length, "length")

    InfoStart(SecondMin, "2nd-min")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.size() < 2) throw SemanticsError();
            int min = getIntValue(x->value[0]);
            int second_min = getIntValue(x->value[1]);
            if (min > second_min) std::swap(min, second_min);
            for (int i = 2; i < x->value.size(); ++i) {
                int now = getIntValue(x->value[i]);
                if (now < min) second_min = min, min = now;
                else second_min = std::min(second_min, now);
            }
            return BuildData(Int, second_min);
        };
    InfoEnd(SecondMin, "2nd-min")

    InfoStart(ThirdMin, "3rd-min")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.size() < 3) throw SemanticsError();
            std::vector<int> value_list;
            for (auto &d: x->value) {
                value_list.push_back(getIntValue(d));
            }
            std::sort(value_list.begin(), value_list.end());
            return BuildData(Int, value_list[2]);
        };
        env->setConst(solver::polygen::KMaxTermNumName, BuildData(Int, 6));
    InfoEnd(ThirdMin, "3rd-min")

    InfoStart(MaxPrefixSum, "mps")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]), sum = res;
            for (int i = 1; i < x->value.size(); ++i) {
                sum += getIntValue(x->value[i]);
                res = std::max(res, sum);
            }
            return BuildData(Int, res);
        };
    InfoEnd(MaxPrefixSum, "mps")

    InfoStart(MaxSuffixSum, "mts")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[1]);
            if (x->value.empty()) throw SemanticsError();
            int last = int(x->value.size()) - 1;
            int res = getIntValue(x->value[last]), sum = res;
            for (int i = last - 1; i >= 0; --i) {
                sum += getIntValue(x->value[i]);
                res = std::max(res, sum);
            }
            return BuildData(Int, res);
        };
    InfoEnd(MaxSuffixSum, "mts")

    InfoStart(MaxSegmentSum, "mss")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]), mts = res;
            for (int i = 1; i < x->value.size(); ++i) {
                mts = std::max(0, mts) + getIntValue(x->value[i]);
                res = std::max(res, mts);
            }
            return BuildData(Int, res);
        };
    InfoEnd(MaxSegmentSum, "mss")

    InfoStart(MaxPrefixSumPos, "mps-pos")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]), sum = res, pos = 0;
            for (int i = 1; i < x->value.size(); ++i) {
                sum += getIntValue(x->value[i]);
                if (sum > res) res = sum, pos = i;
            }
            return BuildData(Int, pos);
        };
        env->setConst(solver::autolifter::KComposedNumName, BuildData(Int, 3));
    InfoEnd(MaxPrefixSumPos, "mps-pos")

    InfoStart(MaxSuffixSumPos, "mts-pos")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int last = int(x->value.size()) - 1;
            int sum = getIntValue(x->value[last]), res = sum, pos = last;
            for (int i = last - 1; i >= 0; --i) {
                sum += getIntValue(x->value[i]);
                if (sum > res) res = sum, pos = i;
            }
            return BuildData(Int, pos);
        };
        env->setConst(solver::autolifter::KComposedNumName, BuildData(Int, 3));
    InfoEnd(MaxSuffixSumPos, "mts-pos")

    InfoStart(IsSorted, "is-sorted")
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) -> Data {
            auto *x = _getListValue(inp_list[0]);
            for (int i = 1; i < x->value.size(); ++i) {
                if (x->value[i] < x->value[i - 1]) return BuildData(Int, 0);
            }
            return BuildData(Int, 1);
        };
    InfoEnd(IsSorted, "is-sorted")

    InfoStart(Atoi, "atoi")
        const int pow_lim = 8;
        type_config.int_min = 0; type_config.int_max = 9; type_config.length_max = pow_lim / 2;
        inp_type = _getLimitedListInt(type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo *info) -> Data {
            int res = 0;
            auto *x = _getListValue(inp_list[0]);
            for (const auto &d: x->value) {
                res = res * 10 + getIntValue(d);
            }
            return BuildData(Int, res);
        };
        auto pow10 = [=](DataList &&inp_list, ExecuteInfo *info) -> Data {
            int n = getIntValue(inp_list[0]);
            if (n >= pow_lim || n < 0) throw SemanticsError();
            int res = 1;
            for (int i = 0; i < n; ++i) res *= 10;
            return BuildData(Int, res);
        };
        auto pow_sem = std::make_shared<TypedAnonymousSemantics>(pow10, (TypeList){theory::clia::getTInt()}, theory::clia::getTInt(), "pow10");
        dad_mod_config.extra_semantics.push_back(pow_sem);
        dad_mod_config.extra_semantics.push_back(env->getSemantics("*"));
    InfoEnd(Atoi, "atoi")

#define Info01Start(name, str_name) \
    LiftingConfigInfo _getConfigInfo ## name() { \
        auto env = std::make_shared<Env>(); \
        dsl::autolifter::prepareEnv(env.get()); \
        _TypeConfig type_config; \
        type_config.int_min = 0; type_config.int_max = 1; type_config.length_max = 15; \
        auto dad_mod_config = _getModConfigInfoDaD(env.get()); \
        auto inp_type = _getLimitedListInt(type_config);

    Info01Start(DropWhile, "dropwhile")
        auto ps = [](DataList&& inp, ExecuteInfo* info) {
            auto x = _getListValue(inp[0]);
            for (int i = 0; i < x->value.size(); ++i) {
                if (getIntValue(x->value[i])) return BuildData(Int, i);
            }
            return BuildData(Int, int(x->value.size()));
        };
    InfoEnd(DropWhile, "dropwhile")

    InfoStart(Balanced, "balanced")
        type_config.int_min = -1; type_config.int_max = 1;
        inp_type = _getLimitedListInt(type_config);
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            int cnt = 0;
            auto* x = _getListValue(inp_list[0]);
            for (const auto& d: x->value) {
                cnt += getIntValue(d);
                if (cnt < 0) return BuildData(Int, 0);
            }
            return BuildData(Int, 1);
        };
    InfoEnd(Balanced, "balanced")

    Info01Start(0Start1Star, "0*1*")
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            int an = 1, bn = 1;
            auto list = _getListIntContent(inp_list[0]);
            for (auto v: list) {
                an = v && an;
                bn = ((!v) || an) && bn;
            }
            return BuildData(Int, bn);
        };
    InfoEnd(0Star1Star, "0*1*")

    Info01Start(Cnt1s, "cnt1s")
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            int i = 0, f = 0, cnt = 0;
            auto a = _getListIntContent(inp_list[0]); int n = a.size();
            for( i = 0; i < n; i++) {
                cnt += (a[i] && !f) ? 1 : 0;
                f = a[i];
            }
            return BuildData(Int, cnt);
        };
    InfoEnd(Cnt1s, "cnt1s")

    InfoStart(LineSight, "line-sight")
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) {
            int ma = 0, vis = 1;
            auto list = _getListIntContent(inp_list[0]);
            for (auto v: list) {
                vis = (ma <= v);
                ma = std::max(ma, v);
            }
            return BuildData(Int, vis);
        };
    InfoEnd(LineSight, "line-sight")

    Info01Start(Max1s, "max1s")
        auto ps = [](DataList &&inp_list, ExecuteInfo* info) -> Data {
            auto *x = _getListValue(inp_list[0]);
            int pre = 0, result = 0;
            for (int i = 0; i <= x->value.size(); ++i) {
                if (i == x->value.size() || !getIntValue(x->value[i])) {
                    result = std::max(result, i - pre);
                    pre = i + 1;
                }
            }
            return BuildData(Int, result);
        };
    InfoEnd(Max1s, "max1s")

    Info01Start(0After1, "0after1")
        auto ps = [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            bool seen1 = false, res = false;
            auto l = _getListIntContent(inp_list[0]);
            for (int i = 0; i < l.size(); ++i) {
                if (seen1 && !(l[i])) res = true;
                seen1 = seen1 || l[i];
            }
            return BuildData(Int, res);
        };
    InfoEnd(0After1, "0after1")

    InfoStart(MaxPrefixProduct, "mpp")
        type_config.int_min = -4; type_config.int_max = 4; type_config.length_max = 5;
        inp_type = _getLimitedListInt(type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo* global_info) {
            auto list(_getListIntContent(inp_list[0]));
            if (list.empty()) throw SemanticsError();
            int current = list[0], ans = current;
            for (int i = 1; i < list.size(); ++i) {
                current *= list[i];
                ans = std::max(ans, current);
            }
            return BuildData(Int, ans);
        };
        dad_mod_config.extra_semantics.push_back(env->getSemantics("*"));
        env->setConst(solver::autolifter::KOccamExampleNumName, BuildData(Int, 2000));
    InfoEnd(MaxPrefixProduct, "mpp")

    InfoStart(MaxSuffixProduct, "mtp")
        type_config.int_min = -4; type_config.int_max = 4; type_config.length_max = 5;
        inp_type = _getLimitedListInt(type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo* global_info) {
            auto list(_getListIntContent(inp_list[0]));
            if (list.empty()) throw SemanticsError();
            std::reverse(list.begin(), list.end());
            int current = list[0], ans = current;
            for (int i = 1; i < list.size(); ++i) {
                current *= list[i];
                ans = std::max(ans, current);
            }
            return BuildData(Int, ans);
        };
        dad_mod_config.extra_semantics.push_back(env->getSemantics("*"));
    InfoEnd(MaxSuffixProduct, "mtp")

    InfoStart(MaxSegmentProduct, "msp")
        type_config.int_min = -4; type_config.int_max = 4; type_config.length_max = 5;
        inp_type = _getLimitedListInt(type_config);
        auto ps = [](DataList &&inp_list, ExecuteInfo* global_info) {
            auto list(_getListIntContent(inp_list[0]));
            if (list.empty()) throw SemanticsError();
            int res = list[0], min = res, max = res;
            for (int i = 1; i < list.size(); ++i) {
                min *= list[i]; max *= list[i];
                if (min > max) std::swap(min, max);
                min = std::min(min, list[i]); max = std::max(max, list[i]);
                res = std::max(res, max);
            }
            return BuildData(Int, res);
        };
        dad_mod_config.extra_semantics.push_back(env->getSemantics("*"));
        env->setConst(solver::autolifter::KOccamExampleNumName, BuildData(Int, 10000));
        env->setConst(solver::autolifter::KComposedNumName, BuildData(Int, 4));
    InfoEnd(MaxSegmentProduct, "msp")


#define RegisterName(task_name, func_name) if (name == task_name) return _getConfigInfo ## func_name()
    dsl::autolifter::LiftingConfigInfo _getConfigInfo(const std::string& name) {
        RegisterName("sum", Sum);
        RegisterName("min", Min);
        RegisterName("max", Max);
        RegisterName("length", Length);
        RegisterName("2nd-min", SecondMin);
        RegisterName("3rd-min", ThirdMin);
        RegisterName("mps", MaxPrefixSum);
        RegisterName("mts", MaxSuffixSum);
        RegisterName("mss", MaxSegmentSum);
        RegisterName("mps-pos", MaxPrefixSumPos);
        RegisterName("mts-pos", MaxSuffixSumPos);
        RegisterName("is-sorted", IsSorted);
        RegisterName("atoi", Atoi);
        RegisterName("dropwhile", DropWhile);
        RegisterName("balanced", Balanced);
        RegisterName("0*1*", 0Start1Star);
        RegisterName("cnt1s", Cnt1s);
        RegisterName("0after1", 0After1);
        RegisterName("line-sight", LineSight);
        RegisterName("max1s", Max1s);\
        RegisterName("mpp", MaxPrefixProduct);
        RegisterName("mtp", MaxSuffixProduct);
        RegisterName("msp", MaxSegmentProduct);
        LOG(FATAL) << "AutoLifter: Unknown task " << name;
    }
}

LiftingTask * dsl::autolifter::track::getDaDLiftingTask(const std::string &name) {
    dsl::autolifter::LiftingConfigInfo info = _getConfigInfo(name);
    return buildLiftingTask(info);
}