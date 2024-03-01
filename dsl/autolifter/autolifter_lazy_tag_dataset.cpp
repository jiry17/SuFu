//
// Created by pro on 2022/3/7.
//

#include "istool/dsl/autolifter/autolifter_dataset.h"
#include "istool/dsl/autolifter/autolifter_dsl.h"
#include "istool/solver/autolifter/composed_sf_solver.h"
#include "istool/ext/deepcoder/data_type.h"
#include "istool/ext/deepcoder/data_value.h"
#include "istool/ext/deepcoder/deepcoder_semantics.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/limited_type/limited_int.h"
#include "istool/ext/limited_type/limited_ds.h"
#include "istool/ext/deepcoder/anonymous_function.h"
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

    PType _getLimitedInt(const _TypeConfig& config) {
        return std::make_shared<LimitedTInt>(config.int_min, config.int_max);
    }
    PType _getLimitedListInt(const _TypeConfig& config) {
        auto content = _getLimitedInt(config);
        return std::make_shared<LimitedTList>(config.length_max, content);
    }

    PProgram _buildAccess(const PProgram& base, int id) {
        auto sem = std::make_shared<AccessSemantics>(id);
        return std::make_shared<Program>(sem, (ProgramList){base});
    }

    LiftingModConfigInfo _getModConfigInfoDaD(Env* env) {
        auto F = std::make_shared<TProduct>((TypeList){TVARA, TVARA});
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList){TLISTINT, TLISTINT}));
        auto m = std::make_shared<Program>(env->getSemantics("++"), (ProgramList){_buildAccess(x, 0), _buildAccess(x, 1)});
        return {m, F};
    }

    LiftingModConfigInfo _getModConfigInfoTag(Env* env, const PType& tag_type, const PSemantics& mod_sem) {
        auto F = std::make_shared<TProduct>((TypeList){tag_type, TVARA});
        auto x = program::buildParam(0, std::make_shared<TProduct>((TypeList){tag_type, TLISTINT}));
        auto m = std::make_shared<Program>(mod_sem, (ProgramList){_buildAccess(x, 0), _buildAccess(x, 1)});
        return{m, F};
    }

    ListValue* _getListValue(const Data& data) {
        auto* res = dynamic_cast<ListValue*>(data.get());
        assert(res);
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
        return {_buildListProgram(ps, str_name), inp_type, env, {lazy_tag_config, dad_mod_config}}; \
    }

    PProgram _buildListProgram(const FullSemanticsFunction& ps, const std::string& name) {
        auto sem = std::make_shared<TypedAnonymousSemantics>(ps, (TypeList){TLISTINT}, theory::clia::getTInt(), name);
        auto p = std::make_shared<Program>(sem, (ProgramList){program::buildParam(0, TLISTINT)});
        return p;
    }

    FullSemanticsFunction _getSumSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* x = _getListValue(inp_list[0]);
            int sum = 0;
            for (auto& d: x->value) sum += getIntValue(d);
            return BuildData(Int, sum);
        };
    }

    FullSemanticsFunction _getSqrSumSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* x = _getListValue(inp_list[0]);
            int res = 0;
            for (auto& d: x->value) {
                int k = getIntValue(d); res += k * k;
            }
            return BuildData(Int, res);
        };
    }

    FullSemanticsFunction _getMpsSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int res = getIntValue(x->value[0]), sum = res;
            for (int i = 1; i < x->value.size(); ++i) {
                sum += getIntValue(x->value[i]);
                if (sum > res) res = sum;
            }
            return BuildData(Int, res);
        };
    }

    FullSemanticsFunction _getMtsSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int last = int(x->value.size()) - 1;
            int res = getIntValue(x->value[last]), sum = res;
            for (int i = last - 1; i >= 0; --i) {
                sum += getIntValue(x->value[i]);
                if (sum > res) res = sum;
            }
            return BuildData(Int, res);
        };
    }

    FullSemanticsFunction _getMssSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* x = _getListValue(inp_list[0]);
            if (x->value.empty()) throw SemanticsError();
            int mts = getIntValue(x->value[0]), res = mts;
            for (int i = 1; i < x->value.size(); ++i) {
                mts = std::max(mts, 0) + getIntValue(x->value[i]);
                if (mts > res) res = mts;
            }
            return BuildData(Int, res);
        };
    }

    FullSemanticsFunction _getSecondMinSemanticsFunction() {
        return [](DataList &&inp_list, ExecuteInfo *info) -> Data {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.size() < 2) throw SemanticsError();
            std::vector<int> l(x->value.size());
            for (int i = 0; i < x->value.size(); ++i) l[i] = getIntValue(x->value[i]);
            std::sort(l.begin(), l.end());
            return BuildData(Int, l[1]);
        };
    }

    FullSemanticsFunction _getThirdMinSemanticsFunction() {
        return [](DataList &&inp_list, ExecuteInfo *info) -> Data {
            auto *x = _getListValue(inp_list[0]);
            if (x->value.size() < 3) throw SemanticsError();
            std::vector<int> l(x->value.size());
            for (int i = 0; i < x->value.size(); ++i) l[i] = getIntValue(x->value[i]);
            std::sort(l.begin(), l.end());
            return BuildData(Int, l[2]);
        };
    }

    FullSemanticsFunction _getMax1sSemanticsFunction() {
        return [](DataList &&inp_list, ExecuteInfo* info) -> Data {
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
    }

    FullSemanticsFunction _getMax1sPosSemanticsFunction() {
        return [](DataList &&inp_list, ExecuteInfo* info) -> Data {
            auto *x = _getListValue(inp_list[0]);
            int pre = 0, result = 0, pos = 0;
            for (int i = 0; i <= x->value.size(); ++i) {
                if (i == x->value.size() || !getIntValue(x->value[i])) {
                    if (i - pre > result) {
                        result = i - pre;
                        pos = pre;
                    }
                    pre = i + 1;
                }
            }
            return BuildData(Int, pos);
        };
    }


    FullSemanticsFunction _getPlusSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data{
            int x = getIntValue(inp_list[0]);
            auto* y = _getListValue(inp_list[1]);
            DataList res;
            for (auto& d: y->value) {
                res.push_back(BuildData(Int, getIntValue(d) + x));
            }
            return BuildData(List, res);
        };
    }

    FullSemanticsFunction _getNegAllSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) ->Data {
            if (!inp_list[0].isTrue()) return inp_list[1];
            auto* x = _getListValue(inp_list[1]);
            DataList res;
            for (auto& d: x->value) res.push_back(BuildData(Int, -getIntValue(d)));
            return BuildData(List, res);
        };
    }

    FullSemanticsFunction _getCoverSemanticsFunction() {
        return [](DataList&& inp_list, ExecuteInfo* info) -> Data {
            auto* y = _getListValue(inp_list[1]);
            DataList res(y->value.size(), inp_list[0]);
            return BuildData(List, res);
        };
    }

    InfoStart(SumPlus, "sum@+")
        auto plus_f = _getPlusSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(plus_f, "plus"));
        lazy_tag_config.extra_semantics.push_back(env->getSemantics("*"));
        auto ps = _getSumSemanticsFunction();
    InfoEnd(SumPlus, "sum@+")

    InfoStart(SumNeg, "sum@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getSumSemanticsFunction();
    InfoEnd(SumNeg, "sum@neg")

    InfoStart(SqrSumPlus, "sqrsum@+")
        auto plus_f = _getPlusSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(plus_f, "plus"));
        lazy_tag_config.extra_semantics.push_back(env->getSemantics("*"));
        env->setConst(theory::clia::KINFName, BuildData(Int, 2000));
        auto ps = _getSqrSumSemanticsFunction();
    InfoEnd(SqrSumPlus, "sqrsum@+")

    InfoStart(SqrSumNeg, "sqrsum@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getSqrSumSemanticsFunction();
    InfoEnd(SqrSumNeg, "sqrsum@neg")

    InfoStart(MpsNeg, "mps@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getMpsSemanticsFunction();
    InfoEnd(MpsNeg, "mps@neg")

    InfoStart(MpsCover, "mps@cover")
        auto cover_f = _getCoverSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(cover_f, "cover"));
        lazy_tag_config.extra_semantics.push_back(env->getSemantics("*"));
        auto ps = _getMpsSemanticsFunction();
    InfoEnd(MpsCover, "mps@cover")

    InfoStart(MtsNeg, "mts@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getMtsSemanticsFunction();
    InfoEnd(MtsNeg, "mts@neg")

    InfoStart(MtsCover, "mts@cover")
        auto cover_f = _getCoverSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(cover_f, "cover"));
        lazy_tag_config.extra_semantics.push_back(env->getSemantics("*"));
        auto ps = _getMtsSemanticsFunction();
    InfoEnd(MtsCover, "mts@cover")

    InfoStart(MssNeg, "mss@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getMssSemanticsFunction();
        auto mss_semantics = std::make_shared<TypedAnonymousSemantics>(ps, (TypeList){TLISTINT}, TINT, "mss");
        return {_buildListProgram(ps, "mss@neg"), inp_type, env, {lazy_tag_config, dad_mod_config}, {mss_semantics}};
    }//InfoEnd(MssNeg, "mss@neg")

    InfoStart(MssCover, "mss@cover")
        auto cover_f = _getCoverSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(cover_f, "cover"));
        lazy_tag_config.extra_semantics.push_back(env->getSemantics("*"));
        auto ps = _getMssSemanticsFunction();
    InfoEnd(MssCover, "mss@cover")

    InfoStart(SecondMinNeg, "2nd-min@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getSecondMinSemanticsFunction();
    InfoEnd(SecondMinNeg, "2nd-min@neg")

    InfoStart(ThirdMinNeg, "3rd-min@neg")
        auto neg_f = _getNegAllSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), type::getTBool(), std::make_shared<AnonymousSemantics>(neg_f, "neg-all"));
        auto ps = _getThirdMinSemanticsFunction();
        env->setConst(solver::polygen::KMaxTermNumName, BuildData(Int, 6));
    InfoEnd(ThirdMinNeg, "3rd-min@neg")

    InfoStart(Max1sCover, "max1s@cover")
        type_config.int_min = 0; type_config.int_max = 1; type_config.length_max = 15;
        inp_type = _getLimitedListInt(type_config);
        auto cover_f = _getCoverSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(cover_f, "cover"));
        auto ps = _getMax1sSemanticsFunction();
    InfoEnd(Max1sCover, "max1s@cover")

    InfoStart(Max1sPosCover, "max1s-pos@cover")
        type_config.int_min = 0; type_config.int_max = 1; type_config.length_max = 15;
        inp_type = _getLimitedListInt(type_config);
        auto cover_f = _getCoverSemanticsFunction();
        auto lazy_tag_config = _getModConfigInfoTag(env.get(), _getLimitedInt(type_config), std::make_shared<AnonymousSemantics>(cover_f, "cover"));
        auto ps = _getMax1sPosSemanticsFunction();
        auto max1s_semantics = std::make_shared<TypedAnonymousSemantics>(_getMax1sSemanticsFunction(), (TypeList){TLISTINT}, TINT, "max1s");
        env->setConst(solver::autolifter::KComposedNumName, BuildData(Int, 4));
        env->setConst(solver::autolifter::KOccamExampleNumName, BuildData(Int, 10000));
        return {_buildListProgram(ps, "max1s-pos@cover"), inp_type, env, {lazy_tag_config, dad_mod_config}, {max1s_semantics}};
    }//InfoEnd(Max1sCover, "max1s@cover")

#define RegisterName(task_name, func_name) if (name == task_name) return _getConfigInfo ## func_name()
    dsl::autolifter::LiftingConfigInfo _getConfigInfo(const std::string& name) {
        RegisterName("sum@+", SumPlus);
        RegisterName("sum@neg", SumNeg);
        RegisterName("sqrsum@+", SqrSumPlus);
        RegisterName("sqrsum@neg", SqrSumNeg);
        RegisterName("mps@neg", MpsNeg);
        RegisterName("mps@cover", MpsCover);
        RegisterName("mts@neg", MtsNeg);
        RegisterName("mts@cover", MtsCover);
        RegisterName("mss@neg", MssNeg);
        RegisterName("mss@cover", MssCover);
        RegisterName("2nd-min@neg", SecondMinNeg);
        RegisterName("3rd-min@neg", ThirdMinNeg);
        RegisterName("max1s@cover", Max1sCover);
        RegisterName("max1s-pos@cover", Max1sPosCover);
        LOG(FATAL) << "AutoLifter: Unknown task " << name;
    }
}

LiftingTask * dsl::autolifter::track::getLazyTagLiftingTask(const std::string &name) {
    auto info = _getConfigInfo(name);
    return buildLiftingTask(info);
}