//
// Created by pro on 2021/12/30.
//

#include "istool/sygus/theory/witness/clia/clia_witness.h"
#include "istool/sygus/theory/witness/clia/clia_wit_value.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/vsa/vsa_extension.h"
#include "glog/logging.h"

const std::string theory::clia::KWitnessIntMaxName = "CLIA@WIntMax";
const std::string theory::clia::KWitnessIntMinName = "CLIA@WIntMin";

WitnessList IntPlusWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto range = theory::clia::extractRange(oup);
    int res_l = range.first, res_r = range.second;
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList res;
    for (int a = lim_l; a <= lim_r; ++a) {
        int l = std::max(lim_l, res_l - a), r = std::min(lim_r, res_r - a);
        if (l > r) continue;
        res.push_back({BuildDirectWData(Int, a), theory::clia::buildRange(l, r)});
    }
    return res;
}

WitnessList IntMinusWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto range = theory::clia::extractRange(oup);
    int res_l = range.first, res_r = range.second;
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList res;
    for (int a = lim_l; a <= lim_r; ++a) {
        int l = std::max(lim_l, a - res_r), r = std::min(lim_r, a - res_l);
        if (l > r) continue;
        res.push_back({BuildDirectWData(Int, a), theory::clia::buildRange(l, r)});
    }
    return res;
}

// todo: finish the following witness functions
#define UnfinishedWitness(name) LOG(FATAL) << #name "WitnessFunction is unfinished"
WitnessList IntTimesWitnessFunction::witness(const WitnessData &oup) {
    UnfinishedWitness(IntTimes);
}
WitnessList IntModWitnessFunction::witness(const WitnessData &oup) {
    UnfinishedWitness(IntMod);
}
WitnessList IntDivWitnessFunction::witness(const WitnessData &oup) {
    UnfinishedWitness(IntDiv);
}

namespace {
    bool getBoolValue(const WitnessData& d) {
        auto* dv = dynamic_cast<DirectWitnessValue*>(d.get());
        if (!dv) WitnessError(d);
        return dv->d.isTrue();
    }
}

WitnessList IntLeqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (res) {
        for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i, lim_r)});
        }
    } else for (int i = lim_l + 1; i <= lim_r; ++i) {
        wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i - 1)});
    }
    return wl;
}

WitnessList IntLqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (res) {
        for (int i = lim_l; i < lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i + 1, lim_r)});
        }
    } else for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i)});
        }
    return wl;
}

WitnessList IntGeqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (res) {
        for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i)});
        }
    } else for (int i = lim_l; i < lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i + 1, lim_r)});
        }
    return wl;
}

WitnessList IntGqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (res) {
        for (int i = lim_l + 1; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i - 1)});
        }
    } else for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i, lim_r)});
        }
    return wl;
}

// Consider only int
WitnessList IntEqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (res) {
        for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), BuildDirectWData(Int, i)});
        }
    } else {
        for (int i = lim_l; i <= lim_r; ++i) {
            if (i > lim_l) wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i - 1)});
            if (i < lim_r) wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i + 1, lim_r)});
        }
    }
    return wl;
}

WitnessList BoolEqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto res = getBoolValue(oup);
    auto t = BuildDirectWData(Bool, true), f = BuildDirectWData(Bool, false);
    if (res) return {{t, t}, {f, f}};
    return {{t, f}, {f, t}};
}

WitnessList IntNeqWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto* dv = dynamic_cast<DirectWitnessValue*>(oup.get());
    auto res = getBoolValue(oup);
    int lim_l = theory::clia::getIntValue(*int_min), lim_r = theory::clia::getIntValue(*int_max);
    WitnessList wl;
    if (!res) {
        for (int i = lim_l; i <= lim_r; ++i) {
            wl.push_back({BuildDirectWData(Int, i), BuildDirectWData(Int, i)});
        }
    } else {
        for (int i = lim_l; i <= lim_r; ++i) {
            if (i > lim_l) wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(lim_l, i - 1)});
            if (i < lim_r) wl.push_back({BuildDirectWData(Int, i), theory::clia::buildRange(i + 1, lim_r)});
        }
    }
    return wl;
}

WitnessList IteWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    auto total = std::make_shared<TotalWitnessValue>();
    return {{BuildDirectWData(Bool, true), oup, total}, {BuildDirectWData(Bool, false), total, oup}};
}

namespace {
    int KDefaultIntMin = -1;
    int KDefaultIntMax = 10;
}

#define LoadCLIAWitnessFunction(name, sem) ext->registerWitnessFunction(name, new sem ## WitnessFunction(int_min, int_max))

void theory::clia::loadWitnessFunction(Env *env) {
    auto* int_min = env->getConstRef(theory::clia::KWitnessIntMinName, BuildData(Int, KDefaultIntMin));
    auto* int_max = env->getConstRef(theory::clia::KWitnessIntMaxName, BuildData(Int, KDefaultIntMax));
    auto* ext = ext::vsa::getExtension(env);
    LoadCLIAWitnessFunction("+", IntPlus); LoadCLIAWitnessFunction("-", IntMinus);
    LoadCLIAWitnessFunction("*", IntTimes); LoadCLIAWitnessFunction("div", IntDiv);
    LoadCLIAWitnessFunction("mod", IntMod); LoadCLIAWitnessFunction("<", IntLq);
    LoadCLIAWitnessFunction("<=", IntLeq); LoadCLIAWitnessFunction(">", IntGq);
    LoadCLIAWitnessFunction(">=", IntGeq);  LoadCLIAWitnessFunction("=", IntEq);
    LoadCLIAWitnessFunction("!=", IntNeq);
    ext->registerWitnessFunction("ite", new IteWitnessFunction());
    ext->registerWitnessFunction("=b", new BoolEqWitnessFunction());
}