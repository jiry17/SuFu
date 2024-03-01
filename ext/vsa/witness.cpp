//
// Created by pro on 2021/12/29.
//

#include "istool/ext/vsa/witness.h"
#include "istool/ext/vsa/vsa_extension.h"
#include "glog/logging.h"

namespace {
    bool getBoolValue(const WitnessData& d) {
        auto* dv = dynamic_cast<DirectWitnessValue*>(d.get());
        if (!dv) WitnessError(d);
        auto* bv = dynamic_cast<BoolValue*>(dv->d.get());
        if (!bv) WitnessError(d);
        return bv->w;
    }
}

WitnessList NotWitnessFunction::witness(const WitnessData &oup) {
    auto* v = oup.get();
    if (dynamic_cast<TotalWitnessValue*>(v)) return {{oup}};
    auto w = getBoolValue(oup);
    return {{BuildDirectWData(Bool, !w)}};
}

WitnessList AndWitnessFunction::witness(const WitnessData &oup) {
    auto* v = oup.get();
    if (dynamic_cast<TotalWitnessValue*>(v)) return {{oup, oup}};
    auto w = getBoolValue(oup);
    if (w) return {{oup, oup}};
    auto total = std::make_shared<TotalWitnessValue>();
    return {{oup, total}, {BuildDirectWData(Bool, true), oup}};
}

WitnessList OrWitnessFunction::witness(const WitnessData &oup) {
    auto* v = oup.get();
    if (dynamic_cast<TotalWitnessValue*>(v)) return {{oup, oup}};
    auto w = getBoolValue(oup);
    if (!w) return {{oup, oup}};
    auto total = std::make_shared<TotalWitnessValue>();
    return {{oup, total}, {BuildDirectWData(Bool, false), oup}};
}

WitnessList ImplyWitnessFunction::witness(const WitnessData &oup) {
    auto* v = oup.get();
    if (dynamic_cast<TotalWitnessValue*>(v)) return {{oup, oup}};
    auto w = getBoolValue(oup);
    if (!w) return {{BuildDirectWData(Bool, true), oup}};
    auto total = std::make_shared<TotalWitnessValue>();
    return {{BuildDirectWData(Bool, false), total}, {oup, oup}};
}

WitnessList DirectWitnessFunction::witness(const WitnessData &oup) {
    return {{oup}};
}

void ext::vsa::loadLogicWitness(VSAExtension *ext) {
    LoadWitness(ext, "and", And);
    LoadWitness(ext, "&&", And);
    LoadWitness(ext, "or", Or);
    LoadWitness(ext, "||", Or);
    LoadWitness(ext, "!", Not);
    LoadWitness(ext, "not", Not);
    LoadWitness(ext, "=>", Imply);
    LoadWitness(ext, "", Direct);
}