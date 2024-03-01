//
// Created by pro on 2021/12/29.
//

#ifndef ISTOOL_WITNESS_VALUE_H
#define ISTOOL_WITNESS_VALUE_H

#include "istool/basic/data.h"

class WitnessValue {
public:
    virtual std::string toString() const = 0;
    virtual bool isInclude(const Data& data) const = 0;
    virtual ~WitnessValue() = default;
};

typedef std::shared_ptr<WitnessValue> WitnessData;
typedef std::vector<WitnessData> WitnessTerm;
typedef std::vector<WitnessTerm> WitnessList;

class DirectWitnessValue: public WitnessValue {
public:
    Data d;
    DirectWitnessValue(const Data& _d);
    virtual std::string toString() const;
    virtual bool isInclude(const Data& data) const;
    virtual ~DirectWitnessValue() = default;
};

#define BuildDirectWData(type, w) std::make_shared<DirectWitnessValue>(Data(std::make_shared<type ## Value>(w)))

class ListedWitnessValue: public WitnessValue {
public:
    DataList l;
    ListedWitnessValue(const DataList& _l);
    virtual std::string toString() const;
    virtual bool isInclude(const Data& data) const;
    virtual ~ListedWitnessValue() = default;
};

class TotalWitnessValue: public WitnessValue {
public:
    virtual std::string toString() const;
    virtual bool isInclude(const Data& data) const;
    virtual ~TotalWitnessValue() = default;
};

#endif //ISTOOL_WITNESS_VALUE_H
