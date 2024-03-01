//
// Created by pro on 2021/11/30.
//

#ifndef ISTOOL_DATA_H
#define ISTOOL_DATA_H

#include <memory>
#include <vector>
#include "value.h"

class Data {
public:
    PValue value;

    Data();
    Data(PValue&& _value);

    std::string toString() const;
    bool operator == (const Data& d) const;
    bool operator <= (const Data& d) const;
    bool operator < (const Data& d) const;
    Value* get() const;
    bool isTrue() const;
    bool isNull() const;

    ~Data() = default;
};

#define BuildData(Type, w) Data(std::make_shared<Type ## Value>(w))

typedef std::vector<Data> DataList;
typedef std::vector<DataList> DataStorage;

namespace data {
    std::string dataList2String(const DataList& data_list);
    DataList concatDataList(const DataList& x, const DataList& y);
    DataStorage cartesianProduct(const DataStorage& separate_data);
}


#endif //ISTOOL_DATA_H
