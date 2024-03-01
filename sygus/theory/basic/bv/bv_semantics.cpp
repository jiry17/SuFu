//
// Created by pro on 2022/2/12.
//

#include "istool/sygus/theory/basic/bv/bv_semantics.h"
#include "istool/sygus/theory/basic/bv/bv_type.h"
#include "istool/sygus/theory/basic/bv/bv_value.h"
#include "istool/basic/bitset.h"
#include "glog/logging.h"

typedef unsigned long long ULL;

namespace {
    ULL _bv2ull(const Bitset& w) {
        if (w.size() > 64) {
            LOG(FATAL) << "The current implementation of BVTheory cannot support Bitset larger than 64, but get " << w.size();
        }
        ULL res = 0;
        for (int i = w.size(); i; --i) {
            res = (res << 1) + w[i - 1];
        }
        return res;
    }

    Bitset _ull2bv(ULL w, int size) {
        Bitset res;
        for (int i = 0; i < size; ++i) {
            res.append(w & 1); w >>= 1;
        }
        return res;
    }

#define TBV(size) (theory::bv::getTBitVector(size))
using theory::bv::getBitVectorValue;
}

BVNegSemantics::BVNegSemantics(int _size): size(_size), NormalSemantics("bvneg", TBV(_size), {TBV(_size)}) {}
Data BVNegSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    ULL x = _bv2ull(getBitVectorValue(inp_list[0]));
    x = -x;
    return BuildData(BitVector, _ull2bv(x, size));
}

BVNotSemantics::BVNotSemantics(int _size): size(_size), NormalSemantics("bvnot", TBV(_size), {TBV(_size)}) {}
Data BVNotSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto w = getBitVectorValue(inp_list[0]);
    return BuildData(BitVector, ~w);
}

BVAddSemantics::BVAddSemantics(int _size): size(_size), NormalSemantics("bvadd", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVAddSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = _bv2ull(getBitVectorValue(inp_list[0])), y = _bv2ull(getBitVectorValue(inp_list[1]));
    return BuildData(BitVector, _ull2bv(x + y, size));
}

BVSubSemantics::BVSubSemantics(int _size): size(_size), NormalSemantics("bvsub", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVSubSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = _bv2ull(getBitVectorValue(inp_list[0])), y = _bv2ull(getBitVectorValue(inp_list[1]));
    return BuildData(BitVector, _ull2bv(x - y, size));
}

BVAndSemantics::BVAndSemantics(int _size): size(_size), NormalSemantics("bvand", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVAndSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]), y = getBitVectorValue(inp_list[1]);
    return BuildData(BitVector, x & y);
}

BVOrSemantics::BVOrSemantics(int _size): size(_size), NormalSemantics("bvor", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVOrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]), y = getBitVectorValue(inp_list[1]);
    return BuildData(BitVector, x | y);
}

BVShlSemantics::BVShlSemantics(int _size): size(_size), NormalSemantics("bvshl", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVShlSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]); auto y = _bv2ull(getBitVectorValue(inp_list[1]));
    Bitset res(size, 0);
    if (y < size) {
        for (int i = y; i < size; ++i) {
            res.set(i, x[i - y]);
        }
    }
    return BuildData(BitVector, res);
}

BVLShrSemantics::BVLShrSemantics(int _size): size(_size), NormalSemantics("bvlshr", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVLShrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]); auto y = _bv2ull(getBitVectorValue(inp_list[1]));
    Bitset res(size, 0);
    if (y < size) {
        for (int i = y; i < size; ++i) {
            res.set(i - y, x[i]);
        }
    }
    return BuildData(BitVector, res);
}

BVAShrSemantics::BVAShrSemantics(int _size): size(_size), NormalSemantics("bvashr", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVAShrSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]); auto y = _bv2ull(getBitVectorValue(inp_list[1]));
    if (y > size) y = size;
    Bitset res(size, 0);
    for (int i = y; i < size; ++i) {
        res.set(i - y, x[i]);
    }
    auto sign = x[size - 1];
    for (int i = size - int(y); i < size; ++i) {
        res.set(i, sign);
    }
    return BuildData(BitVector, res);
}

BVXorSemantics::BVXorSemantics(int _size): size(_size), NormalSemantics("bvxor", TBV(_size), {TBV(_size), TBV(_size)}) {}
Data BVXorSemantics::run(DataList &&inp_list, ExecuteInfo *info) {
    auto x = getBitVectorValue(inp_list[0]), y = getBitVectorValue(inp_list[1]);
    return BuildData(BitVector, x ^ y);
}