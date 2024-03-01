//
// Created by pro on 2022/7/1.
//

#include "istool/selector/baseline/biased_bitvector_selector.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "glog/logging.h"
#include <iostream>

namespace {
    std::string _getFeature(const Data& d, int K) {
        auto* bv = dynamic_cast<BitVectorValue*>(d.get());
        if (!bv) return "";
        std::string res;
        for (int i = 0; i < K; ++i) res += std::to_string(bv->w[i]);
        return res;
    }

    std::string _getFeature(const DataList& inp, int K) {
        std::string res;
        for (auto& d: inp) res += _getFeature(d, K);
        return res;
    }
}

BiasedBitVectorSelector::BiasedBitVectorSelector(FiniteIOExampleSpace *_example_space, int _K):
    example_space(_example_space), K(_K) {
    for (int i = 0; i < example_space->example_space.size(); ++i) {
        io_example_list.push_back(example_space->getIOExample(example_space->example_space[i]));
    }
    for (auto& example: io_example_list) {
        auto feature = _getFeature(example.first, K);
        feature_list.push_back(feature);
    }
}

bool BiasedBitVectorSelector::verify(const FunctionContext &info, Example *counter_example) {
    int best_pos = -1, best_time = ++stamp;
    for (int i = 0; i < io_example_list.size(); ++i) {
        if (!example_space->satisfyExample(info, example_space->example_space[i])) {
            int now = time_stamp[feature_list[i]];
            if (now < best_time) {
                best_pos = i; best_time = now;
            }
        }
    }
    if (best_pos == -1) return true;
    if (counter_example) *counter_example = example_space->example_space[best_pos];
    time_stamp[feature_list[best_pos]] = stamp;
    return false;
}

namespace {
    void _collectAll(int pos, int K, int num, int bit_size, Z3Extension* ext, DataList& tmp, std::vector<DataList>& res) {
        if (pos == num) {
            res.push_back(tmp); return;
        }
        for (int i = 0; i < (1 << K); ++i) {
            Bitset x(bit_size, 0);
            for (int j = 0; j < K; ++j) if (i & (1 << j)) x.set(j, 1);
            tmp[pos] = BuildData(BitVector, x);
            _collectAll(pos + 1, K, num, bit_size, ext, tmp, res);
        }
    }

    std::vector<DataList> _collectAll(int K, int num, int bit_size, Z3Extension* ext) {
        std::vector<DataList> res;
        DataList tmp(num);
        _collectAll(0, K, num, bit_size, ext, tmp, res);
        return res;
    }
}

Z3BiasedBitVectorSelector::Z3BiasedBitVectorSelector(Z3ExampleSpace *example_space, int _K): Z3Verifier(example_space), stamp(0), K(_K) {
    auto param_type_list = example_space->type_list;
    for (const auto& type: param_type_list) {
        assert(dynamic_cast<TBitVector*>(type.get()));
    }
    int len = theory::bv::getBitVectorLength(example_space->env);
    auto case_list = _collectAll(K, param_type_list.size(), len, ext);
    Bitset mask(len, 0);
    for (int i = 0; i < K; ++i) mask.set(i, 1);
    auto mask_v = ext->buildConst(BuildData(BitVector, mask));
    auto param_list = getParamVector();

    for (auto& x: case_list) {
        z3::expr_vector v(ext->ctx);
        for (int i = 0; i < param_list.size(); ++i) {
            v.push_back((param_list[i] & mask_v) == ext->buildConst(x[i]));
        }
        cons_list.push_back(z3::mk_and(v));
        stamp_list.push_back(0);
    }
}

bool Z3BiasedBitVectorSelector::verify(const FunctionContext &info, Example *counter_example) {
    z3::solver s(ext->ctx);
    prepareZ3Solver(s, info);
    auto res = s.check();
    if (res == z3::unsat) return true;
    if (res != z3::sat) {
        LOG(FATAL) << "Z3 failed with " << res;
    }
    if (!counter_example) return false;
    std::vector<std::pair<int, int>> A;
    for (int i = 0; i < cons_list.size(); ++i) A.emplace_back(stamp_list[i], i);
    std::sort(A.begin(), A.end());
    for (auto item: A) {
        s.push(); s.add(cons_list[item.second]);
        res = s.check();
        if (res == z3::sat) {
            getExample(s.get_model(), counter_example);
            stamp_list[item.second] = ++stamp;
            return false;
        }
        s.pop();
    }
    assert(0);
}