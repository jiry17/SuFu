//
// Created by pro on 2022/2/1.
//

#include "istool/dsl/samplesy/samplesy_witness.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/witness/clia/clia_wit_value.h"
#include <unordered_set>

using namespace samplesy;

namespace {
    std::vector<std::string> _getStringList(DataList* data_list, bool is_allow_empty) {
        std::vector<std::string> res;
        for (auto& d: *data_list) {
            auto* sv = dynamic_cast<StringValue*>(d.get());
            if (sv) {
                if (sv->s.empty() && !is_allow_empty) continue;
                res.push_back(sv->s);
            }
        }
        return res;
    }

    std::string _replace(const std::string& x, const std::string& s, const std::string& t) {
        std::string res = x;
        for (auto i = res.find(s); i != std::string::npos; i = res.find(s, i + t.length())) {
            res.replace(i, s.length(), t);
        }
        return res;
    }

    std::string _getStringValue(const WitnessData& oup) {
        auto* dv = dynamic_cast<DirectWitnessValue*>(oup.get());
        assert(dv);
        return theory::string::getStringValue(dv->d);
    }
}

StringReplaceAllWitnessFunction::StringReplaceAllWitnessFunction(DataList *_const_list, DataList *_inp_list):
    const_list(_const_list), inp_list(_inp_list) {
}
WitnessList StringReplaceAllWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) {
        return {{oup, oup, oup}};
    }
    auto inp_str_list = _getStringList(inp_list, true);
    auto cons_str_list = _getStringList(const_list, false);
    auto oup_s = _getStringValue(oup);
    WitnessList res;
    std::unordered_set<std::string> inp_pool;
    for (auto& param: inp_str_list) {
        for (int i = 0; i < param.length(); ++i) {
            std::string sub;
            for (int j = i; j < param.length(); ++j) {
                sub += param[j];
                inp_pool.insert(sub);
            }
        }
    }

    for (const auto& inp: inp_pool) {
        for (const auto &s: cons_str_list) {
            for (const auto &t: cons_str_list) {
                if (s == t) continue;
                if (_replace(inp, s, t) == oup_s) {
                    res.push_back(
                            {BuildDirectWData(String, inp), BuildDirectWData(String, s), BuildDirectWData(String, t)});
                }
            }
        }
    }
    return res;
}

namespace {
    std::string _delete(const std::string& inp, const std::string& s) {
        auto pos = inp.find(s);
        if (pos == std::string::npos) return inp;
        auto res = inp;
        res.replace(pos, s.length(), "");
        return res;
    }

    bool _isSubstr(const std::string& s, const std::string& t) {
        int now = 0;
        for (auto c: s) {
            while (now < t.length() && t[now] != c) ++now;
            if (now == t.length()) return false;
        }
        return true;
    }
}

StringDeleteWitnessFunction::StringDeleteWitnessFunction(DataList *_const_list, DataList *_inp_list):
    const_list(_const_list), inp_list(_inp_list) {
}
WitnessList StringDeleteWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) {
        return {{oup, oup}};
    }
    auto inp_str_list = _getStringList(inp_list, true);
    auto cons_str_list = _getStringList(const_list, true);
    auto oup_s = _getStringValue(oup);
    WitnessList res;
    for (auto& s: cons_str_list) {
        for (int i = 0; i <= oup_s.size(); ++i) {
            auto inp = oup_s; inp.replace(i, 0, s);
            bool is_valid = false;
            for (const auto& param: inp_str_list) {
                if (_isSubstr(inp, param) && _delete(inp, s) == oup_s) {
                    is_valid = true; break;
                }
            }
            if (is_valid) res.push_back({BuildDirectWData(String, inp), BuildDirectWData(String, s)});
        }
    }
    return res;
}

StringAbsSubstrWitnessFunction::StringAbsSubstrWitnessFunction(DataList *_inp_list): inp_list(_inp_list) {}
WitnessList StringAbsSubstrWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    auto param_values = _getStringList(inp_list, true);
    auto oup_s = _getStringValue(oup);
    WitnessList res;
    for (auto& param: param_values) {
        for (auto pos = param.find(oup_s); pos != std::string::npos; pos = param.find(oup_s, pos + 1)) {
            int l = pos, r = pos + oup_s.length();
            res.push_back({BuildDirectWData(String, param), BuildDirectWData(Int, l), BuildDirectWData(Int, r)});
        }
    }
    return res;
}

StringIndexOfWitnessFunction::StringIndexOfWitnessFunction(DataList *_inp_list): inp_list(_inp_list) {}
WitnessList StringIndexOfWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    WitnessList res;
    auto range = theory::clia::extractRange(oup);
    for (auto& para: _getStringList(inp_list, true)) {
        int n = para.length();
        for (int target_pos = range.first; target_pos <= range.second; ++target_pos) {
            std::string sub_string;
            for (int i = target_pos; i < para.length(); ++i) {
                sub_string += para[i];
                int pre_pos = 0;
                for (auto pos = para.find(sub_string); pos < target_pos;
                     pos = para.find(sub_string, pos + 1)) pre_pos = int(pos) + 1;
                res.push_back({BuildDirectWData(String, para), BuildDirectWData(String, sub_string),
                               theory::clia::buildRange(pre_pos, target_pos)});
            }
        }
    }
    return res;
}

IndexMoveWitnessFunction::IndexMoveWitnessFunction(Data *_int_max): int_max(_int_max) {}
WitnessList IndexMoveWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto range = theory::clia::extractRange(oup);
    int inf = theory::clia::getIntValue(*int_max);
    WitnessList res;
    for (int i = 0; i <= inf; ++i) {
        for (int j = -inf; j <= inf; ++j) {
            if (i + j >= range.first && i + j <= range.second) {
                res.push_back({BuildDirectWData(Int, i), BuildDirectWData(Int, j)});
            }
        }
    }
    return res;
}