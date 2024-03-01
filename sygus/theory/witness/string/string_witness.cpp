//
// Created by pro on 2021/12/31.
//

#include "istool/sygus/theory/witness/string/string_witness.h"
#include "istool/sygus/theory/witness/clia/clia_wit_value.h"
#include "istool/sygus/theory/witness/clia/clia_witness.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/ext/vsa/vsa_extension.h"
#include "glog/logging.h"
#include <iostream>

using theory::string::getStringValue;
using theory::clia::getIntValue;

namespace {
    std::string _getString(const WitnessData& d) {
        auto* dv = dynamic_cast<DirectWitnessValue*>(d.get());
        if (!dv) WitnessError(d);
        auto* sv = dynamic_cast<StringValue*>(dv->d.get());
        if (!sv) WitnessError(d);
        return sv->s;
    }

    void _printResult(const std::string& sem_name, const WitnessData& oup, const WitnessList& res) {
        std::cout << "witness for " << sem_name << " " << oup->toString() << std::endl;
        for (const auto& term: res) {
            for (const auto& d: term) std::cout << " " << d->toString();
            std::cout << std::endl;
        }
        int kk; std::cin >> kk;
    }
}

WitnessList StringCatWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto s = _getString(oup); int n = s.length();
    WitnessList res;
    for (int i = 0; i <= n; ++i) {
        auto prefix = s.substr(0, i), suffix = s.substr(i, n);
        res.push_back({BuildDirectWData(String, prefix), BuildDirectWData(String, suffix)});
    }
    return res;
}

namespace {
    bool _isValidReplace(std::string inp, const std::string& s, const std::string &t, const std::string& oup) {
        auto pos = inp.find(s);
        if (pos == std::string::npos) return inp == oup;
        return inp.replace(pos, s.length(), t) == oup;
    }

    void _collectAllValidReplace(const std::string& s, const std::string& t, const std::string& oup, WitnessList& res) {
        auto pos = oup.find(t);
        while (pos != std::string::npos) {
            auto inp = oup; inp.replace(pos, t.length(), s);
            if (_isValidReplace(inp, s, t, oup)) {
                res.push_back({BuildDirectWData(String, inp), BuildDirectWData(String, s), BuildDirectWData(String, t)});
            }
            pos = oup.find(t, pos + 1);
        }
    }

    std::vector<std::string> _getStringConstList(DataList* const_list) {
        std::vector<std::string> res;
        for (const auto& d: *const_list) {
            res.push_back(getStringValue(d));
        }
        return res;
    }

    bool _isSubsequenceOf(const std::string &s, const std::string &t) {
        int now = 0;
        for (int i = 0; i < t.length(); ++i) {
            if (s[now] == t[i]) ++now;
            if (now == s.length()) return true;
        }
        return false;
    }

    bool _isValidInsertion(const std::string& s, const std::vector<std::string>& inp_list) {
        for (const auto& inp: inp_list) {
            if (_isSubsequenceOf(s, inp)) return true;
        }
        return false;
    }
}

WitnessList StringReplaceWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    std::vector<std::string> s_list = _getStringConstList(const_list);
    std::vector<std::string> inp_list = _getStringConstList(input_list);
    auto oup_s = _getString(oup);
    WitnessList res;

    // both s and t are not empty
    for (const auto& s: s_list) {
        if (s.empty()) continue;
        for (const auto& t: s_list) {
            if (s == t || t.empty()) continue;
            _collectAllValidReplace(s, t, oup_s, res);
        }
    }
    // s = "" is duplicated with str.++, ignore
    // t = ""
    int n = oup_s.length();
    for (const auto& t: s_list) {
        for (int i = 0; i < oup_s.length(); ++i) {
            auto inp = oup_s.substr(0, i) + t + oup_s.substr(i, n);
            if (_isValidReplace(inp, "", t, oup_s) && _isValidInsertion(inp, inp_list)) {
                res.push_back({BuildDirectWData(String, inp), BuildDirectWData(String, ""), BuildDirectWData(String, t)});
            }
        }
    }
    return res;
}

namespace {
    std::vector<std::string> _catStringVector(const std::vector<std::string>& x, const std::vector<std::string>& y) {
        std::vector<std::string> res = x;
        for (const auto& s: y) res.push_back(s);
        return res;
    }
}

#define getFullStringList() (_catStringVector(_getStringConstList(const_list), _getStringConstList(input_list)))

WitnessList StringAtWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto oup_s = _getString(oup);
    if (oup_s.length() > 0) return {};
    auto s_list = getFullStringList();
    WitnessList res;
    for (auto& inp: s_list) {
        for (int i = 0; i < inp.length(); ++i) {
            if (inp[i] == oup_s[0]) {
                res.push_back({BuildDirectWData(String, inp), BuildDirectWData(Int, i)});
            }
        }
    }
    return res;
}

WitnessList IntToStrWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup}};
    auto oup_s = _getString(oup);
    if (oup_s.empty()) {
        int l = theory::clia::getIntValue(*int_min);
        if (l < 0) return {{theory::clia::buildRange(l, -1)}};
        return {};
    }
    if (oup_s.length() > 1 && oup_s[0] == '0') return {};
    int inf_value = getIntValue(*int_max);
    for (char c: oup_s) if (c < '0' || c > '9') return {};
    int res = 0;
    for (char c: oup_s) {
        long long ne = 10ll * res + int(c) - int('0');
        if (ne > inf_value) return {};
        res = int(ne);
    }
    return {{BuildDirectWData(Int, res)}};
}

StringSubStrWitnessFunction::StringSubStrWitnessFunction(DataList *_const_list, DataList* _input_list, Data *_int_min, Data *_int_max):
    const_list(_const_list), int_min(_int_min), int_max(_int_max), input_list(_input_list) {
}

void _collectAllValidSubstr(const std::string& s, const std::string& t, WitnessList& res, int l, int r) {
    if (s.length() < t.length()) return;
    if (t.empty()) {
        auto total = std::make_shared<TotalWitnessValue>();
        if (l <= -1) {
            res.push_back({BuildDirectWData(String, s), theory::clia::buildRange(l, -1), total});
        }
        if (l <= 0) {
            res.push_back({BuildDirectWData(String, s), total, theory::clia::buildRange(l, 0)});
        }
        if (s.length() <= r) {
            res.push_back({BuildDirectWData(String, s), theory::clia::buildRange(s.length(), r), total});
        }
    } else {
        int n = s.length(), m = t.length();
        for (auto pos = s.find(t); pos != std::string::npos; pos = s.find(t, pos + 1)) {
            if (pos != n - m) {
                res.push_back({BuildDirectWData(String, s), BuildDirectWData(Int, pos), BuildDirectWData(Int, m)});
            } else if (m <= r) {
                res.push_back({BuildDirectWData(String, s), BuildDirectWData(Int, pos), theory::clia::buildRange(m, r)});
            }
        }
    }
}

namespace {
    void _printWitnessList(const WitnessList& res) {
        for (auto& item: res) {
            for (auto& d: item) std::cout << " " << d->toString();
            std::cout << std::endl;
        }
    }
}

WitnessList StringSubStrWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    auto s_list = getFullStringList();
    auto oup_s = _getString(oup);
    auto l = getIntValue(*int_min), r = getIntValue(*int_max);
    WitnessList res;
    for (const auto& s: s_list) {
        _collectAllValidSubstr(s, oup_s, res, l, r);
    }
    return res;
}

namespace {
    bool _insideRange(const std::pair<int, int>& r, int x) {
        return r.first <= x && x <= r.second;
    }
}

WitnessList StringLenWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup}};
    auto s_list = _getStringConstList(input_list);
    WitnessList res;
    auto range = theory::clia::extractRange(oup);
    if (_insideRange(range, 0)) res.push_back({BuildDirectWData(String, "")});
    for (const auto& s: s_list) {
        int len = s.length();
        if (_insideRange(range, len)) res.push_back({BuildDirectWData(String, s)});
    }
    return res;
}

namespace {
    int _getLastOccur(const std::string& s, const std::string& t, int r) {
        auto pos = s.find(t);
        if (pos == std::string::npos || pos >= r) return -1;
        while (true) {
            auto ne = s.find(t, pos + 1);
            if (ne == std::string::npos || ne >= r) return int(pos);
            pos = ne;
        }
    }
}

StringIndexOfWitnessFunction::StringIndexOfWitnessFunction(DataList* _const_list, DataList* _input_list, Data* _int_min, Data* _int_max):
    const_list(_const_list), input_list(_input_list), int_min(_int_min), int_max(_int_max) {
}
WitnessList StringIndexOfWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup, oup}};
    auto range = theory::clia::extractRange(oup);
    int l = std::max(-1, range.first), r = range.second;
    int lim_l = getIntValue(*int_min), lim_r = getIntValue(*int_max);
    if (l > r) return {};
    WitnessList res;
    auto s_list = _getStringConstList(input_list);
    auto t_list = _getStringConstList(const_list);
    for (const auto& s: s_list) {
        if (l == -1) {
            for (const auto &t: t_list) {
                int pos = _getLastOccur(s, t, int(s.length()) + 1);
                if (pos == -1) {
                    res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t),
                                   std::make_shared<TotalWitnessValue>()});
                } else {
                    if (lim_l < 0) res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t),
                                                  theory::clia::buildRange(lim_l, -1)});
                    if (pos < lim_r) res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t),
                                                    theory::clia::buildRange(pos + 1, lim_r)});
                }
            }
        }
        for (int pos = std::max(0, l); pos <= std::min(r, int(s.length())); ++pos) {
            std::string now;
            for (int i = pos; i < s.length(); ++i) {
                now += s[i];
                auto last_occur = _getLastOccur(s, now, pos);
                last_occur = last_occur == -1 ? 0 : last_occur + 1;
                if (last_occur <= pos) {
                    res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, now),
                                   theory::clia::buildRange(last_occur, pos)});
                }
            }
        }
    }
    return res;
}

namespace {
    bool _getBool(const WitnessData& oup) {
        auto* dv = dynamic_cast<DirectWitnessValue*>(oup.get());
        if (!dv) WitnessError(oup);
        auto bv = dynamic_cast<BoolValue*>(dv->d.get());
        if (!bv) WitnessError(oup);
        return bv->w;
    }
}

WitnessList StringPrefixOfWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto s_list = getFullStringList();
    bool target = _getBool(oup);
    WitnessList res;
    for (const auto& s: s_list) {
        for (const auto& t: s_list) {
            if (target == (s.length() <= t.length() && t.substr(0, s.length()) == s)) {
                res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t)});
            }
        }
    }
    return res;
}

WitnessList StringSuffixOfWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto s_list = getFullStringList();
    bool target = _getBool(oup);
    WitnessList res;
    for (const auto& s: s_list) {
        int n = s.length();
        for (const auto& t: s_list) {
            int m = t.length();
            if (target == (n <= m && t.substr(m - n, n) == s)) {
                res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t)});
            }
        }
    }
    return res;
}

WitnessList StringContainsWitnessFunction::witness(const WitnessData& oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup, oup}};
    auto s_list = getFullStringList();
    auto target = _getBool(oup);
    WitnessList res;
    for (const auto& s: s_list) {
        for (const auto& t: s_list) {
            if (target == (t.length() <= s.length() && s.find(t) != std::string::npos)) {
                res.push_back({BuildDirectWData(String, s), BuildDirectWData(String, t)});
            }
        }
    }
    return res;
}

namespace {
    bool isPositiveInteger(const std::string& s) {
        if (s.empty()) return false;
        for (char c: s) if (c < '0' || c > '9') return false;
        return true;
    }
}

WitnessList StrToIntWitnessFunction::witness(const WitnessData &oup) {
    if (dynamic_cast<TotalWitnessValue*>(oup.get())) return {{oup}};
    auto range = theory::clia::extractRange(oup);
    int l = std::max(-1, range.first), r = range.second;
    WitnessList res;
    if (l == -1 && l <= r) {
        auto s_list = _getStringConstList(input_list);
        for (const auto& s: s_list) {
            if (!isPositiveInteger(s)) res.push_back({BuildDirectWData(String, s)});
        }
    } else for (int i = std::max(0, l); i <= r; ++i) {
        res.push_back({BuildDirectWData(String, std::to_string(i))});
    }
    return res;
}

const std::string theory::string::KStringConstList = "String@Consts";
const std::string theory::string::KStringInputList = "String@Inputs";

#define LoadStringWitness(ext, name, sem) ext->registerWitnessFunction(name, new sem ## WitnessFunction(const_list, input_list))

void theory::string::loadWitnessFunction(Env *env) {
    auto* int_min = env->getConstRef(theory::clia::KWitnessIntMinName);
    auto* int_max = env->getConstRef(theory::clia::KWitnessIntMaxName);
    auto* const_list = env->getConstListRef(theory::string::KStringConstList);
    auto* input_list = env->getConstListRef(theory::string::KStringInputList);
    auto* ext = ext::vsa::getExtension(env);

    LoadWitness(ext, "str.++", StringCat);
    LoadStringWitness(ext, "str.len", StringLen);
    LoadStringWitness(ext, "str.at", StringAt);
    ext->registerWitnessFunction("str.substr", new StringSubStrWitnessFunction(const_list, input_list, int_min, int_max));
    LoadStringWitness(ext, "str.prefixof", StringPrefixOf);
    LoadStringWitness(ext, "str.suffixof", StringSuffixOf);
    LoadStringWitness(ext, "str.contains", StringContains);
    ext->registerWitnessFunction("str.indexof", new StringIndexOfWitnessFunction(const_list, input_list, int_min, int_max));
    LoadStringWitness(ext, "str.replace", StringReplace);
    LoadStringWitness(ext, "str.to.int", StrToInt);
    ext->registerWitnessFunction("int.to.str", new IntToStrWitnessFunction(int_min, int_max));
}