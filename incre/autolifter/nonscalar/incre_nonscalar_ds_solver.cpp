//
// Created by pro on 2023/7/18.
//

#include "istool/incre/autolifter/incre_nonscalar_autolifter.h"
#include "glog/logging.h"

using namespace incre::autolifter;
using namespace incre;

bool comb::isMatchUsage(const Pattern& pattern, const Data& oup, const DataList& inp) {
    switch (pattern->getType()) {
        case PatternType::UNDER_SCORE: return true;
        case PatternType::VAR: {
            auto* pv = dynamic_cast<PtVar*>(pattern.get());
            auto id = std::stoi(pv->name); assert(id < inp.size());
            return oup == inp[id];
        }
        case PatternType::TUPLE: {
            auto* tt = dynamic_cast<PtTuple*>(pattern.get());
            auto* vt = dynamic_cast<VTuple*>(oup.get());
            assert(vt && tt->pattern_list.size() == vt->elements.size());
            for (int i = 0; i < tt->pattern_list.size(); ++i) {
                if (!isMatchUsage(tt->pattern_list[i], vt->elements[i], inp)) return false;
            }
            return true;
        }
        case PatternType::CONSTRUCTOR: {
            auto* vi = dynamic_cast<VInductive*>(oup.get());
            auto* tc = dynamic_cast<PtConstructor*>(pattern.get());
            if (vi->name != tc->name) return false;
            return isMatchUsage(tc->pattern, vi->content, inp);
        }
    }
}

namespace {
    Data _getDummyValue(const Ty &type) {
        if (type->getType() == TyType::INT) return BuildData(Int, 0);
        if (type->getType() == TyType::BOOL) return BuildData(Bool, false);
        if (type->getType() == TyType::UNIT) return Data(std::make_shared<VUnit>());
        LOG(FATAL) << "Unsupported type " << type->toString();
    }
}

Term comb::usage2Term(const Pattern &pattern, const TermList &term_list, const Ty& type) {
    switch (pattern->getType()) {
        case PatternType::UNDER_SCORE: {
            return std::make_shared<TmValue>(_getDummyValue(type));
        }
        case PatternType::VAR: {
            auto* pv = dynamic_cast<PtVar*>(pattern.get());
            return term_list[std::stoi(pv->name)];
        }
        case PatternType::TUPLE: {
            auto* pt = dynamic_cast<PtTuple*>(pattern.get());
            auto* tt = dynamic_cast<TyTuple*>(type.get());
            assert(tt && tt->fields.size() == pt->pattern_list.size());
            TermList sub_list;
            for (int i = 0; i < tt->fields.size(); ++i) {
                sub_list.push_back(comb::usage2Term(pt->pattern_list[i], term_list, tt->fields[i]));
            }
            return std::make_shared<TmTuple>(sub_list);
        }
        case PatternType::CONSTRUCTOR: {
            auto* pc = dynamic_cast<PtConstructor*>(pattern.get());
            auto* ti = dynamic_cast<TyInductive*>(type.get());
            Ty sub_type;
            for (auto& [cname, ctype]: ti->constructors) if (cname == pc->name) sub_type = ctype;
            assert(sub_type);
            return std::make_shared<TmApp>(std::make_shared<TmVar>(pc->name), usage2Term(pc->pattern, term_list, sub_type));
        }
    }
}

namespace {
    const int KSampleBase = 10;

    int _getSampleNum(int bnum, int snum) {
        int result = KSampleBase;
        for (int i = 1; i <= snum; ++i) result *= bnum;
        return result;
    }

    Pattern _getSingleDSUsage(const IOExampleList& example_list, int depth) {
        assert(!example_list.empty());
        int n = example_list[0].first.size();
        auto v = example_list[0].second;
        auto* vt = dynamic_cast<VTuple*>(v.get());
        if (vt) {
            PatternList case_list;
            for (int i = 0; i < vt->elements.size(); ++i) {
                IOExampleList field_example_list;
                for (auto& [inp, oup]: example_list) {
                    auto* current_value = dynamic_cast<VTuple*>(oup.get());
                    assert(current_value);
                    field_example_list.emplace_back(inp, current_value->elements[i]);
                }
                auto case_result = _getSingleDSUsage(field_example_list, depth);
                if (!case_result) return nullptr;
                case_list.push_back(case_result);
            }
            return std::make_shared<PtTuple>(case_list);
        }
        auto* vi = dynamic_cast<VInductive*>(v.get());
        if (vi) {
            std::unordered_set<int> possible_list;
            for (int i = 0; i < n; ++i) possible_list.insert(i);
            for (auto& [inp, oup]: example_list) {
                for (auto it = possible_list.begin(); it != possible_list.end();) {
                    auto pre = it; ++it;
                    if (!(oup == inp[*pre])) possible_list.erase(pre);
                }
                if (possible_list.empty()) break;
            }
            if (!possible_list.empty()) {
                return std::make_shared<PtVar>(std::to_string(*possible_list.begin()));
            }
            if (depth == 0) return nullptr;
            IOExampleList content_example_list;
            for (auto& [inp, oup]: example_list) {
                auto* current_value = dynamic_cast<VInductive*>(oup.get());
                assert(current_value && current_value->name == vi->name);
                content_example_list.emplace_back(inp, current_value->content);
            }
            auto content_result = _getSingleDSUsage(content_example_list, depth - 1);
            if (content_result) return std::make_shared<PtConstructor>(vi->name, content_result);
            return nullptr;
        }
        return std::make_shared<PtUnderScore>();
    }

    int _getMatchNum(const std::vector<int>& index_list, const IOExampleList& example_list, const Pattern& pattern) {
        int num = 0;
        for (auto id: index_list) {
            if (comb::isMatchUsage(pattern, example_list[id].second, example_list[id].first)) {
                ++num;
            }
        }
        return num;
    }

    bool _search(int bnum, int snum, const std::vector<int>& index_list, const IOExampleList& example_list, int depth_limit, PatternList& result, Env* env) {
        if (index_list.empty()) return true;
        int sample_num = _getSampleNum(bnum, snum), satisfy_bound = (int(index_list.size()) - 1) / bnum + 1;
        std::unordered_set<std::string> known_pattern;
        std::vector<std::pair<int, Pattern>> possibility;
        std::uniform_int_distribution<int> dis(0, int(index_list.size()) - 1);
        for (int _ = 0; _ < sample_num; ++_) {
            IOExampleList case_example_list;
            for (int i = 0; i < snum; ++i) case_example_list.push_back(example_list[index_list[dis(env->random_engine)]]);
            auto case_result = _getSingleDSUsage(example_list, depth_limit);
            if (case_result) {
                auto feature = case_result->toString();
                if (known_pattern.find(feature) == known_pattern.end()) {
                    known_pattern.insert(feature);
                    // TODO: use random check
                    auto use_num = _getMatchNum(index_list, example_list, case_result);
                    if (use_num >= satisfy_bound) possibility.emplace_back(use_num, case_result);
                }
            }
        }
        std::sort(possibility.begin(), possibility.end());
        for (int i = int(possibility.size()) - 1; i >= 0; --i) {
            auto& [num, pt] = possibility[i];
            if (num < satisfy_bound) continue;
            result.push_back(pt);
            std::vector<int> new_index_list;
            for (auto index: index_list) if (!comb::isMatchUsage(pt, example_list[index].second, example_list[index].first)) {
                new_index_list.push_back(index);
            }
            if (_search(bnum - 1, snum, new_index_list, example_list, depth_limit, result, env)) {
                return true;
            }
            result.pop_back();
        }
        return false;
    }

    PatternList _reorderPattern(const PatternList& pattern_list, const IOExampleList& example_list) {
        std::vector<std::pair<int, Pattern>> info_list;
        for (auto& pattern: pattern_list) {
            int num = 0;
            for (auto& [inp, oup]: example_list) {
                if (comb::isMatchUsage(pattern, oup, inp)) ++num;
            }
            info_list.emplace_back(-num, pattern);
        }
        std::sort(info_list.begin(), info_list.end());
        PatternList result;
        for (auto& [num, pattern]: info_list) {
            result.push_back(pattern);
        }
        return result;
    }
}

PatternList comb::getDSScheme(const IOExampleList &example_list, int depth_limit, Env* env) {
    std::vector<int> index_list;
    for (int i = 0; i < example_list.size(); ++i) index_list.push_back(i);
    PatternList result;
    for (int tot = 1;; ++tot) {
        for (int branch_num = 1; branch_num <= tot; ++branch_num) {
            int sample_num = tot - branch_num + 1;
            if (_search(branch_num, sample_num, index_list, example_list, depth_limit, result, env)) {
                return _reorderPattern(result, example_list);
            }
        }
    }
}