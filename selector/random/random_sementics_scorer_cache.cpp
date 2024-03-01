//
// Created by pro on 2022/5/18.
//

#include "istool/selector/random/random_semantics_scorer.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

using namespace selector::random;

TerminateEdgeInfo::TerminateEdgeInfo(double _weight): weight(_weight) {
}
ConstTerminateEdgeInfo::ConstTerminateEdgeInfo(const Data &_w, double _weight): w(_w), TerminateEdgeInfo(_weight) {
}
Data ConstTerminateEdgeInfo::getOutput(const DataList &inp) const {
    return w;
}
ParamTerminateEdgeInfo::ParamTerminateEdgeInfo(int _param_id, double _weight): param_id(_param_id), TerminateEdgeInfo(_weight) {
}
Data ParamTerminateEdgeInfo::getOutput(const DataList &inp) const {
    return inp[param_id];
}

PairMatchInfoCache::PairMatchInfoCache(const TopDownContextGraph::Node &node): example_num(0), res(nullptr) {
    for (auto& edge: node.edge_list) {
        auto* ps = dynamic_cast<ParamSemantics*>(edge.semantics.get());
        if (ps) edge_list.push_back(new ParamTerminateEdgeInfo(ps->id, edge.weight));
        auto* cs = dynamic_cast<ConstSemantics*>(edge.semantics.get());
        if (cs) edge_list.push_back(new ConstTerminateEdgeInfo(cs->w, edge.weight));
    }
    double total_prob = 0.0;
    for (auto* info: edge_list) total_prob += info->weight;
    int n = edge_list.size();
    match_info = std::vector<std::vector<int>>(n, std::vector<int>(n, 0));
    initRes();
}
PairMatchInfoCache::~PairMatchInfoCache() {
    for (auto* info: edge_list) {
        delete info;
    }
    delete[] res;
}
void PairMatchInfoCache::initRes() {
    int n = (1 << example_num);
    delete[] res;
    res = new RandomSemanticsScore[n];
    for (int i = 0; i < n; ++i) res[i] = 0;
    for (int i = 0; i < edge_list.size(); ++i) {
        for (int j = 0; j < edge_list.size(); ++j) res[match_info[i][j]] += edge_list[i]->weight * edge_list[j]->weight;
    }
}
void PairMatchInfoCache::pushExample(const DataList &inp) {
    DataList oup_list(edge_list.size());
    for (int i = 0; i < edge_list.size(); ++i) {
        oup_list[i] = edge_list[i]->getOutput(inp);
    }
    int weight = (1 << example_num);
    ++example_num;
    for (int i = 0; i < edge_list.size(); ++i) {
        for (int j = 0; j < edge_list.size(); ++j) {
            if (oup_list[i] == oup_list[j]) match_info[i][j] += weight;
        }
    }
    initRes();
}
void PairMatchInfoCache::popExample() {
    --example_num;
    for (auto& info: match_info) {
        for (auto& state: info) state >>= 1;
    }
    initRes();
}
void PairMatchInfoCache::getTmpPairStateWeight(const DataList &inp, RandomSemanticsScore *tmp) {
    global::recorder.start("terminate-info");
    int n = (1 << example_num);
    for (int i = 0; i < (n << 1); ++i) tmp[i] = 0.0;
    for (int i = 0; i < n; ++i) tmp[i] = res[i];
    std::unordered_map<std::string, std::vector<int>> oup_class;
    for (int i = 0; i < edge_list.size(); ++i) {
        auto oup = edge_list[i]->getOutput(inp);
        oup_class[oup.toString()].push_back(i);
    }
    for (auto& info: oup_class) {
        for (int x_id: info.second) {
            for (int y_id: info.second) {
                double weight = edge_list[x_id]->weight * edge_list[y_id]->weight;
                int status = match_info[x_id][y_id];
                tmp[status] -= weight; tmp[status + n] += weight;
            }
        }
    }
    global::recorder.end("terminate-info");
}

namespace {
    bool _isMatch(TerminateEdgeInfo* info, Semantics* s) {
        auto* ps = dynamic_cast<ParamSemantics*>(s);
        if (ps) {
            auto* p_info = dynamic_cast<ParamTerminateEdgeInfo*>(info);
            if (!p_info) return false;
            return p_info->param_id == ps->id;
        }
        auto* cs = dynamic_cast<ConstSemantics*>(s);
        assert(cs);
        auto* c_info = dynamic_cast<ConstTerminateEdgeInfo*>(info);
        if (!c_info) return false;
        return c_info->w == cs->w;
    }
}

int PairMatchInfoCache::getEdgeIdForSemantics(Semantics *semantics) {
    for (int i = 0; i < edge_list.size(); ++i) if (_isMatch(edge_list[i], semantics)) return i;
    LOG(FATAL) << "Unknown semantics " << semantics->getName();
}

void PairMatchInfoCache::getOneSideMatchWeight(Semantics *sem, RandomSemanticsScore *tmp) {
    int pos = getEdgeIdForSemantics(sem);
    for (int i = 0; i < (1 << example_num); ++i) tmp[i] = 0.0;
    for (int i = 0; i < edge_list.size(); ++i) {
        tmp[match_info[pos][i]] += edge_list[i]->weight;
    }
}


std::vector<int> TripleMatchInfoCache::K2T5[5];
std::vector<int> TripleMatchInfoCache::K5Size;

void TripleMatchInfoCache::prepare2T5(int example_num) {
    int n = (1 << example_num);
    if (K5Size.empty()) K5Size.push_back(1);
    for (int i = K5Size.size(); i <= example_num; ++i) {
        K5Size.push_back(K5Size[i - 1] * 5);
    }
    for (int x = 0; x < 5; ++x) {
        if (K2T5[x].empty()) K2T5[x].push_back(0);
        for (int i = K2T5[x].size(); i < n; ++i) {
            int now = K2T5[x][i >> 1] * 5;
            if (i & 1) now += x;
            K2T5[x].push_back(now);
        }
    }
}

int TripleMatchInfoCache::get5State(int fg, int fp, int gp) const {
#ifdef DEBUG
    assert((fp & gp) == (fp & fg));
#endif
    return K2T5[1][fg & (~fp)] + K2T5[2][fp & (~fg)] + K2T5[3][gp & (~fg)] + K2T5[4][fp & gp];
}

TripleMatchInfoCache::TripleMatchInfoCache(PairMatchInfoCache *two_cache, Semantics *s): edge_list(two_cache->edge_list),
    example_num(two_cache->example_num) {
    prepare2T5(two_cache->example_num + 1);
    p_id = two_cache->getEdgeIdForSemantics(s);
    int n = K5Size[two_cache->example_num];
    res = new RandomSemanticsScore[n];
    for (int i = 0; i < n; ++i) res[i] = 0.0;
    for (int f_id = 0; f_id < edge_list.size(); ++f_id) {
        int fp = two_cache->match_info[f_id][p_id];
        std::vector<int> info;
        for (int g_id = 0; g_id < edge_list.size(); ++g_id) {
            int gp = two_cache->match_info[g_id][p_id];
            int fg = two_cache->match_info[f_id][g_id];
            int m = get5State(fg, fp, gp);
            res[m] += edge_list[f_id]->weight * edge_list[g_id]->weight;
            info.push_back(m);
        }
        match_info.push_back(info);
    }
}
void TripleMatchInfoCache::getTmpTripleStateWeight(const DataList &inp, RandomSemanticsScore *tmp) {
    global::recorder.start("terminate-info");
    int n = K5Size[example_num];
    for (int i = 0; i < (n << 1); ++i) tmp[i] = 0.0;
    for (int i = 0; i < n; ++i) tmp[i] = res[i];
    std::unordered_map<std::string, std::vector<int>> oup_class;
    for (int i = 0; i < edge_list.size(); ++i) {
        oup_class[edge_list[i]->getOutput(inp).toString()].push_back(i);
    }
    for (auto& info: oup_class) {
        for (int x_id: info.second) {
            for (int y_id: info.second) {
                double weight = edge_list[x_id]->weight * edge_list[y_id]->weight;
                int status = match_info[x_id][y_id];
                tmp[status] -= weight; tmp[status + n] += weight;
            }
        }
    }
    global::recorder.end("terminate-info");
}
TripleMatchInfoCache::~TripleMatchInfoCache() {
    delete[] res;
}

CachePool::CachePool(TopDownContextGraph *_graph): graph(_graph), pair_cache_list(graph->node_list.size(), nullptr) {
    for (int i = 0; i < graph->node_list.size(); ++i) {
        pair_cache_list[i] = new PairMatchInfoCache(graph->node_list[i]);
    }
}
CachePool::~CachePool() {
    for (auto* cache: pair_cache_list) delete cache;
    for (auto& info: triple_cache_map) delete info.second;
}
void CachePool::pushExample(const Example &inp) {
    for (auto& info: triple_cache_map) delete info.second;
    triple_cache_map.clear();
    for (auto* cache: pair_cache_list) cache->pushExample(inp);
}
void CachePool::popExample() {
    for (auto& info: triple_cache_map) delete info.second;
    triple_cache_map.clear();
    for (auto* cache: pair_cache_list) cache->popExample();
}

void CachePool::getPairMatchRes(int node_id, const Example &inp, RandomSemanticsScore *res) {
    auto* cache = pair_cache_list[node_id];
    cache->getTmpPairStateWeight(inp, res);
}
void CachePool::getOneSizePairMatchRes(int node_id, Semantics *s, RandomSemanticsScore *res) {
    auto* cache = pair_cache_list[node_id];
    cache->getOneSideMatchWeight(s, res);
}
void CachePool::getTripleMatchRes(int node_id, Semantics *s, const Example &inp, RandomSemanticsScore *res) {
    auto feature = std::to_string(node_id) + "@" + s->getName();
    TripleMatchInfoCache* cache;
    if (triple_cache_map.count(feature)) cache = triple_cache_map[feature];
    else {
        cache = new TripleMatchInfoCache(pair_cache_list[node_id], s);
        triple_cache_map[feature] = cache;
    }
    cache->getTmpTripleStateWeight(inp, res);
}