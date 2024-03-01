//
// Created by pro on 2021/12/29.
//

#ifndef ISTOOL_VSA_BUILDER_H
#define ISTOOL_VSA_BUILDER_H

#include "istool/ext/vsa/vsa_extension.h"
#include "istool/ext/vsa/vsa.h"
#include "istool/basic/specification.h"
#include "istool/basic/time_guard.h"
#include <map>

class VSAPruner {
public:
    virtual bool isPrune(VSANode* node) = 0;
    virtual void clear() = 0;
    virtual ~VSAPruner() = default;
};

class TrivialPruner: public VSAPruner {
public:
    virtual bool isPrune(VSANode* node);
    virtual void clear();
    virtual ~TrivialPruner() = default;
};

typedef std::function<bool(VSANode*)> VSANodeChecker;

class SizeLimitPruner: public VSAPruner {
public:
    int size_limit, remain;
    VSANodeChecker checker;
    SizeLimitPruner(int _size_limit, const VSANodeChecker& checker);
    virtual bool isPrune(VSANode* node);
    virtual void clear();
    virtual ~SizeLimitPruner() = default;
};

class VSABuilder {
protected:
    virtual VSANode* _buildVSA(const Data& oup, const DataList& inp_list, TimeGuard* guard) = 0;
    virtual VSANode* _mergeVSA(VSANode* l, VSANode* r, TimeGuard* guard) = 0;
    std::map<std::string, VSANode*> single_build_cache;
    std::map<std::pair<VSANode*, VSANode*>, VSANode*> merge_build_cache;
public:
    Grammar* g;
    VSAPruner* pruner;
    VSAExtension* ext;
    Env* env;
    VSABuilder(Grammar* _g, VSAPruner* _pruner, Env* _env);
    VSANode* buildFullVSA();
    VSANode* buildVSA(const Data& oup, const DataList& inp_list, TimeGuard* guard);
    VSANode* mergeVSA(VSANode* l, VSANode* r, TimeGuard* guard);
    ~VSABuilder();
};

typedef std::shared_ptr<VSABuilder> PVSABuilder;

class DFSVSABuilder: public VSABuilder {
    VSANode* buildVSA(NonTerminal* nt, const WitnessData& oup, const DataList& inp_list, TimeGuard* guard, std::unordered_map<std::string, VSANode*>& cache);
    VSANode* mergeVSA(VSANode* l, VSANode* r, TimeGuard* guard, std::unordered_map<std::string, VSANode*>& cache);
protected:
    virtual VSANode* _buildVSA(const Data& oup, const DataList& inp_list, TimeGuard* guard);
    virtual VSANode* _mergeVSA(VSANode* l, VSANode* r, TimeGuard* guard);
public:
    DFSVSABuilder(Grammar* _g, VSAPruner* pruner, Env* env);
};

class BFSVSABuilder: public VSABuilder {
protected:
    virtual VSANode* _buildVSA(const Data& oup, const DataList& inp_list, TimeGuard* guard);
    virtual VSANode* _mergeVSA(VSANode* l, VSANode* r, TimeGuard* guard);
public:
    BFSVSABuilder(Grammar* _g, VSAPruner* pruner, Env* env);
};

#endif //ISTOOL_VSA_BUILDER_H
