//
// Created by pro on 2022/5/12.
//

#include "istool/ext/deepcoder/data_example_sampler.h"
#include "istool/basic/example_sampler.h"
#include "istool/ext/deepcoder/anonymous_function.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/clia/clia_example_sampler.h"
#include "istool/ext/deepcoder/data_type.h"
#include "glog/logging.h"

namespace {
    PProgram _buildChecker(const ExampleChecker& checker) {
        auto f = [checker](DataList&& inp, ExecuteInfo* info) {
            auto res = checker(info->param_value);
            return BuildData(Bool, res);
        };
        auto as = std::make_shared<AnonymousSemantics>(f, "checker");
        ProgramList sub_list;
        return std::make_shared<Program>(as, sub_list);
    }
    int KDefaultIntMin = -3;
    int KDefaultIntMax = 3;
    int KDefaultSizeMax = 10;
}

BasicRandomSampler::BasicRandomSampler(const TypeList &_type_list, PProgram &_chk, Env *_env): type_list(_type_list),
    env(_env), checker(_chk) {
    initConst();
}
BasicRandomSampler::BasicRandomSampler(const TypeList &_type_list, const ExampleChecker &_chk, Env *_env): type_list(_type_list),
    env(_env), checker(_buildChecker(_chk)) {
    initConst();
}
void BasicRandomSampler::initConst() {
    int_min = env->getConstRef(theory::clia::KSampleIntMinName, BuildData(Int, KDefaultIntMin));
    int_max = env->getConstRef(theory::clia::KSampleIntMaxName, BuildData(Int, KDefaultIntMax));
    size_max = env->getConstRef(ext::ho::KSampleDSSizeName, BuildData(Int, KDefaultSizeMax));
}

BTreeNode BasicRandomSampler::sampleTree(Type *node_type, Type *leaf_type, int size) {
    if (size == 0) return std::make_shared<BTreeLeafValue>(sampleWithType(leaf_type));
    std::uniform_int_distribution<int> d(0, size - 1);
    int l_size = d(env->random_engine), r_size = size - 1 - l_size;
    return std::make_shared<BTreeInternalValue>(sampleTree(node_type, leaf_type, l_size),
                                                sampleTree(node_type, leaf_type, r_size), sampleWithType(node_type));
}

Data BasicRandomSampler::sampleWithType(Type *type) {
    if (dynamic_cast<TBool*>(type)) {
        std::bernoulli_distribution d;
        return BuildData(Bool, d(env->random_engine));
    }
    if (dynamic_cast<TInt*>(type)) {
        std::uniform_int_distribution<int> d(theory::clia::getIntValue(*int_min), theory::clia::getIntValue(*int_max));
        return BuildData(Int, d(env->random_engine));
    }
    auto* ts = dynamic_cast<TSum*>(type);
    if (ts) {
        std::uniform_int_distribution<int> d(0, int(ts->sub_types.size()) - 1);
        int id = d(env->random_engine);
        auto content = sampleWithType(ts->sub_types[id].get());
        return Data(std::make_shared<SumValue>(id, content));
    }
    auto* tp = dynamic_cast<TProduct*>(type);
    if (tp) {
        DataList content;
        for (const auto& t: tp->sub_types) content.push_back(sampleWithType(t.get()));
        return Data(std::make_shared<ProductValue>(content));
    }
    auto* tl = dynamic_cast<TList*>(type);
    if (tl) {
        std::uniform_int_distribution<int> d(0, theory::clia::getIntValue(*size_max));
        int size = d(env->random_engine);
        DataList content;
        for (int i = 0; i < size; ++i) content.push_back(sampleWithType(tl->content.get()));
        return BuildData(List, content);
    }
    auto* tt = dynamic_cast<TBTree*>(type);
    if (tt) {
        std::uniform_int_distribution<int> d(0, theory::clia::getIntValue(*size_max));
        return Data(sampleTree(tt->content.get(), tt->leaf.get(), d(env->random_engine)));
    }
    LOG(FATAL) << "Type " << type->getName() << " is not supported by BasicRandomSampler";
}

ExampleList BasicRandomSampler::generateExamples(TimeGuard *guard) {
    Example res;
    for (const auto& type: type_list) {
        res.push_back(sampleWithType(type.get()));
    }
    return {res};
}
const std::string ext::ho::KSampleDSSizeName = "Sample@DataSizeMax";
