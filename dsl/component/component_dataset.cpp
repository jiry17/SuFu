//
// Created by pro on 2022/2/13.
//

#include "istool/dsl/component/component_dsl.h"
#include "istool/sygus/theory/theory.h"
#include "istool/dsl/component/component_dataset.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include "glog/logging.h"

#define InfoStart(id) \
dsl::component::ComponentBenchmarkInfo _getTaskInfo## id() { \
    auto env = std::make_shared<Env>(); \
    sygus::setTheory(env.get(), TheoryToken::BV); \
    dsl::component::prepareEnv(env.get()); \
    auto bv_size = theory::bv::getBitVectorLength(env.get()); \
    auto bv_type = theory::bv::getTBitVector(bv_size); \
    auto x = program::buildParam(0, bv_type);

#define InfoEnd(id) }

namespace {
    PProgram _buildProgram(const PSemantics& semantics, const ProgramList& sub_list) {
        return std::make_shared<Program>(semantics, sub_list);
    }

    InfoStart(1)
        auto o = _buildProgram(env->getSemantics("bvdec"), {x});
        auto res = _buildProgram(env->getSemantics("bvand"), {x, o});
        return {env, res};
    InfoEnd(1)

    InfoStart(2)
        auto o = _buildProgram(env->getSemantics("bvinc"), {x});
        auto res = _buildProgram(env->getSemantics("bvand"), {x, o});
        return {env, res};
    InfoEnd(2)

    InfoStart(3)
        auto o = _buildProgram(env->getSemantics("bvneg"), {x});
        auto res = _buildProgram(env->getSemantics("bvand"), {x, o});
        return {env, res};
    InfoEnd(3)

    InfoStart(4)
        auto o = _buildProgram(env->getSemantics("bvdec"), {x});
        auto res = _buildProgram(env->getSemantics("bvxor"), {x, o});
        return {env, res};
    InfoEnd(4)

    InfoStart(5)
        auto o = _buildProgram(env->getSemantics("bvdec"), {x});
        auto res = _buildProgram(env->getSemantics("bvor"), {x, o});
        return {env, res};
    InfoEnd(5)

    InfoStart(6)
        auto o = _buildProgram(env->getSemantics("bvinc"), {x});
        auto res = _buildProgram(env->getSemantics("bvor"), {x, o});
        return {env, res};
    InfoEnd(6)

    InfoStart(7)
        auto o1 = _buildProgram(env->getSemantics("bvnot"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvinc"), {x});
        auto res = _buildProgram(env->getSemantics("bvand"), {o1, o2});
        return {env, res};
    InfoEnd(7)

    InfoStart(8)
        auto o1 = _buildProgram(env->getSemantics("bvnot"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvdec"), {x});
        auto res = _buildProgram(env->getSemantics("bvand"), {o1, o2});
        return {env, res};
    InfoEnd(8)

    InfoStart(9)
        auto o1 = _buildProgram(env->getSemantics("bvashr31"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvxor"), {o1, x});
        auto res = _buildProgram(env->getSemantics("bvsub"), {o2, o1});
        return {env, res, 1, {"bvashr"}, {31}};
    InfoEnd(9)

    InfoStart(10)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvand"), {x, y});
        auto o2 = _buildProgram(env->getSemantics("bvxor"), {x, y});
        auto res = _buildProgram(env->getSemantics("bvuleq"), {o2, o1});
        return {env, res, 2, {"bvuleq"}};
    InfoEnd(10)

    InfoStart(11)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvnot"), {y});
        auto o2 = _buildProgram(env->getSemantics("bvand"), {x, o1});
        auto res = _buildProgram(env->getSemantics("bvulq"), {y, o2});
        return {env, res, 2, {"bvulq"}};
    InfoEnd(11)

    InfoStart(12)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvnot"), {y});
        auto o2 = _buildProgram(env->getSemantics("bvand"), {x, o1});
        auto res = _buildProgram(env->getSemantics("bvuleq"), {o2, y});
        return {env, res, 2, {"bvuleq"}};
    InfoEnd(12)

    InfoStart(13)
        auto o1 = _buildProgram(env->getSemantics("bvashr31"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvneg"), {x});
        auto o3 = _buildProgram(env->getSemantics("bvashr31"), {o2});
        auto res = _buildProgram(env->getSemantics("bvor"), {o1, o3});
        return {env, res, 1, {"bvashr", "bvashr"}, {31}};
    InfoEnd(13)

    InfoStart(14)
        auto y = program::buildParam(1, bv_type);
        auto b1 = Bitset(bv_size, 0); b1.set(0, 1);
        auto p1 = program::buildConst(BuildData(BitVector, b1));
        auto t1 = _buildProgram(env->getSemantics("bvshl"), {p1, y});
        auto t2 = _buildProgram(env->getSemantics("bvdec"), {t1});
        auto o1 = _buildProgram(env->getSemantics("bvnot"), {t2});
        auto o2 = _buildProgram(env->getSemantics("bvadd"), {x, t2});
        auto res = _buildProgram(env->getSemantics("bvand"), {o1, o2});
        return {env, res, 2, {"bvshl", "bvadd"}, {1}};
    InfoEnd(14)

    InfoStart(15)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvand"), {x, y});
        auto o2 = _buildProgram(env->getSemantics("bvxor"), {x, y});
        auto b1 = Bitset(bv_size, 0); b1.set(0, 1);
        auto p1 = program::buildConst(BuildData(BitVector, b1));
        auto o3 = _buildProgram(env->getSemantics("bvashr"), {o2, p1});
        auto res = _buildProgram(env->getSemantics("bvadd"), {o1, o3});
        return {env, res, 2, {"bvashr", "bvadd"}, {1}};
    InfoEnd(15)

    InfoStart(16)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvxor"), {x, y});
        auto o2 = _buildProgram(env->getSemantics("bvuleq"), {y, x});
        o2 = _buildProgram(env->getSemantics("bvneg"), {o2});
        auto o3 = _buildProgram(env->getSemantics("bvand"), {o1, o2});
        auto res = _buildProgram(env->getSemantics("bvxor"), {o3, y});
        return {env, res, 2, {"bvuleq", "bvxor"}};
    InfoEnd(16)

    InfoStart(17)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvxor"), {x, y});
        auto o2 = _buildProgram(env->getSemantics("bvuleq"), {x, y});
        o2 = _buildProgram(env->getSemantics("bvneg"), {o2});
        auto o3 = _buildProgram(env->getSemantics("bvand"), {o1, o2});
        auto res = _buildProgram(env->getSemantics("bvxor"), {o3, y});
        return {env, res, 2, {"bvuleq", "bvxor"}};
    InfoEnd(17)

    InfoStart(18)
        auto y = program::buildParam(1, bv_type);
        auto o1 = _buildProgram(env->getSemantics("bvor"), {x, y});
        auto o2 = _buildProgram(env->getSemantics("bvxor"), {x, y});
        auto b1 = Bitset(bv_size, 0); b1.set(0, 1);
        auto p1 = program::buildConst(BuildData(BitVector, b1));
        auto o3 = _buildProgram(env->getSemantics("bvashr"), {o2, p1});
        auto res = _buildProgram(env->getSemantics("bvsub"), {o1, o3});
        return {env, res, 2, {"bvsub", "bvashr"}, {1}};
    InfoEnd(18)

    InfoStart(19)
        auto o1 = _buildProgram(env->getSemantics("bvdec"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvor"), {x, o1});
        auto o3 = _buildProgram(env->getSemantics("bvinc"), {o2});
        auto res = _buildProgram(env->getSemantics("bvand"), {x, o3});
        return {env, res};
    InfoEnd(19)

    InfoStart(20)
        auto o1 = _buildProgram(env->getSemantics("bvdec"), {x});
        auto o2 = _buildProgram(env->getSemantics("bvand"), {x, o1});
        auto res = _buildProgram(env->getSemantics("bveq"), {x, o2});
        return {env, res, 1, {"bveq"}};
    InfoEnd(20)

    InfoStart(21)
        auto o1 = _buildProgram(env->getSemantics("bvdec"), {x});
        std::vector<std::string> extra_list(4, "bvor");
        for (int k: {1, 2, 4, 8, 16}) {
            auto name = "bvushr" + std::to_string(k);
            auto o2 = _buildProgram(env->getSemantics(name), {o1});
            o1 = _buildProgram(env->getSemantics("bvor"), {o1, o2});
            extra_list.push_back(name);
        }
        auto res = _buildProgram(env->getSemantics("bvinc"), {o1});
        return {env, res, 1, extra_list};
    InfoEnd(21)
}

#define RegisterID(w) case w: {info = _getTaskInfo ## w(); break;}
Specification * dsl::component::getTask(int id) {
    dsl::component::ComponentBenchmarkInfo info;
    switch (id) {
        RegisterID(1) RegisterID(2) RegisterID(3)
        RegisterID(4) RegisterID(5) RegisterID(6)
        RegisterID(7) RegisterID(8) RegisterID(9)
        RegisterID(10) RegisterID(11) RegisterID(12)
        RegisterID(13) RegisterID(14) RegisterID(15)
        RegisterID(16) RegisterID(17) RegisterID(18)
        RegisterID(19) RegisterID(20) RegisterID(21)
        default:
            LOG(FATAL) << "Component task with id " << id << " does not exist";
    }
    LOG(INFO) << "Target: " << info.target->toString();
    return dsl::component::getComponentSpecification(info);
}