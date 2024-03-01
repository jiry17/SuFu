//
// Created by pro on 2022/5/22.
//

#include "istool/ext/vsa/vsa_inside_checker.h"
#include "istool/basic/config.h"
#include "glog/logging.h"

using namespace ext::vsa;

ProgramInsideVSAChecker::ProgramInsideVSAChecker(Env *_env, Grammar* _g, ExampleSpace *example_space):
    env(_env), ext(ext::vsa::getExtension(_env)), g(_g) {
    auto* fio_space = dynamic_cast<FiniteIOExampleSpace*>(example_space);
    for (auto& example: fio_space->example_space) {
        io_example_list.push_back(fio_space->getIOExample(example));
    }
}

DataList ProgramInsideVSAChecker::getOutput(Program *program) {
    auto feature = program->toString();
    if (oup_cache.count(feature)) return oup_cache[feature];
    auto* sem = program->semantics.get();
    auto* ps = dynamic_cast<ParamSemantics*>(sem);
    if (ps) {
        DataList res(io_example_list.size());
        for (int i = 0; i < io_example_list.size(); ++i) res[i] = io_example_list[i].first[ps->id];
        return oup_cache[feature] = res;
    }
    auto* cs = dynamic_cast<ConstSemantics*>(sem);
    if (cs) return oup_cache[feature] = DataList(io_example_list.size(), cs->w);
    auto* fs = dynamic_cast<FullExecutedSemantics*>(sem);
    if (fs) {
        DataStorage sub_storage;
        for (const auto& sub: program->sub_list) {
            sub_storage.push_back(getOutput(sub.get()));
        }
        DataList res(io_example_list.size());
        for (int i = 0; i < io_example_list.size(); ++i) {
            DataList current_inp(program->sub_list.size());
            for (int j = 0; j < program->sub_list.size(); ++j) {
                current_inp[j] = sub_storage[j][i];
            }
            res[i] = fs->run(std::move(current_inp), nullptr);
        }
        return oup_cache[feature] = res;
    }
    LOG(WARNING) << "The output of " << program->semantics->getName() << " cannot be cached";
    DataList res(io_example_list.size());
    for (int i = 0; i < io_example_list.size(); ++i) {
        res[i] = env->run(program, io_example_list[i].first);
    }
    return oup_cache[feature] = res;
}
WitnessList ProgramInsideVSAChecker::getWitness(Semantics *sem, const Data &oup, int id) {
    auto feature = sem->getName() + "@" + oup.toString() + "@" + std::to_string(id);
    if (wit_cache.count(feature)) return wit_cache[feature];
    auto wit_list = ext->getWitness(sem, std::make_shared<DirectWitnessValue>(oup), io_example_list[id].first);
    return wit_cache[feature] = wit_list;
}
bool ProgramInsideVSAChecker::isValid(Program *program) {
    auto feature = program->toString();
    if (cache.count(feature)) return cache[feature];
    global::recorder.start("vsa-check");
    DataList oup = getOutput(program);
    DataStorage inp_storage;
    for (const auto& sub: program->sub_list) {
        inp_storage.push_back(getOutput(sub.get()));
    }
    for (int i = 0; i < io_example_list.size(); ++i) {
        ext->prepareEnv(g, io_example_list[i]);
        global::recorder.start("getwit");
        auto wit_list = getWitness(program->semantics.get(), oup[i], i);
        global::recorder.end("getwit");
        bool is_valid = false;
        for (const auto& term: wit_list) {
            bool flag = true;
            for (int j = 0; j < program->sub_list.size(); ++j) {
                if (!term[j]->isInclude(inp_storage[j][i])) {
                    flag = false; break;
                }
            }
            if (flag) {
                is_valid = true; break;
            }
        }
        if (is_valid) {
            global::recorder.end("vsa-check");
            return cache[feature] = true;
        }
    }
    // LOG(INFO) << "invalid " << program->toString() << std::endl;
    global::recorder.end("vsa-check");
    return cache[feature] = false;
}