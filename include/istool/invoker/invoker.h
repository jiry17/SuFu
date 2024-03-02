//
// Created by pro on 2022/2/15.
//

#ifndef ISTOOL_INVOKER_H
#define ISTOOL_INVOKER_H

#include "istool/basic/specification.h"
#include "istool/basic/time_guard.h"
#include "istool/basic/verifier.h"
#include "istool/solver/solver.h"
#include "multithread_guard.h"
#include "glog/logging.h"

enum class SolverToken {
    OBSERVATIONAL_EQUIVALENCE,
    POLYGEN,
    POLYGEN_CONDITION,
    MULTI_THREAD
};

class InvokeConfig {
    class InvokeConfigItem {
    public:
        void* data;
        std::function<void(void*)> free_operator;
        std::function<void*(void*)> copy_operator;
        InvokeConfigItem(void* _data, std::function<void(void*)> _free_operator, std::function<void*(void*)> _copy_operator);
        InvokeConfigItem(const InvokeConfigItem& item);
        ~InvokeConfigItem();
    };
public:
    std::unordered_map<std::string, InvokeConfigItem*> item_map;

    template<class T> void set(const std::string& name, const T& w) {
        auto it = item_map.find(name);
        if (it != item_map.end()) delete it->second;
        auto free_operator = [](void* w){delete static_cast<T*>(w);};
        auto copy_operator = [](void* w){return new T(*(static_cast<T*>(w)));};
        auto* item = new InvokeConfigItem(new T(w), free_operator, copy_operator);
        item_map[name] = item;
    }
    template<class T> T access(const std::string& name, const T& default_w) const {
        auto it = item_map.find(name);
        if (it == item_map.end()) return default_w;
        auto *res = static_cast<T *>(it->second->data);
        if (!res) {
            LOG(FATAL) << "Config Error: unexpected type for config " << name;
        }
        return *res;
    }
    InvokeConfig() = default;
    InvokeConfig(const InvokeConfig& config);
    ~InvokeConfig();
};

namespace invoker {
    namespace single {
        /**
         * @config "runnable"
         *   Type: std::function<bool(Program*)>
         *   Check whether a program is runnable.
         *   Default: Always true
         */
        Solver* buildOBE(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "is_staged"
         *   Whether synthesize terms and conditions in two separate CEGIS rounds, default false
         */
        Solver* buildPolyGen(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildCondSolver(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildLIASolver(Specification* spec, Verifier* v, const InvokeConfig& config);
    }

    namespace multi {
        typedef std::function<Solver*(Specification*, Verifier*)> SolverBuilder;
        /**
         * @config "solver_list"
         * Type: std::vector<SolverBuilder>
         * The list of solvers to be run in parallel
         */
        FunctionContext synthesis(Specification* spec, Verifier* v, const InvokeConfig& config, TimeGuard* guard);
    }

    Solver* builderSolver(Specification* spec, Verifier* v, SolverToken token, const InvokeConfig& config);
    FunctionContext synthesis(Specification* spec, Verifier* v, SolverToken solver_token, TimeGuard* guard, const InvokeConfig& config={});
}

#endif //ISTOOL_INVOKER_H
