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
    COMPONENT_BASED_SYNTHESIS,
    EUSOLVER,
    EXTERNAL_EUSOLVER,
    MAXFLASH,
    VANILLA_VSA,
    POLYGEN,
    POLYGEN_CONDITION,
    EXTERNAL_CVC5,
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
         * @config "encoder"
         *  Type: std::string
         *  Set the grammar encoder for the target programs. ("Linear" or "Tree")
         *  Default: "Linear"
         */
        Solver* buildCBS(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "runnable"
         *   Type: std::function<bool(Program*)>
         *   Check whether a program is runnable.
         *   Default: Always true
         */
        Solver* buildOBE(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildEuSolver(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "prepare"
         *   Type: std::function<void(Grammar*, Env* env, const IOExample&)> (VSAEnvSetter);
         *   Set constants that are used for building VSAs.
         *   Default: For String, defined in executor/invoker/vsa_invoker.cpp
         *
         * @config "pruner"
         *   Type: VSAPruner* (defined in istool/ext/vsa/vsa_builder.h)
         *   An object that prunes off useless VSA nodes.
         *   Default: For String, keeps the first 1e5 VSA nodes and skip those nodes that use stings longer than the input/output.
         *
         * @config "builder"
         *   Type: std::shared_ptr<VSABuilder>
         *   An object used to construct VSA from examples.
         *   Default: std::make_shared<BFSVSABuilder>(info->grammar, pruner, spec->env.get(), prepare))
         *
         * @config "selector"
         *   Type: VSAProgramSelector*
         *   An object used to select a program from the VSA.
         *   Default: VSARandomProgramSelector*
         *
         * @config "height"
         *   Type: int
         *   The largest height of programs considered by the solver.
         *   Default: 7
         */
        Solver* buildVanillaVSA(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "prepare"
         *   The same as config "prepare" of function invokeVanillaVSA.
         *
         * @config "model"
         *   Type: TopDownModel*
         *   A cost model for programs in the program space. MaxFlash always returns the solution with the minimum cost.
         *   Default: ext::vsa::getSizeModel()
         */
        Solver* buildMaxFlash(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "is_staged"
         *   Whether synthesize terms and conditions in two separate CEGIS rounds, default false
         */
        Solver* buildPolyGen(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildCondSolver(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildLIASolver(Specification* spec, Verifier* v, const InvokeConfig& config);

        /**
         * @config "memory"
         * Type: int
         * The memory limit (GB) for external solvers
         * Default: 4
         */
        Solver* buildExternalEuSolver(Specification* spec, Verifier* v, const InvokeConfig& config);
        Solver* buildExternalCVC5(Specification* spec, Verifier* v, const InvokeConfig& config);
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
    std::pair<int, FunctionContext> getExampleNum(Specification* spec, Verifier* v, SolverToken solver_token, TimeGuard* guard, const InvokeConfig& config={});
    SolverToken string2TheoryToken(const std::string& name);
}

#endif //ISTOOL_INVOKER_H
