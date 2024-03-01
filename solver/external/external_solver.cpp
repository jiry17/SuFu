//
// Created by pro on 2022/5/24.
//

#include "istool/solver/external/external_solver.h"
#include "istool/sygus/theory/theory.h"
#include "istool/sygus/theory/basic/clia/clia.h"
#include "istool/sygus/theory/basic/string/str.h"
#include "istool/sygus/theory/basic/bv/bv.h"
#include <unistd.h>
#include "glog/logging.h"

void ExternalSolver::init() {
    if (access(install_path.c_str(), 0) == -1) {
        LOG(FATAL) << "the external solver" << solver_name << " has not been installed";
    }
}
ExternalSolver::ExternalSolver(Env *_env, const std::string &_solver_name, const std::string &_install_path):
    env(_env), solver_name(_solver_name), install_path(_install_path) {
}

const std::string solver::external::KExternalMemoryLimitName = "External@MemoryLimit";

void solver::external::runCommand(const std::string &command, const std::string& pre_command, int time_limit, int memory_limit) {
    std::string current = command;
     if (time_limit > 0) {
         current = "timeout " + std::to_string(time_limit) + " " + current;
     }
     if (memory_limit > 0) {
         memory_limit *= (1 << 20);
         current = "ulimit -v " + std::to_string(memory_limit) + ";" + command;
     }
     std::system(current.c_str());
}

ExternalSyGuSPBESolver::ExternalSyGuSPBESolver(Specification *spec, ExternalSolver *_solver, bool _is_new_style):
    PBESolver(spec), solver(_solver), io_space(dynamic_cast<IOExampleSpace*>(spec->example_space.get())), is_new_style(_is_new_style) {
    if (!io_space) LOG(FATAL) << "ExternalSyGuSPBESolver supports only IOExampleSpace";
}
ExternalSyGuSPBESolver::~ExternalSyGuSPBESolver() noexcept {
    delete solver;
}
FunctionContext ExternalSyGuSPBESolver::synthesis(const std::vector<Example> &example_list, TimeGuard *guard) {
    auto inp = solver::external::createRandomFile(".sl");
    printSyGuSFile(example_list, inp);
    auto res = solver->invoke(inp, guard);
    std::system(("rm " + inp).c_str());
    return res;
}

// TODO: generalize this part
namespace {
    std::string _getTypeName(Type* type, bool is_new_style) {
        if (dynamic_cast<TInt*>(type)) return "Int";
        if (dynamic_cast<TBool*>(type)) return "Bool";
        if (dynamic_cast<TString*>(type)) return "String";
        auto* bt = dynamic_cast<TBitVector*>(type);
        if (bt) {
            if (is_new_style) return "(_ BitVec " + std::to_string(bt->size) + ")";
            return "(BitVec " + std::to_string(bt->size) + ")";
        }
        LOG(FATAL) << "Unknown type " << type->getName();
    }

    std::string _getDataName(const Data& data, bool is_cvc5) {
        auto* iv = dynamic_cast<IntValue*>(data.get());
        if (iv) {
            if (is_cvc5 && iv->w < 0) {
                return "(- " + std::to_string(-iv->w) + ")";
            }
            return std::to_string(iv->w);
        }
        auto* bv = dynamic_cast<BoolValue*>(data.get());
        if (bv) return bv->w ? "true" : "false";
        auto* sv = dynamic_cast<StringValue*>(data.get());
        if (sv) return "\"" + sv->s + "\"";
        auto* bvv = dynamic_cast<BitVectorValue*>(data.get());
        if (bvv) return bvv->w.toXString();
        LOG(FATAL) << "Unknown data " << data.toString();
    }

    std::string _getSemanticsName(Semantics* sem, bool is_cvc5) {
        auto* cs = dynamic_cast<ConstSemantics*>(sem);
        if (cs) return _getDataName(cs->w, is_cvc5);
        auto name = sem->getName();
        static const std::unordered_map<std::string, std::string> replace = {
                {"&&", "and"}, {"||", "or"}, {"!", "not"}, {"=b", "="}, {"im", "if0"}
        };
        auto it = replace.find(name);
        if (it != replace.end()) return it->second;
        return name;
    }

    std::string _getSymbolName(NonTerminal* symbol) {
        if (symbol->id == 0) return "Start";
        return symbol->name;
    }

    std::string _replaceAllSubstr(const std::string& s, const std::string& x, const std::string& y) {
        auto res(s);
        std::string::size_type pos;
        while ((pos = res.find(x)) != std::string::npos) {
            res.replace(pos, x.length(), y);
        }
        return res;
    }
}
void ExternalSyGuSPBESolver::printSyGuSFile(const std::vector<Example> &example_list, const std::string &file_name) {
    std::string res;
    bool is_cvc5 = solver->solver_name == "CVC5";
    // build header
    auto theory = sygus::getSyGuSTheory(spec->env.get());
    res += "(set-logic " + sygus::theoryToken2String(theory) + ")\n";
    res += sygus::getSyGuSHeader(spec->env.get());
    // build grammar
    for (auto& info: spec->info_list) {
        std::string current = "(synth-fun " + info->name + " (";
        for (int i = 0; i < info->inp_type_list.size(); ++i) {
            if (i) current += " ";
            current += "(Param" + std::to_string(i) + " " + _getTypeName(info->inp_type_list[i].get(), is_new_style) + ")";
        }
        current += ") " + _getTypeName(info->oup_type.get(), is_new_style) + "\n(";
        info->grammar->indexSymbol();
        if (is_new_style) {
            for (auto* nt: info->grammar->symbol_list) {
                current += "(" + _getSymbolName(nt) + " " + _getTypeName(nt->type.get(), is_new_style) + ") ";
            }
            current += ")\n(";
        }
        for (auto* nt: info->grammar->symbol_list) {
            current += "(" + _getSymbolName(nt) + " " + _getTypeName(nt->type.get(), is_new_style) + " (\n";
            for (auto* rule: nt->rule_list) {
                auto* cr = dynamic_cast<ConcreteRule*>(rule);
                if (!cr) LOG(FATAL) << "Current implementation of LIASolver requires ConcreteRule";
                if (rule->param_list.empty()) current += _getSemanticsName(cr->semantics.get(), is_cvc5);
                else {
                    current += "(" + _getSemanticsName(cr->semantics.get(), is_cvc5);
                    for (auto* sub: rule->param_list) current += " " + _getSymbolName(sub);
                    current += ")";
                }
                current += "\n";
            }
            current += "))\n";
        }
        current += "))\n"; res += current;
    }

    // build constraint
    auto f_name = spec->info_list[0]->name;
    for (const auto& example: example_list) {
        auto io_example = io_space->getIOExample(example);
        std::string current = "(constraint (= (" + f_name;
        for (const auto& data: io_example.first) {
            current += " " + _getDataName(data, is_cvc5);
        }
        current += ") " + _getDataName(io_example.second, is_cvc5) + "))\n";
        res += current;
    }
    res += "(check-synth)";
    if (is_cvc5) {
        res = _replaceAllSubstr(res, "str.to.int", "str.to_int");
        res = _replaceAllSubstr(res, "int.to.str", "str.from_int");
    }
    // LOG(INFO) << "constructed sygus file";
    // std::cout << is_new_style << std::endl;
    // std::cout << res << std::endl;
    auto* f = fopen(file_name.c_str(), "w");
    fprintf(f, "%s", res.c_str());
    fclose(f);
}

std::string solver::external::createRandomFile(const std::string &suffix) {
    while (1) {
        std::string file_path = "/tmp/" + std::to_string(rand()) + suffix;
        if (access(file_path.c_str(), 0) == -1) {
            std::string command = "touch " + file_path;
            std::system(command.c_str());
            return file_path;
        }
    }
}

namespace {
    int KDefaultExternalMemoryLimit = 4;
}
void solver::external::setExternalMemoryLimit(Env *env, int limit) {
    env->setConst(KExternalMemoryLimitName, BuildData(Int, limit));
}
int solver::external::getExternalMemoryLimit(Env *env) {
    auto* d = env->getConstRef(KExternalMemoryLimitName, BuildData(Int, KDefaultExternalMemoryLimit));
    return theory::clia::getIntValue(*d);
}