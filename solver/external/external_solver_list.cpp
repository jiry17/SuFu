//
// Created by pro on 2022/5/25.
//

#include "istool/solver/external/external_solver_list.h"
#include "istool/basic/config.h"
#include "istool/sygus/parser/json_util.h"
#include "istool/sygus/parser/parser.h"
#include <fstream>
#include "glog/logging.h"

ExternalEuSolver::ExternalEuSolver(Env *env, const std::string &path): ExternalSolver(env, "EuSolver", path.empty() ? config::KEuSolverPath: path) {
}

namespace {
    std::string _readAllLines(const std::string& file) {
        std::ifstream inf(file.c_str(), std::ios::in);
        char ch; std::string res;
        while (inf.get(ch)) res += ch;
        inf.close();
        return res;
    }

    char _buildx(int k) {
        if (k < 10) return '0' + k;
        return 'a' + (k - 10);
    }

    std::string _b2x(const std::string& x) {
        std::string res;
        for (int i = 0; i < x.length(); ++i) {
            if (i + 1 < x.length() && x[i] == '#' && x[i + 1] == 'b') {
                std::vector<int> A; int now = i + 2;
                while (now < x.length() && x[now] == '0' || x[now] == '1') {
                    A.push_back(x[now] - '0'); ++now;
                }
                i = now - 1; res += "#x";
                assert(A.size() % 4 == 0);
                for (int j = 0; j < A.size(); j += 4) res += _buildx((A[j] << 3) + (A[j + 1] << 2) + (A[j + 2] << 1) + A[j + 3]);
            } else res += x[i];
        }
        return res;
    }
}

FunctionContext ExternalEuSolver::invoke(const std::string &benchmark_file, TimeGuard *guard) {
    auto oup_file = solver::external::createRandomFile(".out");
    std::string command = install_path + "/bin/run_eusolver " + benchmark_file + " > " + oup_file;
    int time_limit = -1;
    if (guard) time_limit = int(guard->getRemainTime());
    int memory_limit = solver::external::getExternalMemoryLimit(env);
    // std::cout << command << std::endl;
    solver::external::runCommand(command, "", time_limit, memory_limit);
    // read result
    auto res = _readAllLines(oup_file);
    int pos = 0;
    while (pos < res.length() && res[pos] != '\n') pos++;
    if (pos == res.length()) {
        LOG(FATAL) << "Unexpected result returned by EuSolver: " << res;
    }
    res = res.substr(pos);
    auto* ouf = fopen(oup_file.c_str(), "w");
    fprintf(ouf, "%s\n", res.c_str());
    fclose(ouf);
    auto value = parser::getJsonForSyGuSFile(oup_file)[0];
    // LOG(INFO) << "Synthesis res " << value;
    auto name = value[1].asString();
    auto program = json::getProgramFromJson(value, env);
    std::system(("rm " + oup_file).c_str());
    return semantics::buildSingleContext(name, program);
}

namespace {
    std::string _replaceAll(const std::string& s, const std::string& x, const std::string& y) {
        auto res(s); std::string::size_type pos;
        while ((pos = res.find(x)) != std::string::npos) {
            res.replace(pos, x.length(), y);
        }
        return res;
    }
}



FunctionContext ExternalCVC5::invoke(const std::string &benchmark_file, TimeGuard *guard) {
    auto oup_file = solver::external::createRandomFile(".out");
    std::string command = install_path + " " + benchmark_file + " --sygus-si=none --no-sygus-add-const-grammar > " + oup_file;
    int time_limit = -1;
    if (guard) time_limit = int(guard->getRemainTime());
    int memory_limit = solver::external::getExternalMemoryLimit(env);
    solver::external::runCommand(command, "", time_limit, memory_limit);

    auto res = _readAllLines(oup_file);
    res = _b2x(res);
    res = _replaceAll(res, "str.from_int", "int.to.str");
    res = _replaceAll(res, "str.to_int", "str.to.int");
    auto* ouf = fopen(oup_file.c_str(), "w");
    // std::cout << res << std::endl;
    fprintf(ouf, "%s\n", res.c_str());
    fclose(ouf);

    auto value = parser::getJsonForSyGuSFile(oup_file)[0][0];
    auto name = value[1].asString();
    auto program = json::getProgramFromJsonWithLet(value, env);
    std::system(("rm " + oup_file).c_str());
    return semantics::buildSingleContext(name, program);
}

ExternalCVC5::ExternalCVC5(Env *env, const std::string &path): ExternalSolver(env, "CVC5", path.empty() ? config::KCVC5Path : path) {}


