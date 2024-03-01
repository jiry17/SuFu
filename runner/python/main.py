from show import *
from config import RunnerConfig
from show import draw_trend
from executor import get_all_benchmark, execute
from cache import *
from pprint import pprint

val_time = CaredValue(lambda x: x["time"], "time")
val_example = CaredValue(lambda x: int(x["result"][0].split(' ')[0]), "example", draw_type=DrawType.LINEAR, merge_type=MergeType.AVE)
src_path = "/tmp/tmp.wHOuYKwdWN/"

def run_clia(is_restart=False):
    solver_list = ["polygen", "eusolver", "obe"]
    verifier_list = ["default", "random", "random200"]
    init_exe = src_path + "cmake-build-debug-remote-host/executor/run_splitor"
    run_exe = src_path + "cmake-build-debug-remote-host/executor/run_splitor_clia"
    os.system("cp " + init_exe + " " + run_exe)
    for solver in solver_list:
        runner_list = [RunnerConfig(src_path + "cmake-build-debug-remote-host/executor/run_splitor_clia",
                                    time_limit=300, memory_limit=4, flags=[solver, verifier], name=verifier, repeat_num = 1)
                    for verifier in verifier_list]
        benchmark_list = get_all_benchmark(src_path + "tests/clia/")

        cache_name = solver + "_clia"

        for runner in runner_list:
            result = execute(runner, benchmark_list, src_path + "runner/cache/" + cache_name + ".json", thread_num=8)
        draw_trend(get_all_solved(result), val_example, cache_name + "_example.png")
        draw_trend(get_all_solved(result), val_time, cache_name + "_time.png")

def run_string(is_restart=False):
    solver_list = ["maxflash", "vsa"]
    verifier_list = ["default", "random", "random200"]
    init_exe = src_path + "cmake-build-debug-remote-host/executor/run_splitor"
    run_exe = src_path + "cmake-build-debug-remote-host/executor/run_splitor_string"
    os.system("cp " + init_exe + " " + run_exe)
    for solver in solver_list:
        runner_list = [RunnerConfig(src_path + "cmake-build-debug-remote-host/executor/run_splitor_string",
                                    time_limit=300, memory_limit=4, flags=[solver, verifier], name=verifier, repeat_num=1)
                    for verifier in verifier_list]
        benchmark_list = get_all_benchmark(src_path + "tests/string/")

        cache_name = solver + "_string"
        if is_restart: clear_cache(src_path + "runner/cache/" + cache_name + ".json", lambda x, y, z: True)

        for runner in runner_list:
            result = execute(runner, benchmark_list, src_path + "runner/cache/" + cache_name + ".json", thread_num=8)
        draw_trend(get_all_solved(result), val_example, cache_name + "_example.png")
        draw_trend(get_all_solved(result), val_time, cache_name + "_time.png")
        

def run_samplesy():
    solver_list = ["splitor", "randomsy", "samplesy"]
    runner_list = [RunnerConfig(src_path + "cmake-build-debug-yifan/run",
                                time_limit=1200, memory_limit=8, flags=[solver], name=solver)
                   for solver in solver_list]
    benchmark_list = get_all_benchmark(src_path + "tests/string/")

    for runner in runner_list:
        result = execute(runner, benchmark_list, src_path + "runner/cache/test_samplesy.json")
    draw_trend(result, val_example, "samplesy_example.png")

def run_bv():
    solver_list = ["default", "random", "prior", "random200", "random2000"]
    runner_list = [RunnerConfig(src_path + "cmake-build-debug-remote-host/executor/run_cbs_icse10",
                                time_limit=1200, memory_limit=8, flags=[solver], name=solver, repeat_num = 5)
                   for solver in solver_list]
    benchmark_list = [str(i) for i in range(1, 22)]

    for runner in runner_list:
        result = execute(runner, benchmark_list, src_path +  "runner/cache/test_bv.json")
    draw_trend(get_all_solved(result), val_example, "bv_example.png")

def run_selector_string(num):
    init_exe = src_path + "cmake-build-debug-remote-host/executor/run_question_selection"
    run_exe = src_path + "cmake-build-debug-remote-host/executor/run_question_selection_string"
    os.system("cp " + init_exe + " " + run_exe)
    solver_list = ["samplesy", "randomsy", "random200", "random2000"]
    runner_list = [RunnerConfig(run_exe, time_limit = 1800, memory_limit=5, flags=[solver], repeat_num = num, name = solver) for solver in solver_list]
    benchmark_list = get_all_benchmark(src_path + "tests/string-interactive/")
    pprint(benchmark_list)
    
    cache = load_cache(src_path + "runner/cache/test_selector.json")
    for solver, solver_res in cache.items():
        new_solver_res = {}
        for benchmark, benchmark_res in solver_res.items():
            benchmark_path = "/tmp/tmp.wHOuYKwdWN/tests/string-interactive/" + benchmark + ".sl"
            if benchmark_path not in benchmark_list:
                print(benchmark_path, "not found")
            else:
                new_solver_res[benchmark] = benchmark_res
        cache[solver] = new_solver_res
    save_cache(src_path + "runner/cache/test_selector.json", cache, False)

    for runner in runner_list:
        result = execute(runner, benchmark_list, src_path + "runner/cache/test_selector.json", thread_num=8)
    draw_trend(result, val_example, "selector_example.png")
    draw_trend(result, val_time, "selector_time.png")
    list_all_result(result, val_example)

def run_selector_repair():
    solver_list = ["samplesy", "randomsy", "random200"]
    runner_list = [RunnerConfig(src_path + "cmake-build-debug-remote-host/executor/run_question_selection_int", time_limit = 3600, memory_limit=8, flags=[solver], repeat_num = 5) for solver in solver_list]
    benchmark_list = get_all_benchmark(src_path + "tests/repair/")
    for runner in runner_list:
        result = execute(runner, benchmark_list, src_path + "runner/cache/test_selector_repair.json")
    draw_trend(result, val_example, "selector_repair_example.png")
    draw_trend(result, val_time, "selector_repair_time.png")

def remove(cache_name, solver_cond = lambda x: True, benchmark_cond = lambda x: True, status_cond = lambda x: True):
    cache_file = src_path + "runner/cache/" + cache_name
    clear_cache(cache_file, lambda solver_name, benchmark_name, x: solver_cond(solver_name) and benchmark_cond(benchmark_name) and status_cond(x))

blist = {"exceljet1", "get-domain-name-from-url", "get-last-name-from-name-with-comma", "initials", "initials-long", "initials-long-repeat", "initials_small", "stackoverflow10", "stackoverflow11"}

if __name__ == "__main__":
    run_clia(True)
    run_string(True)