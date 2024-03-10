from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec

autolifter_cache_path = cache_dir + "AutoLifter.json"
autolifter_root = src_path + "thirdparty/AutoLifter/"
autolifter_benchmark_path = autolifter_root + "run/benchmark.json"
autolifter_runner = autolifter_root + "run/run"

def get_file(paths):
    path = ""
    for name in paths[:-1]:
        path = os.path.join(path, name)
        if not os.path.exists(path): os.mkdir(path)
    final_path = os.path.join(run_dir, path, paths[-1])
    if os.path.exists(final_path): os.remove(final_path)
    return final_path

def execute(problem_name, benchmark):
    solver_name = "AutoLifter"
    oup_file = get_file([src_path + "exp/oup/", "autolifter", problem_name, benchmark])
    runnable_file = get_file([autolifter_root + "run/runnable/", solver_name, problem_name, benchmark])
    command = ["timeout " + str(time_out) + " " + exe_path,
               "--solver=\"" + solver_name + "\"",
               "--problem=\"" + problem_name + "\"", "--name=\"" + benchmark + "\"",
               "--oup=\"" + oup_file + "\"",
               "--runnable=\"" + runnable_file + "\"", ">/dev/null", "2>/dev/null"]
    command = " ".join(command)
    res = extractResult(oup_file)
    return res

def run_autolifter_tasks(autolifter_cache, clear_cache):
    if autolifter_cache is None or clear_cache: autolifter_cache = {}
    is_cover = False

    benchmark_info = load_json("benchmark.json", True)
    benchmarks = []
    for problem, track in benchmark_info.items():
        for benchmark in track:
            benchmarks.append((problem, benchmark))

    for problem, benchmark in tqdm(benchmarks):
        if problem not in autolifter_cache: autolifter_cache[problem] = {}
        if benchmark in autolifter_cache[problem]: continue
        autolifter_cache[problem][benchmark] = execute(problem, benchmark)
        save_cache(autolifter_cache_path, autolifter_cache, is_cover)
        is_cover = True
    return autolifter_cache

def print_autolifter_compare(cache, clear_cache):
    print("---compare with AutoLifter---")
    case_name = {"dad": "dac", "listr": "single-pass", "seg": "lsp", "ds": "segment-tree"}
    autolifter_res = load_cache(autolifter_cache_path)

    if autolifter_res == False: 

    num, atime, btime = 0, 0,0     
    for case, case_result in autolifter_res.items():
        for task_name, task_result in case_result.items():
            full_name = "incre-tests-autolifter-" + case_name[case] + "-" + task_name
            full_name = full_name.replace("@", "-").replace("+", "plus")
            if full_name[-1] == "-": full_name = full_name[:-1] + "neg"
            assert full_name in cache 
            if task_result["status"] != "success" or cache[full_name]["status"] != "success": continue
            num += 1
            atime += cache[full_name]["time"]
            btime += task_result["time"]["total"]
    
    print("the number of tasks solved by SuFu and AutoLifter:", num)
    print("average time of SuFu:", atime / num)
    print("average time of AutoLifter:", btime / num)
    print("\n")