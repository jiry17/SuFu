from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec

autolifter_cache_path = cache_dir + "AutoLifter.json"
autolifter_root = src_path + "thirdparty/AutoLifter/"
autolifter_benchmark_path = autolifter_root + "run/benchmark.json"
autolifter_runner = autolifter_root + "build/main"

def from_autolifter_case_name(name):
    return "lsp" if name == "longest-segment" else name

def from_autolifter_task_name(name):
    name_list = [('cnt1s', 'cnt_1s'), ('page10', 'page10-1'), ('sum@+', 'sum-plus'), ('sum@cover', 'sum-cover'), 
                 ('sqrsum@+', 'sqrsum-plus'), ('sqrsum@cover', 'sqrsum-cover'), ('mts@-', 'mts-neg'), ('mts@cover', 'mts-cover'), 
                 ('mss@-', 'mss-neg'), ('mss@cover', 'mss-cover'), ('min@-', 'min-neg'), ('2nd-min@-', 'second_min-neg'), 
                 ('3rd-min@-', 'third_min-neg'), ('max-1s@cover', 'max1s-cover'), ('max-1s-p@cover', 'max1s_p-cover')]
    for sufu_name, autolifter_name in name_list:
        if autolifter_name == name: return sufu_name 
    return name


def get_file(paths):
    path = ""
    for name in paths[:-1]:
        path = os.path.join(path, name)
        if not os.path.exists(path): os.mkdir(path)
    final_path = os.path.join(run_dir, path, paths[-1])
    if os.path.exists(final_path): os.remove(final_path)
    return final_path

def extractResult(oup_file):
    if not os.path.exists(oup_file):
        return {"status": "fail"}
    with open(oup_file, "r") as inp:
        lines = inp.readlines()
    if "Success" not in "".join(lines):
        return {"status": "fail"}

    def extractTotalTime(lines):
        for line in lines:
            if "Total time cost" in line:
                l, r = line.split(": ")
                if r[-1] == '\n': r = r[:-1]
                return float(r)
        return None

    total_time = extractTotalTime(lines)
    if total_time is None: return {"status": "fail"}
    return {"status": "success", "time": total_time}

def execute(problem_name, benchmark, timeout):
    solver_name = "AutoLifter"
    oup_file = get_file([src_path + "exp/oup/", "autolifter", problem_name, benchmark])
    runnable_file = get_file([autolifter_root + "run/runnable/", solver_name, problem_name, benchmark])
    command = ["timeout " + str(timeout) + " " + autolifter_runner,
               "--solver=\"" + solver_name + "\"",
               "--problem=\"" + problem_name + "\"", "--name=\"" + benchmark + "\"",
               "--oup=\"" + oup_file + "\"",
               "--runnable=\"" + runnable_file + "\"", ">/dev/null", "2>/dev/null"]
    command = " ".join(command)
    print(command)
    os.system(command)
    res = extractResult(oup_file)
    return res

def run_autolifter_tasks(autolifter_cache, clear_cache):
    if autolifter_cache is None or clear_cache: autolifter_cache = {}
    is_cover = False

    benchmark_info = load_cache(autolifter_benchmark_path)
    benchmarks = []
    for problem, track in benchmark_info.items():
        for benchmark in track:
            benchmarks.append((problem, benchmark))

    for problem, benchmark in tqdm(benchmarks):
        if problem not in autolifter_cache: autolifter_cache[problem] = {}
        if benchmark in autolifter_cache[problem]: continue
        autolifter_cache[problem][benchmark] = execute(problem, benchmark, timeout)
        save_cache(autolifter_cache_path, autolifter_cache, is_cover)
        is_cover = True
    return autolifter_cache

def ave(total, num):
    if num == 0: return "N/A"
    return total / num

def print_autolifter_compare(cache, clear_cache, timeout):
    print("---compare with AutoLifter (RQ2)---")
    autolifter_res = load_cache(autolifter_cache_path)
    autolifter_res = run_autolifter_tasks(autolifter_res, clear_cache)

    num, anum, bnum, atime, btime = 0, 0, 0, 0, 0
    for autolifter_case, case_result in autolifter_res.items():
        for autolifter_task_name, task_result in case_result.items():
            full_name = "incre-tests-autolifter-" + from_autolifter_case_name(autolifter_case) + "-" + from_autolifter_task_name(autolifter_task_name)
            full_name = full_name.replace("@", "-").replace("+", "plus")
            if full_name[-1] == "-": full_name = full_name[:-1] + "neg"
            assert full_name in cache 
            if cache[full_name]["status"] == "success": anum += 1
            if task_result["status"] == "success": bnum += 1
            if task_result["status"] != "success" or cache[full_name]["status"] != "success": continue
            num += 1
            atime += cache[full_name]["time"]
            btime += task_result["time"]
    print(anum, bnum)
    print("(SuFu) #solved tasks:", anum,  "averege time:", ave(atime, num))
    print("(AutoLifter) #solved tasks:", bnum,  "averege time:", ave(btime, num))
    print("\n")