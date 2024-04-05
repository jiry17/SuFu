from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec, get_all

executor = src_path + "build/executor/run"
res_dir = run_dir + "res/sufu/"
oup_dir = run_dir + "oup/sufu/"
label_dir = run_dir + "label/"
sufu_cache_path = cache_dir + "sufu.json"

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

def run_sufu_tasks(sufu_cache, clear_cache, use_gurobi, time_out):
    if sufu_cache is None or clear_cache: sufu_cache = {}
    is_cover = False

    for task_path in tqdm(get_all_benchmark_rec(benchmark_root, lambda x: ".f" in x and "autolifter-base" not in x)):
        tmp = task_path[len(src_path):-2].split("/")
        tmp[0] = "incre-tests"
        name = "-".join(tmp)
        if name in sufu_cache: continue
        label_path = label_dir + name
        res_path = res_dir + name
        oup_path = oup_dir + name
        os.system("touch " + oup_path)

        command = ["timeout " + str(time_out), executor, "-benchmark=" + task_path, "-output=" + res_path, "-use_gurobi=" + use_gurobi]
        command += [">" + oup_path, "2>/dev/null"]
        command = " ".join(command)
        os.system(command)

        with open(oup_path, "r") as inp:
            lines = inp.readlines()
        if len(lines) == 0 or "Success" not in lines[-1]:
            sufu_cache[name] = {"status": "fail"}
        else:
            sufu_cache[name] = {"status": "success", "time": float(lines[-2][:-1])}
        save_cache(sufu_cache_path, sufu_cache, is_cover)
        is_cover = True

def get_source_name(name):
    if name == "fusion": return "Fusion"
    if name == "synduce": return "Recursion"
    if name == "autolifter": return "D&C"
    if name == "total": return "Total"
    return name

def get_attr_name(name):
    if name == "task-num": return "#Task"
    if name == "num": return "SuFu"
    if name == "label": return "(Sketch Generation) Time"
    if name == "time": return "(Sketch Solving) Time"
    if name == "align-size": return "(Sketch Solving) S_compress"
    if name == "extract-size": return "(Sketch Solving) S_extract"
    if name == "comb-size": return "(Sketch Solving) S_holes"

def print_attr(sufu_cache, clear_cache):
    print("---The detailed performance of SuFu (RQ1)---")
    print("Table 4")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["task-num"]:
            print("line: ", get_source_name(batch_name), ", column: ", get_attr_name(attr), ", value: ", get_all(sufu_cache, batch_name, attr, True), sep="")
    
    print("\nTable 5")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["task-num", "num"]:
            print("line: ", get_source_name(batch_name), ", column: ", get_attr_name(attr), ", value: ", get_all(sufu_cache, batch_name, attr, True), sep="")
    
    print("\nTable 6")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["label", "time", "align-size", "extract-size", "comb-size"]:
            print("line: ", get_source_name(batch_name), ", column: ", get_attr_name(attr), ", value: ", get_all(sufu_cache, batch_name, attr, True), sep="")
    print("\n")
