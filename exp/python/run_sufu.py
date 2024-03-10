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

def run_sufu_tasks(sufu_cache, clear_cache, use_gurobi):
    if sufu_cache is None or clear_cache: sufu_cache = {}
    is_cover = False

    for task_path in tqdm(get_all_benchmark_rec(benchmark_root, lambda x: ".f" in x and "autolifter-base" not in x)):
        tmp = task_path[len(src_path):-2].split("/")
        tmp[0] = "incre-tests"
        name = "-".join(tmp)
        if name in sufu_cache: continue
        if name in failed_list:
            sufu_cache[name] = {"status": "fail"}
            print(sufu_cache[name])
            save_cache(cache_path, sufu_cache, is_cover)
            is_cover = True
            continue
        label_path = label_dir + name
        res_path = res_dir + name
        oup_path = oup_dir + name
        os.system("touch " + oup_path)

        command = ["timeout " + str(time_out), executor, "-benchmark=" + task_path, "-output=" + oup_path, "-use_gurobi=" + use_gurobi]
        command += ["2>/dev/null"]
        command = " ".join(command)
        print(command)
        os.system(command)

        with open(oup_path, "r") as inp:
            lines = inp.readlines()
        if len(lines) == 0 or "Success" not in lines[-1]:
            sufu_cache[name] = {"status": "fail"}
        else:
            sufu_cache[name] = {"status": "success", "time": float(lines[-2][:-1])}
        print(sufu_cache[name])
        save_cache(cache_path, sufu_cache, is_cover)
        is_cover = True
    print("\n")

def print_attr(sufu_cache, clear_cache, use_gurobi, is_cover):
    print("---calculate attribute in each batch---")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["num", "time", "align-size", "extract-size", "comb-size"]:
            print("batch: ", batch_name, ", attr: ", attr, ", value: ", get_all(sufu_cache, batch_name, attr, True), sep="")
    print("\n")
