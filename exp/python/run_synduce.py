from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec, get_all
import time

def get_status(lines):
    for line in lines:
        if "Solution found" in line: return "success"
        if "problem is unrealizable" in line: return "unrealizable"
    return "fail"

def run_synduce_tasks(synduce_cache, clear_cache, is_cover):
    if synduce_cache is None or clear_cache: synduce_cache = {}
    
    for task_path in tqdm(get_all_benchmark_rec(synduce_benchmark_root, lambda x: ".ml" in x or ".pmrs" in x)):
        name = ".".join(task_path.split(".")[:-1])
        name = "-".join(name[len(synduce_path):].split("/"))
        if name in synduce_cache: 
            continue
        synduce_res_path = synduce_res_dir + name
        os.system("touch " + synduce_res_path)

        command = ["timeout " + str(time_out), synduce_executor, task_path]
        command += [">", synduce_res_path, "2>/dev/null"]
        command = " ".join(command)
        print(command)
        starttime = time.time()
        os.system(command)
        endtime = time.time()

        with open(synduce_res_path, "r") as inp:
            lines = inp.readlines()
        ti = (endtime - starttime)
        synduce_cache[name] = {"status": get_status(lines), "time": float(endtime-starttime)}
        print(synduce_cache[name])
        save_cache(synduce_cache_path, synduce_cache, is_cover)
        is_cover = True

    print("\nfail tasks in synduce:")
    for name, result in synduce_cache.items():
        if result["status"] == "fail":
            print(name)
    print("\n")

def print_synduce_compare(sufu_cache, clear_cache, use_gurobi, is_cover):
    print("---compare with Synduce---")
    synduce_cache = load_cache(synduce_cache_path)
    is_cover = False
    if synduce_cache is None: print("None!")
    run_synduce_tasks(synduce_cache, clear_cache, is_cover)

    print("total task num:", len(synduce_cache))
    print("SuFu solved:", get_all(sufu_cache, "synduce", "num", True))
    print("Synduce solved:", get_all(synduce_cache, "total", "num", False))

    num, asum, ssum = 0, 0, 0
    for name in synduce_cache.keys():
        if synduce_cache[name]["status"] != "success": continue
        task_name = "incre-tests-synduce-" + "-".join(name.split("-")[1:])
        
        full_name = None
        for my_name in sufu_cache.keys():
            if task_name == my_name:
                assert full_name is None 
                full_name = my_name
                break
        assert full_name is not None 
        if sufu_cache[full_name]["status"] != "success": continue
        num += 1
        asum += sufu_cache[full_name]["time"]
        ssum += synduce_cache[name]["time"]

    print("the number of tasks solved by SuFu and Synduce:", num)
    print("average time of SuFu in both solved tasks:", asum / num)
    print("average time of Synduce in both solved tasks:", ssum / num)
    print("\n")