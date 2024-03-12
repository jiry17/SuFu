from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec, get_all
import time

synduce_path = src_path + "thirdparty/Synduce/"
synduce_benchmark_root = src_path + "thirdparty/Synduce-benchmarks/"
synduce_executor = synduce_path + "Synduce"
synduce_res_dir = run_dir + "res/synduce/"
# synduce_oup_dir = run_dir + "oup/synduce/"
synduce_cache_path = cache_dir + "synduce.json"

def get_status(lines):
    for line in lines:
        if "Solution found" in line: return "success"
        if "problem is unrealizable" in line: return "unrealizable"
    return "fail"

def run_synduce_tasks(synduce_cache, clear_cache, timeout):
    if synduce_cache is None or clear_cache: synduce_cache = {}
    is_cover = False
    
    for task_path in tqdm(get_all_benchmark_rec(synduce_benchmark_root, lambda x: ".ml" in x or ".pmrs" in x)):
        name = ".".join(task_path.split(".")[:-1])
        name = "-".join(name[len(synduce_path):].split("/"))
        if name in synduce_cache: 
            continue
        synduce_res_path = synduce_res_dir + name
        os.system("mkdir -p " + synduce_res_dir)
        os.system("touch " + synduce_res_path)

        command = ["timeout " + str(timeout), synduce_executor, task_path]
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

def ave(total, num):
    if num == 0: return "N/A"
    return total / num

def print_synduce_compare(sufu_cache, clear_cache, timeout):
    print("---compare with Synduce (RQ2)---")
    synduce_cache = load_cache(synduce_cache_path)
    run_synduce_tasks(synduce_cache, clear_cache, timeout)

    num, atime, stime = 0, 0, 0
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
        atime += sufu_cache[full_name]["time"]
        stime += synduce_cache[name]["time"]

    anum = get_all(sufu_cache, "synduce", "num", True)
    snum = get_all(synduce_cache, "total", "num", False)
    # print(anum, snum)
    print("(SuFu) #solved tasks:", anum,  "averege time:", ave(atime, num))
    print("(Synduce) #solved tasks:", snum,  "averege time:", ave(stime, num))
    print("\n")