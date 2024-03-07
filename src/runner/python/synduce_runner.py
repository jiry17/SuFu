from pprint import pprint
from cache import *
from tqdm import tqdm
from executor import get_all_benchmark_rec
import time

src_path = "/home/jiry/2023S/Synduce/"
benchmark_root = src_path + "benchmarks/"
executor = src_path + "Synduce"
run_dir = "/home/jiry/2023S/ISTool/runner/"
res_dir = run_dir + "res/synduce/"
oup_dir = run_dir + "oup/synducel/"
cache_path = run_dir + "synduce.json"
time_out = 600

cache = load_cache(cache_path)
is_cover = False


for name, result in cache.items():
    if result["status"] != "success":
        print(name, result["status"])
exit(0)

def get_status(lines):
    for line in lines:
        if "Solution found" in line: return "success"
        if "problem is unrealizable" in line: return "unrealizable"
    return "fail"

#cache = clearFail(cache)

for task_path in tqdm(get_all_benchmark_rec(benchmark_root, lambda x: ".ml" in x or ".pmrs" in x)):
    name = ".".join(task_path.split(".")[:-1])
    name = "-".join(name[len(src_path):].split("/"))
    if name in cache: continue
    res_path = res_dir + name
    os.system("touch " + res_path)

    command = ["timeout " + str(time_out), executor, task_path]
    command += [">", res_path, "2>/dev/null"]
    command = " ".join(command)
    print(command)
    starttime = time.time()
    os.system(command)
    endtime = time.time()

    with open(res_path, "r") as inp:
        lines = inp.readlines()
    ti = (endtime - starttime)
    cache[name] = {"status": get_status(lines), "time": float(endtime-starttime)}
    print(cache[name])
    save_cache(cache_path, cache, is_cover)
    is_cover = True

for name, result in cache.items():
    if result["status"] == "fail":
        print(name)