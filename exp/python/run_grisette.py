from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
import pprint
import os
from executor import get_all_benchmark_rec, get_all
import subprocess

def run_grisette_tasks(grisette_cache, clear_cache, is_cover):
    if grisette_cache is None or clear_cache: grisette_cache = {}

    for task_path in tqdm(get_all_benchmark_rec(grisette_benchmark_root, lambda x: ".hs" in x)):
        name = "incre-tests-" + "-".join(task_path[len(src_path):-3].split("/")[3:])
        if name in grisette_cache:
            continue
        mv_command = "cp " + task_path + " " + grisette_execute_file
        print(mv_command)
        os.system(mv_command)

        commands = f":l {grisette_program_name}\nmain\n:q\n"
        
        result = subprocess.run(["stack", "ghci"], input=commands, capture_output=True, text=True, cwd=grisette_execute_path)
        result = result.stdout.split("\n")[-5:]
        pprint.pprint(result)
        status, ti = None, None
        for line in result:
            if "success!" in line:
                assert status is None 
                status = "success"
            if "Timeout occured" in line:
                assert status is None 
                statuc = "timeout"
            if "Time:" in line and "seconds" in line:
                assert ti is None 
                ti = float(line.split(" ")[1][:-1])
        if status is None: status = "failed"
        if status == "success":
            assert ti is not None 
            grisette_cache[name] = {"status": "success", "time": ti}
        else:
            grisette_cache[name] = {"status": status}
        os.system("killall z3")

        print(grisette_cache[name])
        save_cache(grisette_cache_path, grisette_cache, is_cover)
        is_cover = True


def print_grisette_compare(sufu_cache, clear_cache, use_gurobi, is_cover):
    print("---compare with Grisette---")
    grisette_cache = load_cache(grisette_cache_path)
    is_cover = False
    if grisette_cache is None: print("None!")
    run_grisette_tasks(grisette_cache, clear_cache, is_cover)

    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        num, snum, atime, stime = 0, 0, 0, 0
        for name in grisette_cache:
            if "incre-tests-" + batch_name not in name and batch_name != "total": continue
            if "incre-tests-dp" in name: continue
            if grisette_cache[name]["status"] == "success": 
                #print(name)
                snum += 1
            if grisette_cache[name]["status"] != "success" or sufu_cache[name]["status"] != "success":
                continue
            num += 1
            atime += sufu_cache[name]["time"]
            stime += grisette_cache[name]["time"]
        print("batch: ", batch_name, ", sketch-num: ", snum, ", average time of SuFu: ", atime / num, ", average time of Grisette: ", stime / num, sep="")
