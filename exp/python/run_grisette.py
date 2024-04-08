from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
import pprint
import os
from executor import get_all_benchmark_rec, get_all, print_result
import subprocess

grisette_path = src_path + "thirdparty/Grisette/"
grisette_benchmark_root = grisette_path + "benchmark/"
grisette_execute_path = grisette_path + "src/"
grisette_execute_file = grisette_execute_path + "run_test/Main.hs"
grisette_program_name = "run_test/Main.hs"
grisette_cache_path = cache_dir + "grisette.json"

def run_grisette_tasks(grisette_cache, clear_cache, time_out):
    if grisette_cache is None or clear_cache: grisette_cache = {}
    is_cover = False

    for task_path in tqdm(get_all_benchmark_rec(grisette_benchmark_root, lambda x: ".hs" in x)):
        name = "incre-tests-" + "-".join(task_path[len(src_path):-3].split("/")[3:])
        if name in grisette_cache:
            continue
        mv_command = "cp " + task_path + " " + grisette_execute_file
        os.system(mv_command)

        commands = f":l {grisette_program_name}\nmain\n:q\n"
        
        try:
            result = subprocess.run(["stack", "ghci"], input=commands, capture_output=True, text=True, cwd=grisette_execute_path, timeout=time_out)
            result = result.stdout.split("\n")[-5:]
        except:
            result = []
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
        
        # os.system("killall z3")
        # kill z3
        z3_pid = subprocess.getoutput("pgrep z3")
        if z3_pid:
            subprocess.run(["kill", z3_pid], check=True)

        save_cache(grisette_cache_path, grisette_cache, is_cover)
        is_cover = True

def ave(total, num):
    if num == 0: return "N/A"
    return total / num

def get_source_name(name):
    if name == "fusion": return "Fusion"
    if name == "synduce": return "Recursion"
    if name == "autolifter": return "D&C"
    if name == "total": return "Total"
    return name

def print_grisette_compare(sufu_cache, clear_cache, time_out):
    grisette_cache = load_cache(grisette_cache_path)
    run_grisette_tasks(grisette_cache, clear_cache, time_out)
    grisette_cache = load_cache(grisette_cache_path)

    title = "Table 8. Comparison between SuFu and Grisette"

    data = {}
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        task_num, num, anum, snum, atime, stime = 0, 0, 0, 0, 0, 0
        for name in grisette_cache:
            if "incre-tests-" + batch_name not in name and batch_name != "total": continue
            if "incre-tests-dp" in name: continue
            task_num += 1
            if sufu_cache[name]["status"] == "success": 
                anum += 1
            if grisette_cache[name]["status"] == "success": 
                snum += 1
            if grisette_cache[name]["status"] != "success" or sufu_cache[name]["status"] != "success":
                continue
            num += 1
            atime += sufu_cache[name]["time"]
            stime += grisette_cache[name]["time"]
        data[batch_name] = [anum, ave(atime, num), snum, ave(stime, num)]
    
    content = [
        ["Source", "Approach", "#Solved", "Time"] * 2
    ]
    for (xsource, ysource) in [("fusion", "synduce"), ("autolifter", "total")]:
        content.append(
            [get_source_name(xsource), "SuFu", data[xsource][0], data[xsource][1]] + 
            [get_source_name(ysource), "SuFu", data[ysource][0], data[ysource][1]]
        )
        content.append(
            ["", "Grisette", data[xsource][2], data[xsource][3]] +
            ["", "Grisette", data[ysource][2], data[ysource][3]]
        )
    print_result(title, content)
