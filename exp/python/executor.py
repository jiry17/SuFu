import os
import subprocess
import sys
import time
from config import *
from cache import *
from tqdm import tqdm
import random


def _get_tmp_output_file(output_folder: str):
    while True:
        file_id = random.randint(0, 10 ** 9)
        output_file = os.path.join(output_folder, str(file_id) + ".out")
        if not os.path.exists(output_file):
            os.system("touch %s" % output_file)
            return output_file


def _read_all_lines(file):
    with open(file, "r") as inp:
        lines = inp.readlines()
    res = []
    for line in lines:
        while len(line) > 0 and (line[0] == ' ' or line[0] == '\n'): line = line[1:]
        while len(line) > 0 and (line[-1] == ' ' or line[-1] == '\n'): line = line[:-1]
        if len(line) > 0: res.append(line)
    return res


def _deal_thread(thread_pool, pos, cache):
    status = thread_pool[pos]["thread"].poll()
    if status is not None and status != 1:
        output_file = thread_pool[pos]["output_file"]
        name = thread_pool[pos]["name"]
        result = _read_all_lines(output_file)
        if name not in cache: cache[name] = []
        if len(result) == 0:
            cache[name].append({"status": False})
        else:
            res = result[:-1]
            time_cost = float(result[-1])
            cache[name].append({"status": True, "result": res, "time": time_cost})
        os.system("rm %s" % output_file)
        thread_pool[pos] = None


def _run_command(thread_pool, command, name, output_file, cache):
    pos = None
    while pos is None:
        for i in range(len(thread_pool)):
            if thread_pool[i] is not None:
                _deal_thread(thread_pool, i, cache)
            if thread_pool[i] is None:
                pos = i
        time.sleep(0.1)

    thread_pool[pos] = {
        "name": name,
        "output_file": output_file,
        "thread": subprocess.Popen(command, stdout=subprocess.PIPE, shell=True)
    }


def _join_all(thread_pool, cache):
    while True:
        is_clear = True
        for i in range(len(thread_pool)):
            if thread_pool[i] is not None:
                _deal_thread(thread_pool, i, cache)
                is_clear = False
        if is_clear: return


def _get_benchmark_name(path: str):
    return os.path.splitext(os.path.basename(path))[0]


def execute(config: RunnerConfig, benchmark_list: list, cache_path: str, thread_num=4,
            output_folder="/tmp/", save_step=5):
    thread_pool = [None for _ in range(thread_num)]
    cache = load_cache(cache_path)
    backup_cache(cache_path)
    if config.name not in cache:
        cache[config.name] = {}
    step_num = 0

    pre_list = benchmark_list
    benchmark_list = []
    for path in pre_list:
        benchmark_name = _get_benchmark_name(path)
        existing_num = 0 if benchmark_name not in cache[config.name] else len(cache[config.name][benchmark_name])
        benchmark_list.extend([path] * max(0, config.repeat_num - existing_num))

    for benchmark_file in tqdm(benchmark_list):
        benchmark_name = _get_benchmark_name(benchmark_file)
        output_file = _get_tmp_output_file(output_folder)
        command = config.build_command(benchmark_file, output_file)
        _run_command(thread_pool, command, benchmark_name, output_file, cache[config.name])

        step_num += 1
        if step_num % save_step == 0:
            save_cache(cache_path, cache, True)

    _join_all(thread_pool, cache[config.name])
    save_cache(cache_path, cache, True)
    return cache


def get_all_benchmark(path: str, valid = lambda f: ".f" in f):
    path_list = os.listdir(path)
    benchmark_list = []
    for file in path_list:
        if valid(file):
            benchmark_list.append(os.path.join(path, file))
    return benchmark_list

def get_all_benchmark_rec(root: str, valid = lambda f: ".f" in f):
    benchmark_list = []
    for path, _, files in os.walk(root):
        for file in files:
            if valid(file):
                benchmark_list.append(os.path.join(path, file))
    return benchmark_list

extract_info = {
  "autolifter": {'time': 0.026734187499999982},
  "fusion": {'time': 0.015470187500000001},
  "synduce": {'time': 0.018215449438202254},
  "total": {'time': 0.02088398275862069}
}

def get_attribute(cache, name, attr):
    if cache[name]["status"] != "success": return 0
    if attr == "num": return 1
    if attr == "time": return cache[name]["time"]
    sufu_oup_dir = run_dir + "oup/sufu/"
    oup = sufu_oup_dir + name
    with open(oup, "r") as inp:
        lines = inp.readlines()
    for line in lines[-15:]:
        if attr + ":" in line:
            l = line[:-1] if line[-1] == '\n' else line 
            return float(l.split(" ")[-1])
    print("warning: attribute", attr, "not found in", name)
    return 0

def _get_all(cache, batch_name, attr):
    total = 0
    ma = 0
    for name in cache.keys():
        if batch_name != "total" and batch_name not in name: continue
        if "dp" in name: continue
        total += get_attribute(cache, name, attr)
        ma = max(ma, get_attribute(cache, name, attr))
    # print(batch_name, attr, ma)
    return total 

def get_all(cache, batch_name, attr, flag):
    num = _get_all(cache, batch_name, "num")
    if attr == "num": return num
    if num == 0:
        return 0
    else:
        v = _get_all(cache, batch_name, attr) / num
    if attr == "time" and flag: v -= extract_info[batch_name]["time"]
    return v