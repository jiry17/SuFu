import os 
from tqdm import tqdm
from executor import get_all_benchmark_rec
from pprint import pprint
import random

src_path = "/home/jiry/zyw/AutoElim/artifact/SuFu/src/"
executor = src_path + "cmake-build-debug-remote-host/executor/run_status"
benchmark_root = "/home/jiry/zyw/AutoElim/artifact/SuFu/benchmark"
oup_dir = src_path + "runner/oup/status/"
print(benchmark_root)
benchmarks = get_all_benchmark_rec(benchmark_root, lambda x: ".f" in x and "autolifter-base" not in x)

print(len(benchmarks))
cared_names = ["size", "compress-num", "label-num", "rewrite-size"]
category = ["incre-tests/autolifter", "incre-tests/dp", "incre-tests/fusion", "incre-tests/synduce"]
vals = {}

def _insert(cate, name, val):
    if cate not in vals: vals[cate] = {}
    if name not in vals[cate]: vals[cate][name] = 0
    vals[cate][name] += val

def insert(cate, name, val):
    _insert(cate, name, val)
    _insert("total", name, val)

def getcate(path):
    for cate in category:
        if cate in path: return cate 
    assert False

for task_path in benchmarks:
    name = "-".join(task_path[len(src_path):-2].split("/"))
    if "zyw" in name or "autolifter_base" in name: continue
    oup_path = oup_dir + name
    command = [executor, task_path]
    command += [">", oup_path, "2>/dev/null"]
    command = " ".join(command)
    #print(command)
    os.system(command)

    cate = getcate(task_path)
    if cate == "incre-tests/dp": continue

    with open(oup_path, "r") as inp:
        lines = inp.readlines()
    
    #pprint(lines)

    insert(cate, "num", 1)
    insert(cate, "time", float(lines[-1]))
    for line in lines[-5: -1]:
        items = line[:-1].split(" ")
        if items[0][:-1] == "compress-num" and int(items[1]) > 1:
            print(task_path, int(items[1]))
        insert(cate, items[0][:-1], int(items[1]))
        #print(line)
    #if random.randint(0, 3) == 0: break

for cate in vals.keys():
    num = vals[cate]["num"]
    for name in vals[cate].keys():
        if name == "num": continue
        vals[cate][name] /= num 
pprint(vals)