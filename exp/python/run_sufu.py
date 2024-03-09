from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec, get_all

executor = src_path + "build/executor/run"
res_dir = run_dir + "res/autolabel/"
oup_dir = run_dir + "oup/autolabel/"
label_dir = run_dir + "label/"
sufu_cache_path = cache_dir + "autolabel.json"

failed_list = ["incre-tests-fusion-deforestation-page7-1", 
"incre-tests-fusion-algprog-page62",
"incre-tests-dp-15-11",
"incre-tests-dp-15-4",
"incre-tests-dp-15-3",
"incre-tests-synduce-treepaths-mips",
"incre-tests-synduce-treepaths-leftmostodd",
"incre-tests-synduce-constraints-ensures-bal_2",
"incre-tests-synduce-tree-poly",
"incre-tests-synduce-tree-poly2",
"incre-tests-synduce-list-bal",
"incre-tests-synduce-list-polyhom",
"incre-tests-synduce-ptree-maxsum",
"incre-tests-autolifter-lsp-page22-2",
"incre-tests-autolifter-dac-longest10s2",
"incre-tests-autolifter-dac-lis",
"incre-tests-autolifter-dac-max_sum_between_ones",
"incre-tests-autolifter-dac-msp",
"incre-tests-autolifter-dac-count10s2",
"incre-tests-autolifter-dac-atoi",
"incre-tests-autolifter-dac-longest_odd10s",
"incre-tests-autolifter-dac-largest_peak",
"incre-tests-autolifter-single-pass-mmm",
"incre-tests-autolifter-single-pass-longest10s2",
"incre-tests-autolifter-single-pass-lis",
"incre-tests-autolifter-single-pass-max_sum_between_ones",
"incre-tests-autolifter-single-pass-mas",
"incre-tests-autolifter-single-pass-longest_odd10s",
"incre-tests-autolifter-single-pass-largest_peak"]

def clearFail(cache):
    new_cache = {}
    for name, status in cache.items():
        if status["status"] == "success" or name in failed_list:
            new_cache[name] = status
    return new_cache

def run_sufu_tasks(sufu_cache, clear_cache, use_gurobi, is_cover):
    sufu_cache = clearFail(sufu_cache)
    if sufu_cache is None or clear_cache: sufu_cache = {}

    for task_path in tqdm(get_all_benchmark_rec(benchmark_root, lambda x: ".f" in x and "autolifter-base" not in x)):
        tmp = task_path[len(src_path):-2].split("/")
        tmp[0] = "incre-tests"
        name = "-".join(tmp)
        if name in sufu_cache: continue
        if name in failed_list:
            sufu_cache[name] = {"status": "fail"}
            print(sufu_cache[name])
            save_cache(cache_path, sufu_cache, is_cover)
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

def print_sufu_fail_task(sufu_cache):
    # print fail tasks
    print("---fail tasks---")
    for name, result in sufu_cache.items():
        if result["status"] == "fail":
            print("fail", name)
    print("\n")

def print_attr(sufu_cache, clear_cache, use_gurobi, is_cover):
    print("---calculate attribute in each batch---")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["num", "time", "align-size", "extract-size", "comb-size"]:
            print("batch: ", batch_name, ", attr: ", attr, ", value: ", get_all(sufu_cache, batch_name, attr, True), sep="")
    print("\n")
