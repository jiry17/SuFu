from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec

def print_autolifter_compare(cache, clear_cache):
    print("---compare with AutoLifter---")
    case_name = {"dad": "dac", "listr": "single-pass", "seg": "lsp", "ds": "segment-tree"}
    autolifter_res = load_cache(cache_dir + "AutoLifter.json")

    if autolifter_res == False: 

    num, atime, btime = 0, 0,0     
    for case, case_result in autolifter_res.items():
        for task_name, task_result in case_result.items():
            full_name = "incre-tests-autolifter-" + case_name[case] + "-" + task_name
            full_name = full_name.replace("@", "-").replace("+", "plus")
            if full_name[-1] == "-": full_name = full_name[:-1] + "neg"
            assert full_name in cache 
            if task_result["status"] != "success" or cache[full_name]["status"] != "success": continue
            num += 1
            atime += cache[full_name]["time"]
            btime += task_result["time"]["total"]
    
    print("the number of tasks solved by SuFu and AutoLifter:", num)
    print("average time of SuFu:", atime / num)
    print("average time of AutoLifter:", btime / num)
    print("\n")