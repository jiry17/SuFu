from pprint import pprint
from cache import *
from tqdm import tqdm
from executor import get_all_benchmark_rec

src_path = "SUFUPATH" + "/"
benchmark_root = src_path + "benchmark"
executor = src_path + "src/build/executor/run"
run_dir = src_path + "exp/"
res_dir = run_dir + "res/autolabel/"
oup_dir = run_dir + "oup/autolabel/"
label_dir = run_dir + "label/"
cache_dir = run_dir + "result_cache/"
cache_path = cache_dir + "autolabel.json"
time_out = 1200

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

def get_attribute(cache, name, attr):
    if cache[name]["status"] != "success": return 0
    if attr == "num": return 1
    if attr == "time": return cache[name]["time"]
    oup = oup_dir + name
    with open(oup, "r") as inp:
        lines = inp.readlines()
    for line in lines[-15:]:
        if attr + ":" in line:
            l = line[:-1] if line[-1] == '\n' else line 
            return float(l.split(" ")[-1])
    print("attribute", attr, "not found in", name)
    assert False

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

extract_info = {
  "autolifter": {'time': 0.026734187499999982},
  "fusion": {'time': 0.015470187500000001},
  "synduce": {'time': 0.018215449438202254},
  "total": {'time': 0.02088398275862069}
}

def get_all(cache, batch_name, attr, flag):
    num = _get_all(cache, batch_name, "num")
    if attr == "num": return num
    v = _get_all(cache, batch_name, attr) / num
    if attr == "time" and flag: v -= extract_info[batch_name]["time"]
    return v

def run_all_tasks(cache, is_cover):
    for task_path in tqdm(get_all_benchmark_rec(benchmark_root, lambda x: ".f" in x and "autolifter-base" not in x)):
        tmp = task_path[len(src_path):-2].split("/")
        tmp[0] = "incre-tests"
        name = "-".join(tmp)
        if name in cache: continue
        if name in failed_list:
            cache[name] = {"status": "fail"}
            print(cache[name])
            save_cache(cache_path, cache, is_cover)
            continue
        label_path = label_dir + name
        res_path = res_dir + name
        oup_path = oup_dir + name
        os.system("touch " + oup_path)

        command = [executor, "-incre-tests=" + task_path, "-output=" + oup_path]
        command += ["2>/dev/null"]
        command = " ".join(command)
        print(command)
        os.system(command)

        with open(oup_path, "r") as inp:
            lines = inp.readlines()
        if len(lines) == 0 or "Success" not in lines[-1]:
            cache[name] = {"status": "fail"}
        else:
            cache[name] = {"status": "success", "time": float(lines[-2][:-1])}
        print(cache[name])
        save_cache(cache_path, cache, is_cover)
        is_cover = True
    print("\n")

def print_attr(cache, clear_cache, is_cover):
    if cache is None or clear_cache: cache = {}
    run_all_tasks(cache, is_cover)

    print("---calculate attribute in each batch---")
    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        for attr in ["num", "time", "align-size", "extract-size", "comb-size"]:
            print("batch: ", batch_name, ", attr: ", attr, ", value: ", get_all(cache, batch_name, attr, True), sep="")
    print("\n")


def print_synduce_compare(cache, clear_cache, is_cover):
    if cache is None or clear_cache: cache = {}
    run_all_tasks(cache, is_cover)

    print("---compare with Synduce---")
    synduce_res = load_cache(cache_dir + "synduce.json")
    print("total task num:", len(synduce_res))
    print("SuFu solved:", get_all(cache, "synduce", "num", True))
    print("Synduce solved:", get_all(synduce_res, "total", "num", False))

    num, asum, ssum = 0, 0, 0
    for name in synduce_res.keys():
        if synduce_res[name]["status"] != "success": continue
        task_name = "incre-tests-synduce-" + "-".join(name.split("-")[1:])
        
        full_name = None
        for my_name in cache.keys():
            if task_name == my_name:
                assert full_name is None 
                full_name = my_name
                break
        assert full_name is not None 
        if cache[full_name]["status"] != "success": continue
        num += 1
        asum += cache[full_name]["time"]
        ssum += synduce_res[name]["time"]

    print("the number of tasks solved by SuFu and Synduce:", num)
    print("average time of SuFu in both solved tasks:", asum / num)
    print("average time of Synduce in both solved tasks:", ssum / num)
    print("\n")

def print_autolifter_compare(cache, clear_cache, is_cover):
    if cache is None or clear_cache: cache = {}
    run_all_tasks(cache, is_cover)

    print("---compare with AutoLifter---")
    case_name = {"dad": "dac", "listr": "single-pass", "seg": "lsp", "ds": "segment-tree"}
    autolifter_res = load_cache(cache_dir + "AutoLifter.json")

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

def print_sketch_compare(cache, clear_cache, is_cover):
    if cache is None or clear_cache: cache = {}
    run_all_tasks(cache, is_cover)

    print("---compare with Grisette---")
    sketch_res = load_cache(cache_dir + "sketch.json")

    for batch_name in ["fusion", "synduce", "autolifter", "total"]:
        num, snum, atime, stime = 0, 0, 0, 0
        for name in sketch_res:
            if "incre-tests-" + batch_name not in name and batch_name != "total": continue
            if "incre-tests-dp" in name: continue
            if sketch_res[name]["status"] == "success": 
                #print(name)
                snum += 1
            if sketch_res[name]["status"] != "success" or cache[name]["status"] != "success":
                continue
            num += 1
            atime += cache[name]["time"]
            stime += sketch_res[name]["time"]
        print("batch: ", batch_name, ", sketch-num: ", snum, ", average time of SuFu: ", atime / num, ", average time of Grisette: ", stime / num, sep="")