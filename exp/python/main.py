from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec, print_result
from run_sufu import sufu_cache_path, run_sufu_tasks, print_attr
from run_autolifter import build_autolifter_compare
from run_grisette import print_grisette_compare
from run_synduce import build_synduce_compare
import argparse

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-exp', '--experiment', type=str, default="total", choices=["rq1", "rq2", "rq3", "total"])
    parser.add_argument('-c', '--cache', type=str, default="Continue", choices=["Restart", "Continue", "R", "C"])
    parser.add_argument('-g', '--use_gurobi', type=str, default="false", choices=["false", "true"])
    parser.add_argument('-t', '--time_out', type=int, default=600)
    # parser.add_argument('-d', '--draw', type=int, choices=[0,1], default=0)
    return parser.parse_args()


if __name__ == "__main__":
    # get args
    args = parse_args()
    if args.cache == "Restart" or args.cache == "R": clear_cache = True
    else: clear_cache = False
    use_gurobi = args.use_gurobi
    time_out = args.time_out

    # get result of SuFu
    sufu_cache = load_cache(sufu_cache_path)
    run_sufu_tasks(sufu_cache, clear_cache, use_gurobi, time_out)
    sufu_cache = load_cache(sufu_cache_path)

    # print compare result
    if (args.experiment == "rq1" or args.experiment == "total"):
        print_attr(sufu_cache, clear_cache)
        
    if (args.experiment == "rq2" or args.experiment == "total"):
        synduce_table = build_synduce_compare(sufu_cache, clear_cache, time_out)
        autolifter_table = build_autolifter_compare(sufu_cache, clear_cache, time_out)

        title = "Table 7. Comparison between SuFu and program restructuring tools."
        merged_table = []
        for (line1, line2) in zip(synduce_table, autolifter_table):
            merged_table.append(line1 + line2)
        print_result(title, merged_table)

    if (args.experiment == "rq3" or args.experiment == "total"):
        print_grisette_compare(sufu_cache, clear_cache, time_out)
