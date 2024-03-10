from pprint import pprint
from cache import *
from config import *
from tqdm import tqdm
from executor import get_all_benchmark_rec
from run_sufu import sufu_cache_path, run_sufu_tasks, print_attr
from run_autolifter import print_autolifter_compare
from run_grisette import print_grisette_compare
from run_synduce import print_synduce_compare
import argparse

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-exp', '--experiment', type=str, default="total", choices=["attribute", "synduce", "autolifter", "grisette", "total"])
    parser.add_argument('-c', '--cache', type=str, default="Continue", choices=["Restart", "Continue", "R", "C"])
    parser.add_argument('-g', '--use_gurobi', type=str, default="false", choices=["false", "true"])
    # parser.add_argument('-d', '--draw', type=int, choices=[0,1], default=0)
    return parser.parse_args()


if __name__ == "__main__":
    if not os.path.exists("result_cache"):
        os.system("mkdir result_cache")

    # get args
    args = parse_args()
    if args.cache == "Restart" or args.cache == "R": clear_cache = True
    else: clear_cache = False
    use_gurobi = args.use_gurobi

    # get result of SuFu
    sufu_cache = load_cache(sufu_cache_path)
    run_sufu_tasks(sufu_cache, clear_cache, use_gurobi)

    # print compare result
    if (args.experiment == "attribute" or args.experiment == "total"):
        print_attr(sufu_cache, clear_cache)
    if (args.experiment == "synduce" or args.experiment == "total"):
        print_synduce_compare(sufu_cache, clear_cache)
    if (args.experiment == "autolifter" or args.experiment == "total"):
        print_autolifter_compare(sufu_cache, clear_cache)
    if (args.experiment == "grisette" or args.experiment == "total"):
        print_grisette_compare(sufu_cache, clear_cache)
