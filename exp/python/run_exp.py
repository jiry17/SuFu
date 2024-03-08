from pprint import pprint
from cache import *
from tqdm import tqdm
from executor import get_all_benchmark_rec
from runner import *
import argparse



def clearFail(cache):
    new_cache = {}
    for name, status in cache.items():
        if status["status"] == "success" or name in failed_list:
            new_cache[name] = status
    return new_cache


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('-exp', '--experiment', type=str, default="total", choices=["attribute", "synduce", "autolifter", "sketch", "total"])
    parser.add_argument('-c', '--cache', type=str, default="Continue", choices=["Restart", "Continue", "R", "C"])
    # parser.add_argument('-d', '--draw', type=int, choices=[0,1], default=0)
    return parser.parse_args()


if __name__ == "__main__":
    if not os.path.exists("result_cache"):
        os.system("mkdir result_cache")

    # get args
    args = parse_args()
    if args.cache == "Restart" or args.cache == "R": clear_cache = True
    else: clear_cache = False

    # get result of SuFu
    cache = load_cache(cache_path)
    cache = clearFail(cache)

    # print fail tasks
    print("---fail tasks---")
    for name, result in cache.items():
        if result["status"] == "fail":
            print("fail", name)
    print("\n")

    # print compare result
    is_cover = False
    if (args.experiment == "attribute" or args.experiment == "total"):
        print_attr(cache, clear_cache, is_cover)
    if (args.experiment == "synduce" or args.experiment == "total"):
        print_synduce_compare(cache, clear_cache, is_cover)
    if (args.experiment == "autolifter" or args.experiment == "total"):
        print_autolifter_compare(cache, clear_cache, is_cover)
    if (args.experiment == "sketch" or args.experiment == "total"):
        print_sketch_compare(cache, clear_cache, is_cover)
