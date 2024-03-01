from enum import Enum
import math
import matplotlib.pyplot as plt


class MergeType(Enum):
    LEN = 1
    SUM = 2
    AVE = 3
    GEO = 4


def _merge(data, merge_type):
    if merge_type == MergeType.LEN:
        return len(data)
    if merge_type == MergeType.SUM:
        return sum(data)
    if merge_type == MergeType.AVE:
        if len(data) == 0: return -1
        return sum(data) / len(data)
    if merge_type == MergeType.GEO:
        if len(data) == 0 or min(data) <= 0: return -1
        log_data = list(map(lambda x: math.log(x), data))
        return math.exp(sum(log_data) / len(log_data))
    assert False


class DrawType(Enum):
    LINEAR = 0
    LOG = 1


class CaredValue:
    def __init__(self, f, name, merge_type=MergeType.LEN, draw_type=DrawType.LOG):
        self.f = f
        self.name = name
        self.merge_type = merge_type
        self.draw_type = draw_type


def _print_result(row_names, col_names, result, number_size = 6):
    assert len(row_names) == len(result)
    for res in result: assert len(col_names) == len(res)

    row_size = 0
    for row_name in row_names:
        row_size = max(row_size, len(row_name) + 1)

    # print head
    print("%*s " % (row_size, ""), end="")
    for col_name in col_names:
        print("| %*s " % (max(len(col_name), number_size), col_name), end="")
    print()

    #print contents
    for i in range(len(row_names)):
        print("%*s " % (row_size, row_names[i]), end="")
        for j in range(len(col_names)):
            val = result[i][j]
            size = max(len(col_names[j]), number_size)
            if type(val) == int:
                print("| %*d " % (size, val), end="")
            elif type(val) == float:
                print("| %*.2f " % (size, val), end="")
        print()


def _is_success(res_item):
    for item in res_item:
        if item["status"]: return True
    return False


def _apply_f(cared_val: CaredValue, res_item):
    res = []
    for item in res_item:
        if item["status"]:
            res.append(cared_val.f(item))
    assert len(res) > 0
    return sum(res) / len(res)


def _calculate_all(cared_val: CaredValue, result):
    data = {}
    for name, res in result.items():
        if _is_success(res):
            data[name] = _apply_f(cared_val, res)
    return data


def _calculate(cared_val: CaredValue, result):
    res = _calculate_all(cared_val, result)
    res = list(res.values())
    return _merge(res, cared_val.merge_type)


def _calculate_cmp(cared_val: CaredValue, result1, result2):
    data1 = _calculate_all(cared_val, result1)
    data2 = _calculate_all(cared_val, result2)
    ratios = []
    for name in data1:
        if name not in data2: continue
        ratios.append(data1[name] / data2[name])
    return _merge(ratios, MergeType.GEO)


def compare(name1, name2, cared_val_list, result):
    assert name1 in result and name2 in result
    row_names = [name1, name2, "cmp"]
    col_names = [v.name for v in cared_val_list]
    summary = [[_calculate(v, result[name1]) for v in cared_val_list],
               [_calculate(v, result[name2]) for v in cared_val_list],
               [_calculate_cmp(v, result[name1], result[name2]) for v in cared_val_list]]
    _print_result(row_names, col_names, summary)


def _set_axis(draw_type: DrawType):
    if draw_type == DrawType.LINEAR:
        return
    if draw_type == DrawType.LOG:
        plt.xscale('log')
        return


def list_tasks(result_map, prop=lambda x: not x["status"]):
    num = 0
    for name, res in result_map.items():
        for item in res:
            if prop(item):
                print(name)
                num += 1
                break
    print(num)

def get_all_solved(result_map):
    failed_set = {}
    for solver, result in result_map.items():
        for name, item in result.items():
            if not _is_success(item):
                failed_set[name] = 1
    new_result_map = {}
    for solver, result in result_map.items():
        new_result = {}
        for name, item in result.items():
            if name not in failed_set: new_result[name] = item
        new_result_map[solver] = new_result
    return new_result_map

def list_all_result(result_map, val: CaredValue):
    row_list = {}
    for solver_name, res in result_map.items():
        for benchmark_name in res.keys():
            row_list[benchmark_name] = 1
    row_list = sorted(list(row_list.keys()))
    col_list = sorted(list(result_map.keys()))

    summarized_result = {}
    for solver_name, res in result_map.items():
        summarized_result[solver_name] = _calculate_all(val, res)
    print(summarized_result)

    all_res = []
    for row_name in row_list:
        res = []
        for col_name in col_list:
            print(row_name, col_name)
            if row_name not in summarized_result[col_name]: 
                res.append(-1)
                continue
            res.append(summarized_result[col_name][row_name])
        all_res.append(res)
    _print_result(row_list, col_list, all_res, 4)
    

def draw_trend(result_map, val: CaredValue, fig_path, x_size=4, y_size=3, is_x_name=True,
         is_y_name=True, title=None, is_complete_x=True):
    ls_list = ['-', '--', '-.', '--', '-.']
    lc = ['cornflowerblue', 'indianred', 'orange', 'forestgreen', 'violet']
    pc = ['mediumblue', 'darkred', 'peru', 'darkgreen', 'indigo']
    plt.figure(figsize=(x_size, y_size))
    if is_x_name:
        plt.xlabel(val.name)
    if is_y_name:
        plt.ylabel("#benchmark")
    if title is not None:
        plt.title(title)
    _set_axis(val.draw_type)
    draw_id = 0
    x_limit = 0
    y_limit = 0

    for _, result in result_map.items():
        data = list(_calculate_all(val, result).values())
        x_limit = max(x_limit, max(data))
        y_limit = max(y_limit, len(data))

    plt.ylim(0, y_limit)

    name_list = []
    for solver, result in result_map.items():
        name_list.append(solver)
        data = list(_calculate_all(val, result).values())
        x_list = sorted(data) + [x_limit]
        y_list = list(range(1, len(x_list))) + [len(x_list) - 1]
        if draw_id < len(lc):
            plt.plot(x_list, y_list, ls=ls_list[draw_id], color=lc[draw_id])
            draw_id += 1
        else:
            plt.plot(x_list, y_list)
    plt.legend(name_list)
    plt.tight_layout()
    plt.savefig(fig_path)