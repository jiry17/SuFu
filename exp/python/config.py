import os

src_path = "/home/jiry/zyw/AutoElim/artifact/SuFu" + "/"
benchmark_root = src_path + "benchmark"
run_dir = src_path + "exp/"
cache_dir = run_dir + "result_cache/"
time_out = 600

synduce_path = src_path + "baseline/Synduce/"
synduce_benchmark_root = synduce_path + "benchmarks/"
synduce_executor = synduce_path + "Synduce"
synduce_res_dir = run_dir + "res/synduce/"
synduce_oup_dir = run_dir + "oup/synduce/"
synduce_cache_path = cache_dir + "synduce.json"

grisette_path = src_path + "baseline/Grisette/"
grisette_benchmark_root = grisette_path + "benchmark/"
grisette_execute_path = grisette_path + "src/"
grisette_execute_file = grisette_execute_path + "run_test/Main.hs"
grisette_program_name = "run_test/Main.hs"
grisette_cache_path = cache_dir + "grisette.json"

def _get_bin_name(bin_file):
    return os.path.basename(bin_file)

class RunnerConfig:
    def __init__(self, bin_file: str, time_limit: int, memory_limit: int, flags=None, name=None, repeat_num=1, flag_generator=None):
        if flags is None: flags = []
        if name is None:
            name = _get_bin_name(bin_file)
            for flag in flags: name += "@" + flag
        self.bin_file = bin_file
        self.time_limit = time_limit
        self.memory_limit = memory_limit
        self.flags = flags
        self.name = name
        self.repeat_num = repeat_num
        self.flag_generator = flag_generator

    def build_command(self, task_file: str, result_file: str):
        command = ['ulimit -v ' + str(self.memory_limit * 1024 * 1024) + ';',
                   "timeout " + str(self.time_limit), self.bin_file,
                   task_file, result_file]
        command += self.flags + [">/dev/null", "2>/dev/null"]
        print(" ".join(command))
        return " ".join(command)
    