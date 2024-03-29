import os

src_path = SUFUPATH + "/"
benchmark_root = src_path + "benchmark"
run_dir = src_path + "exp/"
cache_dir = run_dir + "result_cache/"
# time_out = 600

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
        return " ".join(command)
    