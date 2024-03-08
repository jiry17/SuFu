# SuFu

Artifact for PLDI'24: Superfusion: Eliminating Intermediate Data Structures via Inductive Synthesis.

The updates of this project can be found on [GitHub](https://github.com/jiry17/SuFu).

### Install 

#### Build from source (Test on Ubuntu 20.04) 

1. Install dependencies

   ```bash
   $ apt-get update
   $ apt-get install cmake ninja-build git wget libgoogle-glog-dev python3-tk python3-pip libboost-all-dev libjsoncpp-dev libboost-all-dev libgoogle-perftools-dev
   $ pip install --upgrade pip==9.0.1
   $ pip3 install pyparsing z3-solver
   ```


2. Clone *SuFu* 

   ```bash
   $ git clone https://github.com/jiry17/SuFu
   ```


3. Build the whole project under the root directory of the project.

   ```bash
   $ cd SuFu
   $ ./install
   ```

4. *SuFu* can use *gurobi* as the underlying ILP solver. Therefore, if you want to use *gurobi*, then a license of *gurobi* is required. You can get an academic license of gurobi via the following steps. Otherwise, you can skip this part.

   1. Register or login at the [webside](https://www.gurobi.com/) of gurobi.
   2. Visit the [Free Academic License page](https://www.gurobi.com/downloads/end-user-license-agreement-academic/).
   3. Click ***I Accept These Conditions***.
   4. Get a command like  `grbgetkey x...x` at the bottom of the webpage.
   5. Replace `grbgetkey` with `gurobi912/linux64/bin/grbgetkey` and execute this command under the root directory of the project.
   6. Test whether the license works normally by executing `gurobi912/linux64/bin/gurobi.sh` under the root directory of the project. 

#### Run tests

1. Test whether the project is successfully built:

   ```bash
   $ cd build
   $ executor/run
   ```

   The last line of output in command line should be "Success". And the optimized program is in file `build/res.f`.

### Run synthesizers

#### Run synthesizers on a single SyGuS file 

```bash
$ cd build
# Run SuFu
$ executor/run INPUT OUTPUT USE_GUROBI
```

Some examples are listed below:

```bash
$ cd build
# Run SuFu for benchmark autolifter/single-pass/mts.f, output the result into "build/res.f" and don't use gurobi as underlying ILP solver.
$ executor/run -benchmark="benchmark/autolifter/single-pass/mts.f" -output="build/res.f" -use_gurobi=false
```

#### Run experiments 

```bash
$ cd exp
$ ./run_exp  [-exp {1,2}]  [-c {R <Restart>,C <Clear>}] [-s {0,1}]
# For example, to reproduce all results:
$ ./run_exp -c R
```

1. `-exp`: the id of the experiment you want to run. All experiments will be executed by default.
2. `-c`: whether to clear the cache: `R` represents yes while `C` represents no, and the default value is `C`. 
3. `-s`: whether to skip random test when the same benchmark fails in cegis test:  `1` represents yes while  `0`  represents no. The default value is `1`. 
4. `-d`: whether to draw the figures: `1` represents yes while `0` represents no. The default value is `0`.

Some parameters can be set in config.py

1. `KMemoryLimit`: the memory limit. The default value is 8 GB
2. `KTimeLimit`: the time limit. The default value is 120 seconds.
3. `KIntMin `: lower bound of the input. The default value is -20.
4. `KIntMax `: upper bound of the input. The default value is 20.
5. `KExampleLimit `: The limit of examples involves in the synthesizing process. The default value is 10000.
6. `KRepeatNum `: the number of repetitions of each execution. The default value is 1 for efficiency. Note that all the algorithms are random, the smaller this value is, the more volatile the result will be. Our experiment set this value as 5.

The result of each single execution is cached in `exp/result_cache` . 

The figure of each experiment will be stored in `exp/figure`.

These results are expected to be consistent with results presented in `run`.

### Reproduce results in the paper 

#### Reproduce results of experiment 1

```bash
$ cd exp
$ ./run_exp -exp 1
```

For (a) ~ (d) of Figure 2, the script will redraw them respectively.

For data listed in Table 2, `run_exp` will recalculate them and print them to the standard output.

#### Reproduce results of experiment 2

````bash
$ cd exp
$ ./run_exp -exp 2
````

For (e) ~ (h) of Figure 2, the script will redraw them respectively.

For data listed in Table 3, `run_exp` will recalculate them and print them to the standard output.

**Note**: If you don't use *gurobi*, there may be some differences between the results listed in our paper and the reproduced ones because of the performance differences between ILP solvers. However, this does not affect the significance of our experimental results. If you use *gurobi*, there can also be some small differences due to randomness.