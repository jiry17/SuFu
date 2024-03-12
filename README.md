# SuFu

Artifact for PLDI'24: Superfusion: Eliminating Intermediate Data Structures via Inductive Synthesis.

The updates of this project can be found on [GitHub](https://github.com/jiry17/SuFu).

### Install 

You can either build *SuFu* from the source or use it in a docker container.

#### Build from source (Tested on Ubuntu 20.04) 

1. Install dependencies. This project requires gcc $\geq 9$, CMake $\geq 3.13$, opam (tested on version 2.1.3 and switch ocaml.4.10.0), and python3. The other dependencies can be installed as follows.

   ```bash
   $ apt-get install libjsoncpp-dev libgoogle-glog-dev libgflags-dev pkg-config
   $ pip3 install tqdm argparse
   $ opam install ocamlfind yojson
   ```


2. Clone *SuFu* 

   ```bash
   $ git clone https://github.com/jiry17/SuFu
   ```


3. Build the whole project under the root directory of the project.

   ```bash
   $ cd SuFu; ./install
   ```

4. (optional) *SuFu* will perform better when *Gurobi*, a commercial constraint solver, is available. You can get an academic license of *gurobi* via the following steps. 

   1. Register or login at the [webside](https://www.gurobi.com/) of *Gurobi*.
   2. Visit the [Free Academic License page](https://www.gurobi.com/downloads/end-user-license-agreement-academic/) and follow the instructions on it.
   3. Get a command like  `grbgetkey x...x` at the bottom of the webpage.
   4. Execute this command under directory `SuFu/thirdparty/gurobi912/linux64/bin/`.
   5. Test your license by running `gurobi.sh` under the same directory as Step 4.

#### Download docker image

We also released a docker image where SuFu is already built at `~/SuFu`. You can download this image as follows.

```bash
$ docker pull takanashirikka/sufu
```

Note that *Gurobi* is unavailable in this docker image because the adademic license of *Gurobi* cannot be used on virtual machines. Therefore, we still recommend building this project from source.

#### Run tests

1. Test whether the project is successfully built:

   ```bash
   $ build/executor/run --benchmark=benchmark/autolifter/single-pass/sum.f --output=res.f --use_gurobi=false
   ```

   The last line of the command-line output shoud be `Success`, and there should be an optimized program in file `res.f`.

2. Test whether *SuFu* works normally with *gurobi*. Note that the docker version must fail on this task because it does not have a *gurobi* license.

   ```bash
   $ build/executor/run --benchmark=benchmark/autolifter/single-pass/sum.f --output=res.f --use_gurobi=true
   ```

   Similar to the previous test, the last line of the command-line output should be `Success`, and there should be an optimized program in file `res.f`.

### Run *SuFu* on a Single Task

You can run *SuFu* using the binary file `build/executor/run`.

```bash
$ build/executor/run --benchmark=BENCHMARK --output=OUTPUT --use_gurobi={true, false}
```

1. `--benchmark`: the file of the reference program.
2. `--output`: the output file of the optimized program.
3. `--gurobi`: whether use *Gurobi* as the underlying constraint solver.

For example, the following command runs *SuFu* (without *Gurobi*) to optimize a reference program in file `benchmark/autolifter/single-pass/mts.f` and stores the optimized program to file `res.f`.

```bash
build/executor/run --benchmark=benchmark/autolifter/single-pass/mts.f --output=res.f --use_gurobi=false
```

### Reproduce Results in Our Paper

We provide a script `exp/python/main.py` to reproduce the results in our paper. Its usage is as follows.

```bash
$ cd exp/python
$ python3 main.py [-exp {attribute,synduce,autolifter,grisette,total}]
                  [-c {Restart,Continue,R,C}] [-g {false,true}]
# For example, calculate the statistices in our paper using the cached results
$ python3 main.py -exp total -c C
```

1. `-exp`:  the name of the experiment, where `attribute` evaluates *SuFu* and summarizes the performance, `synduce` / `autolifter` / `grisette` compares *SuFu* with each baseline, and `total` denotes all experiments.
2. `-c`: whether to clear the cached results: `R` represents yes while `C` represents no. All cached results can be found in `exp/result_cache`.

For the case of building from source, you need to install the baseline solvers (in directory `thirdparty`) before re-running the comparison with baseline solvers. In our docker image, we have installed all baseline solvers such that you can directly re-run the comparisons with *Synduce* and *Grisette*.

**Note:** The baseline solver *AutoLifter* requires *Gurobi* as an underlying solver. Therefore, the comparison with *AutoLifter* requires a *Gurobi* license and cannot be reproduced in docker.

#### Install *AutoLifter*

1. Install the bounded verifier *cbmc* as follows.

   ```bash
   $ apt-get install cbmc
   ```

2. Clone *AutoLifter* and build it from source.

   ```bash
   $ cd thirdparty
   $ git clone https://github.com/jiry17/AutoLifter.git
   $ cd AutoLifter; ./install
   ```

3. Test whether *AutoLifter* is successfully built.

   ```bash
   $ cd thirdparty/AutoLifter/run
   $ ./run -p dac -t sum --solver Relish
   ```

   The last few lines of the command-line output should be `Success` followed by some synthesis results and some statistics.

#### Install *Synduce*

1. Install *Synduce* (in directory `thirdparty`) following the instructions in its [repository](https://github.com/synduce/Synduce). 

2. Test whether *Synduce* is successfully built for our usage. 

   ```bash
   $ cd thirdparty/Synduce
   $ ./Synduce benchmarks/combine/mts.ml
   ```

   The last few lines of the command-line outputs should be `Solution found in …` followed by the synthesis results.

#### Install *Grisette*

1. Install dependencies. haskell-stack is required to build *Grisette*.

2. Install *Grisette* and build the execution environment.

   ```bash
   $ cd thirdparty/Grisette/src/run_test
   $ stack ghci
   ```

3. Test whether *Grisette* is successfully built for our usage.

   ```bash
   $ cd thirdparty/Grisette
   $ cp benchmark/autolifter/single-pass/sum.hs src/run_test/Main.hs
   $ cd src; echo "main" | stack ghci
   ```

   The output in ghci should be `"success!"` followed with a line showing the time cost.

### The Structure of This Repository

#### Evaluation results

All evaluation results are available in directory `exp`, organized as follows.
1. The performance of each solver is summarized in `exp/result_cache/*.json`, where the synthesis status (fail or success) and the time cost are recorded. 
2. `exp/label` records the fully annotated program generated by our sketch extraction approach, `exp/oup/sufu` records the command-line output of each execution, and `exp/res/sufu` records the optimized programs generated by *SuFu*.

Besides, all benchmarks in our dataset are vailable in directory `benchmark`.

#### Source code

Our source code is stored in  `src`, where `src/surface` implements our surface language in Ocaml, and the other sub-directories of `src` implement *SuFu* in C++, organized as follows.

| Directory  |               Description                |
| :--------: | :--------------------------------------: |
|  `basic`   | Those basic data structures required to describe a synthesis task, such as values (`values`), semantics (`semantics`), and programs (`program`). |
| `executor` | The entrance of invoking *SuFu* (`executor/run_incre_label.cpp`). |
|  `incre`   | The core of *SuFu*, where `analysis` extracts examples, `autolabel` implements our sketch extraction approach, `solve` implements the core synthesis algorithm, and  `grammar` constructs the program spaces used in synthesis. |
| `include`  |   The head files of this C++ project.    |
|  `solver`  | The implementation of *PolyGen*, which is used to synthesize sketch holes from separate input-output examples. |

