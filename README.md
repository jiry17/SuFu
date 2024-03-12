# SuFu

Artifact for PLDI'24: Superfusion: Eliminating Intermediate Data Structures via Inductive Synthesis.

The updates of this project can be found on [GitHub](https://github.com/jiry17/SuFu).

### Install 

You can either build *SuFu* from the source or use it in a docker container.

#### Build from source (Tested on Ubuntu 20.04) 

1. Install dependencies. This project requires gcc $\geq 9$, CMake $\geq 3.13$, opam (tested on version 2.1.3 and switch ocaml.4.10.0), and python3. The other dependencies can be installed as follows.

   ```
   apt-get install libjsoncpp-dev libgoogle-glog-dev libgflags-dev pkg-config
   pip3 install tqdm argparse
   opam install ocamlfind yojson
   ```


2. Clone *SuFu* 

   ```bash
   git clone https://github.com/jiry17/SuFu
   ```


3. Build the whole project under the root directory of the project.

   ```bash
   cd SuFu; ./install
   ```

4. (optional) *SuFu* will perform better when *Gurobi*, a commercial constraint solver, is available. You can get an academic license of *gurobi* via the following steps. 

   1. Register or login at the [webside](https://www.gurobi.com/) of *Gurobi*.
   2. Visit the [Free Academic License page](https://www.gurobi.com/downloads/end-user-license-agreement-academic/) and follow the instructions on it.
   3. Get a command like  `grbgetkey x...x` at the bottom of the webpage.
   4. Execute this command under directory `SuFu/thirdparty/gurobi912/linux64/bin/`.
   5. Test your license by running `gurobi.sh` under the same directory as Step 4.

#### Download docker image

We also released a docker image where SuFu is already built at `~/SuFu`. You can download this image as follows.

```
docker pull takanashirikka/sufu
```

Note that *Gurobi* is unavailable in this docker image because the adademic license of *Gurobi* cannot be used on virtual machines. Therefore, we still recommend building this project from source.

#### Run tests

1. Test whether the project is successfully built:

   ```
   build/executor/run --benchmark=benchmark/autolifter/single-pass/sum.f --output=res.f --use_gurobi=false
   ```

   The last line of the command-line output shoud be `Success`, and there should be an optimized program in file `res.f`.

2. Test whether *SuFu* works normally with *gurobi*. Note that the docker version must fail on this task because it does not have a *gurobi* license.

   ```
   build/executor/run --benchmark=benchmark/autolifter/single-pass/sum.f --output=res.f --use_gurobi=true
   ```

   Similar to the previous test, the last line of the command-line output should be `Success`, and there should be an optimized program in file `res.f`.

### Run *SuFu* on a single task

You can run *SuFu* using the binary file `build/executor/run`.

```bash
build/executor/run --benchmark=BENCHMARK --output=OUTPUT --use_gurobi={true, false}
```

1. `--benchmark`: the file of the reference program.
2. `--output`: the output file of the optimized program.
3. `--gurobi`: whether use *Gurobi* as the underlying constraint solver.

For example, the following command runs *SuFu* (without *Gurobi*) to optimize a reference program in file `benchmark/autolifter/single-pass/mts.f` and stores the optimized program to file `res.f`.

```bash
build/executor/run --benchmark=benchmark/autolifter/single-pass/mts.f -output=res.f -use_gurobi=false
```

### Reproduce results in the paper



```bash
$ cd exp/python
$ python3 main.py [-exp {"attribute", "synduce", "autolifter", "grisette", "total"}]  [-c {R <Restart>,C <Continue>}]
# For example, to reproduce all results:
$ python3 main.py -exp=autolifter -c=R
```

1. `-exp`: the name of the experiment you want to run: `attribute` represents sufu attribute experiment, `synduce` / `autolifter` / `grisette` represents comparative experiments between `SuFu` and each baseline, `total` represents all experiments. The default value is `total`.
2. `-c`: whether to clear the cache: `R` represents yes while `C` represents no, and the default value is `C`. 

The result of each single execution is cached in `exp/result_cache` . 

The figure of each experiment will be stored in `exp/figure`.

These results are expected to be consistent with results presented in `run`.

### Reproduce results in the paper 

#### Reproduce results of experiment on the details of SuFu performance

```bash
$ cd exp/python
$ python3 main.py -exp=attribute -c=R
```

For results of comparing SuFu and Synduce listed in Table 7, `main.py` will recalculate them and print them to the standard output.

#### Reproduce results of experiment with Synduce

```bash
$ cd exp/python
$ python3 main.py -exp=synduce -c=R
```

For results of details on the performance of SuFu listed in Table 6, `main.py` will recalculate them and print them to the standard output.

#### Reproduce results of experiment with AutoLifter

````bash
$ cd exp/python
$ python3 main.py -exp=autolifter -c=R
````

For results of comparing SuFu and AutoLifter listed in Table 7, `main.py` will recalculate them and print them to the standard output.

#### Reproduce results of experiment with Grisette

````bash
$ cd exp/python
$ python3 main.py -exp=grisette -c=R
````

For results of comparing SuFu and Grisette listed in Section 7.4, `main.py` will recalculate them and print them to the standard output.

**Note**: If you don't use *gurobi*, there may be some differences between the results listed in our paper and the reproduced ones because of the performance differences between ILP solvers. However, this does not affect the significance of our experimental results. If you use *gurobi*, there can also be some small differences due to randomness.