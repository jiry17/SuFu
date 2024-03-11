# SuFu

Artifact for PLDI'24: Superfusion: Eliminating Intermediate Data Structures via Inductive Synthesis.

The updates of this project can be found on [GitHub](https://github.com/jiry17/SuFu).

### Install 

#### Build from source (Test on Ubuntu 20.04) 

1. Install dependencies

   ```bash
   $ apt-get update
   $ apt-get install cmake ninja-build git wget libgoogle-glog-dev python3-tk python3-pip libboost-all-dev libjsoncpp-dev cbmc ocaml ocaml-nox camlp4-extra opam cabal-install openjdk-11-jre-headless
   $ apt-get install haskell-stack 
   $ pip3 install pyparsing tqdm argparse toml
   $ opam init
   $ opam update
   $ opam install dune.3.10.0 ocamlfind base fmt getopt sexplib lwt fileutils stdio yojson ppx_let ppx_deriving ppx_sexp_conv ppx_hash lwt_ppx parsexp_io core_unix menhirLib ocamlgraph menhir
   $ eval $(opam env)
   $ curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh    # install ghcup
   $ curl -sSL https://get.haskellstack.org/ | sh -s -- -f                      # install stack
   $ source ~/.bashrc
   $ ghcup install ghc 9.2.5                                                    # install ghc
   $ ghcup set ghc 9.2.5
   $ cabal update
   $ cabal install cabal-install
   $ cabal install grisette
   $ source ~/.bashrc
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