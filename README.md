# BQN.jl

An implementation of [BQN][] in [Julia][].

## Development

Add the following to the environment (direnv is useful here):
```
export JULIA_ROOT="/path/to/julia-1.7.0"
export JULIA_PROJECT="${PWD}"
PATH_add "${JULIA_ROOT}/bin"
PATH_add "${PWD}/bin"
```

Then:

- Linux operating system is assumed
- Clone the repo
- Run `make init` to initialize submodules and build the CBQN (BQN
  implementation used for bootstrap).
- Run `julia` and eval `using BQN`
  - `BQN.bqneval0` evals the BQN expression using CBQN for compilation
  - `BQN.bqneval` evals the BQN expression using the self hosted compiler
- `make test0` runs the test suite for VM using CBQN hosted compiler
- `make test` runs the test suite for VM using self hosted compiler

[BQN]: https://mlochbaum.github.io/BQN/index.html
[Julia]: https://julialang.org
