# BQN.jl

An implementation of [BQN][] in [Julia][].

## Development

Add the following to the environment (direnv is useful here):
```
export JULIA_ROOT="/path/to/julia-1.7.2"
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
  - `BQN.bqn0` evals the BQN expression using CBQN for compilation
  - `BQN.bqn` evals the BQN expression using the self hosted compiler
- There's BQN REPL mode:
  - Start julia and after `using BQN` do `BQN.Repl.init()`
  - Enter `)` and start typing BQN code
- `make test0` runs the test suite for VM using CBQN hosted compiler
- `make test` runs the test suite for VM using self hosted compiler

[BQN]: https://mlochbaum.github.io/BQN/index.html
[Julia]: https://julialang.org
