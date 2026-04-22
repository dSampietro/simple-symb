# Symbolic Interpreter for a Simple Imperative Language

## Development setup

- Install [OPAM](https://opam.ocaml.org/) and [Z3](https://github.com/Z3Prover/z3)
- Clone the repository, create a new OPAM switch and setup the environment

```bash
git clone https://github.com/francescoborri/simple-symb.git
cd simple-symb
opam switch create . --deps-only --with-dev-setup
eval $(opam env)
```

- Build the project

```bash
dune build
```

## Example usage

- Run the symbolic interpreter on a sample program

```bash
dune exec -- simple_symb test/examples/division_by_zero.t
```
