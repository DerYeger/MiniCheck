# MiniCheck

MiniCheck is a CLI for CTL and bounded LTL model checking on transition systems.

## Syntax

> All labels, i.e., atomic propositions, as well as state names can only consist of letters without numbers and whitespace.
> This does not limit they're expressiveness, as a simple transformation of label names does not affect their boolean values.
> E.g., `nsoda === 0` is equivalent to `numberOfSodaIsZero`, as both propositions only represent a boolean value.

> All formulas require brackets as described in the sections below.
> This approach was chosen to avoid ambiguity regarding operator precedence.

### Transition Systems

The syntax of transition systems is displayed in the example file [examples/vending-machine.txt](./examples/vending-machine.txt).

The arrow indicates initial states, while dashes indicate regular states.
Each state is automatically labelled by itself, i.e., its name.
Further labels, i.e., atomic propositions, can be specified by appending them to the state name, separated by a colon (see [examples/labels.txt](./examples/labels.txt)).

#### Validations

A transition system must have at least one initial state.
The transitions may only specify defined states.
All states must have an outgoing transition.

### CTL Formulas

State formulas:

- `true` == `true`
- `a` == `a`
- `¬Φ` == `!(Φ)`
- `Φ_1 ∧ Φ_2` == `(Φ_1 && Φ_2)`
- `Φ_1 ∨ Φ_2` == `(Φ_1 || Φ_2)`
- `Φ_1 → Φ_2` == `(Φ_1 -> Φ_2)`
- `Φ_1 ↔ Φ_2` == `(Φ_1 <-> Φ_2)`
- `Φ_1 ⊕ Φ_2` == `(Φ_1 xor Φ_2)`
- `∃φ` == `E φ`
- `∀φ` == `A φ`

Path formulas:

- `Φ_1 Until Φ_2` == `(Φ_1 U Φ_2)`
- `Next Φ` == `(X Φ)`
- `Eventually Φ` == `(F Φ)`
- `Always Φ` == `(G Φ)`

#### Validations

All atomic propositions used in a formula must be defined in the transition system.

### LTL Formulas

- `true` == `true`
- `a` == `a`
- `¬Φ` == `!(Φ)`
- `Φ_1 ∧ Φ_2` == `(Φ_1 && Φ_2)`
- `Φ_1 ∨ Φ_2` == `(Φ_1 || Φ_2)`
- `Φ_1 → Φ_2` == `(Φ_1 -> Φ_2)`
- `Φ_1 ↔ Φ_2` == `(Φ_1 <-> Φ_2)`
- `Φ_1 ⊕ Φ_2` == `(Φ_1 xor Φ_2)`
- `Φ_1 Until Φ_2` == `(Φ_1 U Φ_2)`
- `Next Φ` == `(X Φ)`
- `Eventually Φ` == `(F Φ)`
- `Always Φ` == `(G Φ)`

#### Validations

All atomic propositions used in a formula must be defined in the transition system.

## Development

Run `cabal update` to install dependencies and `cabal run MiniCheck --` to start the application.

Reference the usage section on CLI arguments.
Note that they must follow the `--` separator.

### Requirements

The following tools must be installed and activated, preferably via GHCup (v0.1.19.2).

- cabal v3.10.1.0
- GHC v9.2.5
- HLS 1.9.1.0

### Structure

The project is dvided into the following directories:

- `app`: The CLI application.
- `lib`: The business logic for parsing, validating, and evaluating CTL/LTL formulas and transition systems.
- `test`: Unit tests for the business logic.

### Tests

The test suite can be run using `cabal test` or `cabal test --enable-coverage`.
It covers the parser and semantics for CTL and LTL formulas as well as transition systems.

## Installation

The same requirements as those described in the development section apply.

Run `cabal install --overwrite-policy=always` to install the CLI application.

The [run-examples.sh](./run-examples.sh) script will install the CLI locally and run a few test commands to verify the installation.

## Usage

The CLI application can be invoked with the `MiniCheck` command.
Pass `--help` for help text.

Three modes are available:

- `minicheck validate TS_FILE`: Parse and validate the transition system found at `TS_FILE`.
- `minicheck ctl TS_FILE CTL_FORMULA`: Evaluate the CTL formula in the transition system found at `TS_FILE`.
- `minicheck ltl TS_FILE LTL_FORMULA BOUND`: Evaluate the LTL formula in the transition system found at `TS_FILE` with `BOUND` as the maximum path length.
