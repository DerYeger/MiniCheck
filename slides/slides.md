---
theme: seriph
class: text-center
background: false
highlighter: shiki
lineNumbers: false
drawings:
  persist: false
transition: slide-left
title: MiniCheck
---

# MiniCheck

A CLI for CTL and bounded LTL model checking

---

# Agenda

- Overview
- Architecture
- Transition Systems
- CTL Formulas
- LTL Formulas
- CLI
- Demo

---

# What is implemented?

- Core
  - Transition systems
  - CTL model checking
  - Validations
- Bounded LTL model checking
- Unit tests
  - 88% coverage

---

# Architecture

- Cabal project
- Modularized
  - app
  - lib
    - CTL
    - LTL
    - TS
    - Utils
  - test

---

# Transition Systems

- Implicit intrinsic label
- Labels are sequence of letters

```text
States:
-> pay
- soda
- select
- beer

Transitions:
- pay -> insert_coin -> select
- select -> τ -> soda
- select -> τ -> beer
- soda -> get_soda -> pay
- beer -> get_beer -> pay
```

---

# CTL Formulas

- Explicit bracketing
- Atomic propositions are sequence of letters

```text
E (F soda)
```

---

# LTL Formulas

- Explicit bracketing
- Atomic propositions are sequence of letters

```text
(F (beer || soda))
```

---

# CLI

- `minicheck` executable
- `minicheck --help` for usage
- Three modes
  - `minicheck validate TS FILE`
  - `minicheck ctl TS FILE CTL FORMULA`
  - `minicheck ltl TS FILE LTL FORMULA BOUND`
