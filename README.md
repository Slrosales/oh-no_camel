# OhNoCamel - MinCaml Compiler

**OhNoCamel** is a functional compiler for a subset of the OCaml language (MinCaml). Developed as part of the M1 Compilation Project, it translates high-level MinCaml source code into executable ARM assembly through a multi-stage pipeline, utilizing **ASML** as a pivotal intermediate representation.

## Team Members

*   **Laura Sofia Gomez Rosales**
*   **José Francisco Lugo Nomesque**
*   **Jin Shirakawa**
*   **Shaiva Muthaiyah**
*   **Ayda Nassir**
*   **Timothé Baleras**

## Overview

The objective of this project is to build a robust compiler pipeline that transforms MinCaml—a minimal subset of OCaml—into ARM assembly code. The project emphasizes:
*   **Incremental Development:** Building features step-by-step (Arithmetic $\to$ Control Flow $\to$ Functions $\to$ Closures).
*   **Architecture:** A clear separation between the **Frontend** (Analysis & Normalization) and **Backend** (Code Generation), bridged by the ASML intermediate language.
*   **Collaboration:** Developed using Git/GitLab with a focus on code organization and modularity.

## System Requirements

### Hardware
*   **OS:** UNIX-based operating system (Linux preferred).
*   **Memory:** Minimum 4GB RAM recommended.
*   **Architecture:** ARM (native) or x86/64 (requires emulation).

### Software
*   **OCaml Compiler:** Version 5.2.0 or higher.
*   **Build Tools:** `make`, `dune` (or standard OCaml build tools).
*   **Version Control:** Git.
*   **Emulation:** `qemu-arm` (required to run generated code on non-ARM machines).
*   **Assembler/Linker:** `gcc-arm-none-eabi` toolchain.

## Language Specification

OhNoCamel supports a strict subset of MinCaml focused on integer arithmetic and recursive functions.

| Feature | Details |
| :--- | :--- |
| **Primitive Types** | `Int` (Integer), `Bool` (Boolean), `Unit`. |
| **Data Structures** | **Tuples** and **Arrays** (Integers only). |
| **Operators** | `+`, `-`, `*`, `/`. (Float arithmetic is **unsupported**). |
| **Control Flow** | `if-then-else` constructs (branching). |
| **Functions** | Recursive functions (`let rec`), Closures, and Higher-order functions. |
| **Externals** | `print_int`, `create_array`, `sin`, `cos`, `sqrt`, etc. |

**Note:** Floating-point operations are explicitly **unsupported**. Attempting to compile code with floats will result in compilation errors.

## Compiler Architecture

The compilation process passes through the following stages:

1.  **Lexing & Parsing:** Converts source code into an Abstract Syntax Tree (AST).
2.  **Type Checking:** Static analysis to ensure type safety (Integers vs Booleans, etc.).
3.  **K-Normalization:** Simplifies nested expressions into explicit sequences of operations.
4.  **Alpha Conversion:** Renames variables to ensure global uniqueness.
5.  **Nested Let Reduction:** Flattens nested scope structures.
6.  **Closure Conversion:** Transforms functions into closures, making free variables explicit.
7.  **ASML Generation:** Converts closures into the ASML intermediate representation.
8.  **Optimization:** Local optimizations on ASML code.
9.  **ARM Generation:** Register allocation and emission of ARM assembly (`.s`).

## Usage

### Building the Compiler
Navigate to the `ocaml` directory and run:
```bash
cd ocaml
make
```
This produces the executable `mincamlc`.

### Compiling a Program
To compile a MinCaml source file to ARM assembly:
```bash
./mincamlc ../mincaml/filename.ml
```

### Advanced Options & Backend Testing
You can interact directly with specific parts of the pipeline using flags.

**Direct ASML Input:**
To skip the frontend and compile an ASML file directly to ARM:
```bash
./mincamlc -asml_input_mode -reg_alloc lc ../tests/asml_backend/valid/rec4.asml -o ../ARM/output.s
```

**Flags:**
*   `-o <file>`: Specify output filename.
*   `-asml_input_mode`: Treat input file as ASML code (Backend only).
*   `-reg_alloc <strategy>`: Select register allocation strategy.
    *   `lc`: Greedy linear allocation.
    *   `spill`: Total spilling method (useful for debugging).

### Running the Output
To run the generated ARM assembly on a Linux machine (using QEMU):

1.  Generate the assembly (`output.s`).
2.  Assemble and link it (usually via `gcc` or `make` in the ARM directory).
3.  Run with QEMU:
```bash
qemu-arm ./output.arm
```

## ⚠️ Limitations & Known Issues

*   **Floats:** Floating-point operations and float arrays are not supported.
*   **Backend Closures:** There are known edge cases regarding specific closure implementations in the backend.
*   **Spilling:** Memory-intensive programs may fail when using the `spill` allocation strategy due to stack limitations or inefficient access patterns.
*   **Syntax:** The parser expects strict syntax adherence (e.g., proper parentheses usage).
