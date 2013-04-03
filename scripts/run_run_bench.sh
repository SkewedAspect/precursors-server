#!/bin/bash

# Run run_bench.escript, coloring the output according to the type of operation.

# Exit on all errors.
set -e

# Recompile, just in case.
erlc -smp gen_bench.erl

escript run_bench.escript
