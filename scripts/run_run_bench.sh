#!/bin/bash

# Run run_bench.escript, coloring the output according to the type of operation.

# Exit on all errors.
set -e

# Recompile, just in case.
erlc -smp gen_bench.erl

escript run_bench.escript | awk '
$1 ~ /^lookup|^get|^fetch/ {print "\033[1;32m" $0 "\033[m"; next}
$1 ~ /^iterate_/ {print "\033[33m" $0 "\033[m"; next}
$1 ~ /^store|^insert|^keystore/ {print "\033[1;34m" $0 "\033[m"; next}
$1 ~ /^delete|^erase/ {print "\033[35m" $0 "\033[m"; next}
{print}
'
