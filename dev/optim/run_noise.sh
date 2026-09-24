#!/bin/bash
# Run noise_probe.R for each "regime seed" line of $1, $2 at a time.
export BENCH_DIR=$(pwd) OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
grep -v '^$' "$1" | xargs -P "$2" -L 1 bash -c 'Rscript noise_probe.R "$0" "$1" > noise_$0_$1.log 2>&1'
date > NOISE_GRID_DONE
