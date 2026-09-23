#!/bin/bash
# Run a grid of benchmark cells as independent processes (processes scale on
# dev1; threads do not). Each cell is one line "regime seed variant" of the
# cell file; results land in <outdir>, one RDS and one log line per cell.
#
#   setsid nohup ./run_grid.sh cells.txt results 16 < /dev/null > grid.log 2>&1 &
#
# Writes <outdir>/GRID_DONE when every cell has finished.
cells=$1; out=$2; par=${3:-8}
mkdir -p "$out"
export BENCH_DIR=$(pwd)
export BIGIRT_LIB=${BIGIRT_LIB:-$HOME/bigirt-optim/lib-base}
# One BLAS/OpenMP thread per process, so parallel cells do not contend.
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
echo $$ > "$out/GRID_PID"
grep -v '^#' "$cells" | grep -v '^$' | xargs -P "$par" -L 1 bash -c \
  'Rscript ${HARNESS:-harness.R} "$0" "$1" "$2" '"$out"' > '"$out"'/"$0_$1_$2".log 2>&1; tail -n 1 '"$out"'/"$0_$1_$2".log'
date > "$out/GRID_DONE"
