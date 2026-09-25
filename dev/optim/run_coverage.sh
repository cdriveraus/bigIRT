#!/bin/bash
# Run the standard-error coverage cells (coverage_cell.R) as independent
# processes, one fit per process, skipping any cell whose RDS already exists so
# an interrupted grid resumes. Each line of the cell file is "cell rep arm".
#
#   BIGIRT_LIB=~/bigirt-cert/lib setsid nohup ./run_coverage.sh cells_coverage.txt \
#     coverage 14 < /dev/null > coverage_grid.log 2>&1 &
#
# The script's own pid goes to <outdir>/GRID_PID; setsid makes it the leader
# of its process group, so `kill -- -$(cat <outdir>/GRID_PID)` stops the grid
# and nothing else. Writes <outdir>/GRID_DONE when every cell has finished.
cells=$1; out=$2; par=${3:-8}
mkdir -p "$out"
export BIGIRT_LIB=${BIGIRT_LIB:?set BIGIRT_LIB to the library under test}
# One BLAS/OpenMP thread per process, so parallel cells do not contend.
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
echo $$ > "$out/GRID_PID"
grep -v '^#' "$cells" | grep -v '^$' | xargs -P "$par" -L 1 bash -c \
  'f='"$out"'/$(printf "%s_%s_%03d" "$0" "$2" "$1"); [ -f "$f.rds" ] && exit 0;
   Rscript coverage_cell.R "$0" "$1" "$2" '"$out"' > "$f.log" 2>&1; tail -n 1 "$f.log"'
date > "$out/GRID_DONE"
