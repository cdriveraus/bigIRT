#!/bin/bash
# The certificate-cost run (cert_cost.R): the study cells four at a time at one
# core each, then the scaling cells one at a time at four cores.
#
#   BIGIRT_LIB=~/lib setsid nohup bash ./run_cert_cost.sh <outdir> < /dev/null > run.log 2>&1 &
#
# The script's pid is <outdir>/RUN_PID (kill -- -PID stops the run and nothing
# else); <outdir>/RUN_DONE is written at the end.
out=$1
mkdir -p "$out"
export BIGIRT_LIB=${BIGIRT_LIB:?set BIGIRT_LIB to the library under test}
export OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
echo $$ > "$out/RUN_PID"
tr -d '\r' < cells_cert_study.txt | grep -v '^#' | grep -v '^$' | xargs -P 4 -L 1 bash -c \
  'Rscript cert_cost.R study "$0" "$1" "$2" '"$out"' > '"$out"'/study_"$0"_"$2"_"$1".log 2>&1; tail -n 1 '"$out"'/study_"$0"_"$2"_"$1".log'
tr -d '\r' < cells_cert_scale.txt | grep -v '^#' | grep -v '^$' | while read -r N I PL; do
  Rscript cert_cost.R scale "$N" "$I" "$PL" "$out" 4 > "$out/scale_${N}_${I}_${PL}.log" 2>&1
  tail -n 1 "$out/scale_${N}_${I}_${PL}.log"
done
date > "$out/RUN_DONE"
