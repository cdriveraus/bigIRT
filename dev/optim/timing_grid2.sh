#!/bin/bash
# Uncontended timing on dev2: main against the anova-refine branch.
cd ~/bigirt-optim/bench
export BENCH_DIR=. OMP_NUM_THREADS=1 OPENBLAS_NUM_THREADS=1
for r in M5 T2 C F; do for c in 8 26; do
  echo "== $r cores $c main";      BIGIRT_LIB=~/bigirt-optim/lib-main Rscript prof_split.R $r 1 $c 2>&1 | grep total
  echo "== $r cores $c head none"; BIGIRT_LIB=~/bigirt-optim/lib-anova Rscript prof_split.R $r 1 $c none 2>&1 | grep total
  echo "== $r cores $c head auto"; BIGIRT_LIB=~/bigirt-optim/lib-anova Rscript prof_split.R $r 1 $c 2>&1 | grep total
done; done
echo TIMING_DONE
