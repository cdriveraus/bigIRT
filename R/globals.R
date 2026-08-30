## Column names used in data.table expressions, and the internal bookkeeping
## columns fitIRT adds to a copy of the caller's data. R's code analysis sees
## these as free variables because data.table evaluates them inside the frame of
## the table rather than the calling environment; declaring them here keeps the
## check output free of notes that would otherwise hide a real one.
utils::globalVariables(c(
  ## response columns, in simIRT and the comparison helpers
  ".",
  "id", "Item", "Scale", "score", "p", "pcorrect", "D",
  "parameter", "corr_type", "clms",
  ## per-item and per-person means, in dropPerfectScores
  "itemMean", "personMean",
  ## convergence bookkeeping, in fitIRT
  "itemGradNorm", "strictCriterion", "strictStreak",
  ## columns fitIRT attaches to its working copy of the data
  "__bigIRT_input_row__", "__bigIRT_training__"
))
