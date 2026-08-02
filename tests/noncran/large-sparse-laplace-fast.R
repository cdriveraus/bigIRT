## One-pass large sparse Laplace stress test.
##
## Run from the project root, for example:
## Rscript package/tests/noncran/large-sparse-laplace-fast.R
##
## Optional arguments: Nsubjects Nitems answers_per_subject cores output_csv

args <- commandArgs(trailingOnly = TRUE)
arg_int <- function(position, default){
  if(length(args) < position || !nzchar(args[position])) return(default)
  value <- suppressWarnings(as.integer(args[position]))
  if(!is.finite(value) || value < 1L) stop("Argument ", position, " must be a positive integer.")
  value
}

n_subjects <- arg_int(1L, 50000L)
n_items <- arg_int(2L, 5000L)
answers_per_subject <- arg_int(3L, 10L)
cores <- arg_int(4L, min(4L, parallel::detectCores(logical = FALSE)))
output_csv <- if(length(args) >= 5L) args[5L] else ""

if(answers_per_subject > n_items) stop("answers_per_subject cannot exceed n_items.")
if((n_subjects * answers_per_subject) %% n_items != 0L){
  stop("n_subjects * answers_per_subject must be divisible by n_items to guarantee equal item exposure.")
}

suppressPackageStartupMessages({
  library(data.table)
  library(pkgload)
})
pkgload::load_all("package", export_all = FALSE, helpers = FALSE, quiet = TRUE)

message(sprintf(
  "Generating sparse 2PL data: %d subjects x %d items x %d answers = %s rows.",
  n_subjects, n_items, answers_per_subject,
  format(n_subjects * answers_per_subject, big.mark = ",")
))

## Each round assigns every item equally often. Distinct round offsets ensure
## every subject receives distinct items; each item has exactly
## n_subjects * answers_per_subject / n_items responses.
subject_id <- rep.int(seq_len(n_subjects), times = answers_per_subject)
round_id <- rep(seq_len(answers_per_subject), each = n_subjects)
item_id <- ((subject_id - 1L + (round_id - 1L)) %% n_items) + 1L

ability <- stats::rnorm(n_subjects)
discrimination <- pmax(0.15, stats::rnorm(n_items, mean = 1, sd = 0.2))
difficulty <- stats::rnorm(n_items)
eta <- discrimination[item_id] * ability[subject_id] - difficulty[item_id]
dat <- data.table(
  id = subject_id,
  Item = item_id,
  Scale = 1L,
  score = stats::rbinom(length(item_id), size = 1L, prob = stats::plogis(eta))
)
rm(subject_id, round_id, item_id, eta)
invisible(gc())

subject_counts <- dat[, .N, by = id]$N
item_counts <- dat[, .N, by = Item]$N
if(min(subject_counts) < answers_per_subject || min(item_counts) < 50L || anyDuplicated(dat, by = c("id", "Item"))){
  stop("Sparse-design invariant failed: subject/item exposure or uniqueness is incorrect.")
}
message(sprintf(
  "Design verified: minimum subject exposure = %d; minimum item exposure = %d.",
  min(subject_counts), min(item_counts)
))

gc_before <- gc()
started <- proc.time()[["elapsed"]]
message(sprintf("Starting one laplace_fast outer pass on %d core(s)...", cores))
fit <- fitIRT(
  dat,
  pl = 2L,
  cores = cores,
  priors = TRUE,
  ebayes = FALSE,
  dropPerfectScores = FALSE,
  marginalApprox = "laplace_fast",
  laplaceOuterIter = 1L,
  laplaceItemIter = 1L,
  laplaceDiagnostics = TRUE,
  laplaceKeepCovariance = FALSE,
  estimateAbilityCorr = FALSE,
  normalise = FALSE,
  verbose = 1L,
  plot = FALSE
)
elapsed <- proc.time()[["elapsed"]] - started
gc_after <- gc()

diag <- as.data.table(fit$laplaceDiagnostics)
result <- data.table(
  subjects = n_subjects,
  items = n_items,
  responses = nrow(dat),
  min_subject_exposure = min(subject_counts),
  min_item_exposure = min(item_counts),
  cores = cores,
  elapsed_sec = elapsed,
  outer_passes = fit$laplaceStatus$outer_iters,
  converged = fit$laplaceStatus$converged,
  stop_reason = fit$laplaceStatus$reason,
  item_grad_norm = fit$laplaceStatus$last_item_grad_norm,
  person_sec = diag$personStepSec[1L],
  item_sec = diag$itemStepSec[1L],
  refresh_sec = diag$refreshStepSec[1L],
  objective_sec = diag$objectiveEvalSec[1L],
  memory_mb_before = sum(gc_before[, 2L]),
  memory_mb_after = sum(gc_after[, 2L])
)
print(result)
if(nzchar(output_csv)) fwrite(result, output_csv)
