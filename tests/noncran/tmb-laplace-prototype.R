# Private feasibility benchmark; it intentionally compiles outside the source
# tree and therefore does not add TMB to DESCRIPTION.
tmb_library <- "C:/Users/Driver/AppData/Local/Temp/bigirt-tmb-spike"
.libPaths(unique(c(tmb_library, .libPaths())))
suppressPackageStartupMessages({
  library(TMB)
  library(numDeriv)
})
devtools::load_all(".", compile = FALSE, quiet = TRUE)

source_cpp <- normalizePath("tools/tmb_laplace_prototype.cpp", winslash = "/")
build_dir <- "C:/tmb-laplace-prototype"
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
cpp <- file.path(build_dir, basename(source_cpp))
file.copy(source_cpp, cpp, overwrite = TRUE)
TMB::compile(cpp, flags = "-O2")
dll <- TMB::dynlib(tools::file_path_sans_ext(cpp))
dyn.load(dll)

fit_tmb_laplace_prototype <- function(dat, pl = 2L, n_scale = 1L, max_iter = 100L) {
  dat <- data.table::as.data.table(dat)[order(id)]
  n_person <- max(dat$id)
  n_item <- max(dat$Item)
  data <- list(
    id = as.integer(dat$id - 1L), item = as.integer(dat$Item - 1L),
    score = as.integer(dat$score), n_person = as.integer(n_person),
    n_item = as.integer(n_item), n_scale = as.integer(n_scale), pl = as.integer(pl),
    ability_sd = 1, item_prior_sd = 4
  )
  parameters <- list(
    theta = matrix(0, n_person, n_scale),
    alpha = matrix(log(exp(0.7) - 1), n_item, n_scale),
    b = rep(0, n_item), gamma = rep(-4, n_item), delta = rep(4, n_item)
  )
  objective <- TMB::MakeADFun(data, parameters, random = "theta", DLL = tools::file_path_sans_ext(basename(cpp)), silent = TRUE)
  initial_gradient <- numDeriv::grad(objective$fn, objective$par)
  exact_gradient <- objective$gr(objective$par)
  gradient_error <- max(abs(initial_gradient - exact_gradient) / pmax(1, abs(initial_gradient)))
  timing <- system.time({
    optimizer <- nlminb(objective$par, objective$fn, objective$gr, control = list(eval.max = max_iter, iter.max = max_iter))
  })
  list(elapsed = unname(timing[["elapsed"]]), optimizer = optimizer,
    gradient_error = gradient_error, objective = objective)
}

set.seed(20260802)
cases <- list(
  one_d_2pl = list(Nsubs = 200L, Nitems = 20L, Nscales = 1L, NitemsAnswered = 6L, pl = 2L),
  three_d_4pl = list(Nsubs = 200L, Nitems = 24L, Nscales = 3L, NitemsAnswered = c(4L, 4L, 4L), pl = 4L)
)
results <- lapply(cases, function(case) {
  sim <- do.call(simIRT, c(case[setdiff(names(case), "pl")], list(mirt = case$Nscales > 1L)))
  fit <- fit_tmb_laplace_prototype(sim$dat, pl = case$pl, n_scale = case$Nscales)
  data.frame(elapsed_sec = fit$elapsed, gradient_error = fit$gradient_error,
    convergence = fit$optimizer$convergence, objective = fit$optimizer$objective)
})
print(results)
