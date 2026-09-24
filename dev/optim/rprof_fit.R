# Rprof of one package fit: what the time outside the kernels is.
#   Rscript rprof_fit.R <regime> <seed> <cores>
args <- commandArgs(TRUE); regime <- args[1]; seed <- as.integer(args[2]); cores <- as.integer(args[3])
lib <- Sys.getenv("BIGIRT_LIB"); if(nzchar(lib)) .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({library(bigIRT); library(data.table)})
here <- Sys.getenv("BENCH_DIR", ".")
source(file.path(here, "models.R")); source(file.path(here, "scale_models.R"))
dat <- bench_data(regime, seed)
fa <- modifyList(bench_fit_args(regime), list(cores = cores))
pf <- tempfile()
Rprof(pf, interval = 0.005)
t <- system.time(fit <- suppressWarnings(do.call(fitIRT, c(list(dat = dat), fa))))[["elapsed"]]
Rprof(NULL)
s <- summaryRprof(pf)
cat(sprintf("total %.1f s\n", t))
tt <- s$by.total
tt <- tt[!grepl("^\"(do.call|fitIRT|suppressWarnings|withCallingHandlers|system.time|eval)", rownames(tt)), ]
print(head(tt[order(-tt$total.time), c("total.time", "total.pct", "self.time")], 40))
cat("\nby self:\n")
print(head(s$by.self, 25))
