stanconfigure <- function (pkgdir = ".")
{
    pkgdir <- .check_pkgdir(pkgdir)
    stan_files <- list.files(file.path(pkgdir, "inst",
        "stan"), full.names = TRUE, pattern = "\\.stan$")
    if (length(stan_files) != 0) {
        .add_standir(pkgdir, "R", msg = FALSE, warn = FALSE)
        .add_standir(pkgdir, "src", msg = FALSE, warn = FALSE)
        sapply(stan_files, .make_cc, pkgdir = pkgdir)
        # acc <- .setup_Makevars(pkgdir, add = TRUE)
    }
    else {
        # acc <- .setup_Makevars(pkgdir, add = FALSE)
    }
    acc <- any(acc) | .rm_cc(pkgdir)
    Rcpp::compileAttributes(pkgdir)
    stanmodels <- .update_stanmodels(pkgdir)
    acc <- acc | .add_stanfile(stanmodels, pkgdir, "R",
        "stanmodels.R")
    invisible(acc)
}
