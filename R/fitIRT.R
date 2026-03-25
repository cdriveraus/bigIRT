

inv_logit <- function(x) exp(x)/(1+exp(x))
logit <- function(x)  log(x)-log((1-x))

afunc <- function(x) log1p(exp(x))
afunci <- function(x) log(exp(x)-1)

cfunc <- function(x) inv_logit(x)*.5
cfunci <- function(x) logit(x*2)
dfunc <- function(x) inv_logit(x)*.5+.5
dfunci <- function(x) logit((x-.5)*2)

pcalc <- function(score,ability,B,A=1,C=0,D=1){
  p <- C + (1.0-C) / ( 1.0 + exp( (-A * (ability - B))  ) )
}

a_matrix_to_vector <- function(x){
  as.array(as.vector(t(as.matrix(x))))
}

a_vector_to_matrix <- function(x, nitems, nscales){
  matrix(as.numeric(x), nrow = nitems, ncol = nscales, byrow = TRUE)
}

checkP <- function(fit){ #for checking max a posteriori model parameters using R based calc instead of stan based
  p=rep(NA,fit$dat$Nobs)
  use_matrix_a <- !is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2
  for(i in 1:length(p)){
    if(use_matrix_a){
      aload <- fit$pars$A[fit$dat$item[i],]
      eta <- sum(aload * fit$pars$Ability[fit$dat$id[i],]) - fit$itemPars$B[fit$dat$item[i]]
      pmid <- fit$itemPars$C[fit$dat$item[i]] + (fit$itemPars$D[fit$dat$item[i]] - fit$itemPars$C[fit$dat$item[i]]) * inv_logit(eta)
      p[i] <- pmid
    } else {
    p[i] <- fit$itemPars$C[fit$dat$item[i]] + (1.0-fit$itemPars$C[fit$dat$item[i]]) / ( 1.0 + exp(
      (-fit$itemPars$A[fit$dat$item[i]] * (
        fit$personPars[fit$dat$id[i], 1+fit$dat$scale[i]] -
          fit$itemPars$B[fit$dat$item[i]]
      ))));
    }

    if(fit$dat$score[i]==0) p[i]= 1.0-p[i];
  }
  return(p)
}


# birtRunGeneratedQuantities<- function(fit){
#
#   log1p_exp=function(x) log1p(exp(x))
#
#   genq <- function(){
#     browser()
#
#     A=rep(NA,Nitems) # item A values
#     B=rep(NA,Nitems) #item B values
#     C=rep(NA,Nitems) #item C values
#     #put the user supplied fixed values into the item parameter objects
#     A[fixedA] = Adata[fixedA];
#     B[fixedB] = Bdata[fixedB];
#     C[fixedC] = Cdata[fixedC];
#
#     #put the free parameters into the item parameter objects
#     A[whichnotfixedA] = invspApars;
#     B[whichnotfixedB] = Bpars;
#     C[whichnotfixedC] = logitCpars;
#
#
#     # for(i in 1:Nsubs){ #for every subject
#     #   for(j in 1:Nscales){ #and every scale
#     #     if(fixedAbilityLogical[i,j]==1){
#     #       Ability[i,j] = Abilitydata[i,j];
#     #     } else{ #if ability is user supplied, input it
#     #       Ability[i,j] = Abilitypars[Abilityparsindex[i,j]]; # or input the free parameter
#     #       if(NpersonPreds) {
#     #         predsmean=rep(0, NpersonPreds); #compute mean of person predictors
#     #         count=0;
#     #         for( ri in 1:Nobs){
#     #           if(id[i] == i){
#     #             count=count+1;
#     #             predsmean=predsmean+personPreds[i,];
#     #           }
#     #         }
#     #         predsmean= predsmean/count;
#     #         Ability[i,j] = Ability[i,j] +predsmean * Abilitybeta[j,]; #when there are person predictors, apply the effect
#     #       }
#     #     }
#     #   }
#     # }
#
#
#     for(i in 1:Nitems){ #for every item
#       count=0;
#       predsmean=rep(0,NitemPreds);
#       for( ri in 1:Nobs){
#         if(item[ri] == i){
#           count=count+1;
#           predsmean=predsmean+itemPreds[ri,];
#         }
#       }
#       predsmean= predsmean/count;
#       if(fixedAlog[i]==0){ #if free A par and item predictors, compute average item effect
#         A[i] =A[i]+ matrix(predsmean,1) %*% t(invspAbeta[ifelse(itemSpecificBetas==1,freeAref[item[i]],1),,drop=FALSE]); #when there are person predictors, apply the effect
#         A[i]=log1p_exp(A[i]);
#       }
#       if(fixedB[i]==0){ #if free B par and item predictors, compute average item effect
#         B[i] = B[i] + matrix(predsmean,1) %*% t(Bbeta[ifelse(itemSpecificBetas==1, freeBref[item[i]], 1),,drop=F]); #when there are person predictors, apply the effect
#       }
#     }
#
#
#     #
#     #       #linearised regression weights for reporting
#     #       if(doApreds){
#     #         if(size(Abeta)==1){
#     #           Abeta[1,] = ((log1p_exp(mean(invspApars)+invspAbeta[1,]*.01))-(log1p_exp(mean(invspApars)-invspAbeta[1,]*.01)))/.02;
#     #         }
#     #         if(size(Abeta)>1){
#     #           for(i in 1:size(Abeta)){
#     #             Abeta[i,] = ((log1p(exp(invspApars[i])+invspAbeta[i,]*.01))-(log1p(exp(invspApars[i])-invspAbeta[i,]*.01)))/.02;
#     #           }
#     #         }
#     #       }
#     #
#     #       if(doCpreds){
#     #         if(size(Cbeta)==1)   Cbeta[1,] = ((inv_logit(mean(logitCpars))+logitCbeta[1,]*.01)-(inv_logit(mean(logitCpars))-logitCbeta[1,]*.01))/.02;
#     #         if(size(Cbeta)>1){
#     #           for(i in 1:size(Cbeta)){
#     #             Cbeta[i,] = ((inv_logit(logitCpars[i])+logitCbeta[i,]*.01)-(inv_logit(logitCpars[i])-logitCbeta[i,]*.01))/.02;
#     #           }
#     #         }
#     #       }
#
#   } #end internal genq function
#
#   e <- list2env(c(fit$pars,fit$dat))
#   environment(genq) <- e
#   genq()
# }


#' normaliseIRT
#'
#' Normalise item response theory (IRT) parameters.
#'
#' @param B Vector of item difficulty parameters.
#' @param Ability Vector of persons' ability parameters.
#' @param A Vector of item discrimination parameters.
#' @param normbase The base from which the normalisation should be calculated. Can be 'Ability' or 'B'.
#' @param normaliseScale The scale to normalise to.
#' @param normaliseMean The mean to normalise to. The default is 0 for the
#'   supported \code{normbase} values ('Ability' or 'B').
#' @param robust if TRUE, outliers (greater than 1.5x the interquartile range from the interquartile region of 25-75%)
#' are dropped before computing the mean and sd for normalisation.
#'
#' @return A list containing the normalised A, B, and Ability parameters.
#' @export
#' @examples
#' B <- rnorm(100,2,1)
#' Ability <- rnorm(500,3,.5)
#' A <- rnorm(100,1.4,.05)
#' normaliseIRT(B, Ability, A, normbase='B')
#'
#'
normaliseIRT <- function(B,Ability, A,normbase='Ability',normaliseScale=1,  normaliseMean=0,robust=TRUE){

  # if(!robust){
  #   includePersons <- 1:length(Ability)
  #   includeItems <- 1:length(B)
  # }
  # if(robust){
  #   includePersons <- which(Ability %in% boxplot(Ability, plot = F)$out)
  #   includeItems <- which(B %in% boxplot(B, plot = F)$out & A %in% boxplot(A, plot = F)$out)
  # }

  if(normbase %in% c('Ability','B')){
    if(normbase =='Ability'){
      nsd <- ifelse(robust, diff(quantile(Ability,probs=c(.25,.75))), sd(Ability)) / (normaliseScale)
      nm <- ifelse(robust, median(Ability),mean(Ability))
    }

    if(normbase =='B'){
      nsd <- ifelse(robust, diff(quantile(B, probs=c(.25,.75))), sd(B)) / (normaliseScale)
      nm <- ifelse(robust, median(B), mean(B))
    }

    Ability <- (Ability -nm)/ nsd +normaliseMean
    B  <- ( B-nm) / nsd +normaliseMean
    A <-  A * nsd
  }

  # if(normbase == 'A'){
  #   logA <- log(A)
  #   nsd <- ifelse(robust,diff(quantile(logA,probs=c(.25,.75))), sd(logA)) / (normaliseScale)
  #   if(is.na(nsd)) stop('Error calculating sd of discrimination parameters -- do they vary or are any negative?')
  #   nm <- ifelse(robust, median(logA),mean(logA))
  #   normaliseMean <- log(normaliseMean)
  #
  #   Ability <- (Ability -nm)/ nsd +normaliseMean
  #   B  <- ( B-nm) / nsd +normaliseMean
  #   logA <-  (logA -nm)/nsd +normaliseMean
  # }

  return(list(A=A,B=B,Ability=Ability))
}

#' Normalize Multidimensional IRT Parameters
#'
#' Applies a curve-preserving affine transform to a multidimensional IRT
#' solution so that the latent ability distribution is centered and whitened,
#' with optional orthogonal alignment to a reference loading matrix.
#'
#' @param B Numeric vector of item intercept/difficulty parameters.
#' @param Ability Numeric matrix of person abilities with rows corresponding to
#'   persons and columns to latent factors.
#' @param A Numeric item-by-factor loading matrix.
#' @param AbilityCorr Optional latent correlation matrix. If omitted, the
#'   correlation is estimated from `Ability`.
#' @param normaliseScale Numeric scalar giving the target latent SD after
#'   whitening. Defaults to 1.
#' @param normaliseMean Numeric scalar or vector giving the target latent mean
#'   after normalization. Defaults to 0.
#' @param jitter Small positive constant added for numerical stability.
#' @param referenceA Optional reference loading matrix used for orthogonal
#'   Procrustes alignment.
#' @param align Character string, either `"none"` or `"orthogonal"`.
#'
#' @return A list containing normalized `A`, `B`, `Ability`, and associated
#'   affine transform pieces (`center`, `chol_cov`, `inv_chol`, `AbilityCorr`,
#'   `sign`, and `rotation`).
#' @export
normaliseMIRT <- function(B, Ability, A, AbilityCorr = NULL,
  normaliseScale = 1, normaliseMean = 0, jitter = 1e-8,
  referenceA = NULL, align = c("none", "orthogonal")){

  align <- match.arg(align)

  B_names <- names(B)
  ability_dimnames <- dimnames(as.matrix(Ability))
  A_dimnames <- dimnames(as.matrix(A))
  Ability <- as.matrix(Ability)
  A <- as.matrix(A)
  B <- as.numeric(B)
  if(nrow(A) != length(B)) stop("A must have one row per item in B.")
  if(ncol(A) != ncol(Ability)) stop("A and Ability must have the same number of dimensions.")
  K <- ncol(A)
  if(K <= 1L){
    out <- normaliseIRT(B = B, Ability = as.numeric(Ability[,1]), A = as.numeric(A[,1]),
      normbase = "B", normaliseScale = normaliseScale, normaliseMean = normaliseMean, robust = FALSE)
    names(out$B) <- B_names
    return(c(out, list(
      center = mean(Ability[,1], na.rm = TRUE),
      chol_cov = matrix(stats::sd(Ability[,1], na.rm = TRUE), 1, 1),
      inv_chol = matrix(1 / pmax(stats::sd(Ability[,1], na.rm = TRUE), jitter), 1, 1),
      global_scale = stats::sd(B, na.rm = TRUE) / normaliseScale,
      latent_shift = 0,
      AbilityCorr = matrix(1, 1, 1),
      sign = 1,
      rotation = matrix(1, 1, 1)
    )))
  }

  mu <- colMeans(Ability, na.rm = TRUE)
  ability_sd <- apply(Ability, 2, stats::sd, na.rm = TRUE)
  ability_sd[!is.finite(ability_sd) | ability_sd < jitter] <- 1

  corr <- if(is.null(AbilityCorr)) {
    stats::cor(Ability, use = "pairwise.complete.obs")
  } else {
    as.matrix(AbilityCorr)
  }
  corr[!is.finite(corr)] <- 0
  corr <- (corr + t(corr)) / 2
  diag(corr) <- 1

  cov_mat <- diag(ability_sd, K) %*% corr %*% diag(ability_sd, K)
  cov_mat <- cov_mat + diag(jitter, K)
  chol_cov_raw <- chol(cov_mat)
  chol_cov <- chol_cov_raw / normaliseScale
  inv_chol <- solve(chol_cov)

  Ability0 <- sweep(Ability, 2, mu, "-") %*% inv_chol
  B0 <- as.numeric(B - A %*% mu)
  A0 <- A %*% t(chol_cov)
  latent_shift <- rep_len(as.numeric(normaliseMean), K)
  Ability2 <- sweep(Ability0, 2, latent_shift, "+")
  A1 <- A0
  B2 <- as.numeric(B0 + A1 %*% latent_shift)
  primary_factor <- max.col(abs(A1), ties.method = "first")
  sign_vec <- rep(1, K)
  for(k in seq_len(K)){
    idx <- which(primary_factor == k & abs(A1[,k]) > jitter)
    if(length(idx)){
      mean_loading <- mean(A1[idx, k], na.rm = TRUE)
      if(is.finite(mean_loading) && mean_loading < 0) sign_vec[k] <- -1
    }
  }
  sign_mat <- diag(sign_vec, K)
  Ability2 <- Ability2 %*% sign_mat
  A1 <- A1 %*% sign_mat
  AbilityCorr2 <- stats::cor(Ability2, use = "pairwise.complete.obs")

  rotation <- diag(1, K)
  if(!is.null(referenceA) && identical(align, "orthogonal")){
    referenceA <- as.matrix(referenceA)
    if(!all(dim(referenceA) == dim(A1))) stop("referenceA must have the same dimensions as A.")
    M <- crossprod(A1, referenceA)
    s <- svd(M)
    rotation <- s$u %*% t(s$v)
    A1 <- A1 %*% rotation
    Ability2 <- Ability2 %*% rotation
    AbilityCorr2 <- t(rotation) %*% AbilityCorr2 %*% rotation
  }
  dimnames(Ability2) <- ability_dimnames
  dimnames(A1) <- A_dimnames
  names(B2) <- B_names

  list(
    A = A1,
    B = B2,
    Ability = Ability2,
    center = mu,
    chol_cov = chol_cov,
    inv_chol = inv_chol,
    global_scale = 1,
    latent_shift = latent_shift,
    AbilityCorr = AbilityCorr2,
    sign = sign_vec,
    rotation = rotation
  )
}

#' Extract Comparable MIRT Parameters
#'
#' Extracts a common set of multidimensional IRT parameters from either a
#' `bigIRT` fit or a [`mirt`](https://cran.r-project.org/package=mirt) fit.
#'
#' @param object A fitted `bigIRT` object or a `mirt` `SingleGroupClass`
#'   object.
#' @param score_method Character string passed to `mirt::fscores()` when
#'   extracting abilities from a `mirt` fit.
#'
#' @return A list with components `source`, `A`, `B`, `C`, `D`, `Ability`, and
#'   `corr`.
#' @export
extractMIRTpars <- function(object, score_method = "EAP"){
  if(inherits(object, "bigIRT_simIRT")){
    ability <- as.matrix(object$Ability)
    if(is.null(dim(ability))) ability <- matrix(as.numeric(ability), ncol = 1)
    K <- ncol(ability)
    factor_names <- colnames(ability)
    if(is.null(factor_names)) factor_names <- paste0("F", seq_len(K))
    colnames(ability) <- factor_names

    if(!is.null(object$primaryScale)){
      A <- as.matrix(object$A)
      B <- as.numeric(object$B)
      C <- as.numeric(object$C)
      D <- rep(1, length(B))
      rownames(A) <- if(!is.null(rownames(A))) rownames(A) else as.character(seq_len(nrow(A)))
      colnames(A) <- factor_names
    } else {
      A_in <- as.matrix(object$A)
      B_in <- as.matrix(object$B)
      C_in <- as.matrix(object$C)
      nitems_per_scale <- nrow(A_in)
      total_items <- nitems_per_scale * K
      A <- matrix(0, nrow = total_items, ncol = K)
      B <- numeric(total_items)
      C <- numeric(total_items)
      D <- rep(1, total_items)
      item_names <- character(total_items)
      idx <- 1L
      for(k in seq_len(K)){
        rows <- idx:(idx + nitems_per_scale - 1L)
        A[rows, k] <- A_in[, k]
        B[rows] <- B_in[, k]
        C[rows] <- C_in[, k]
        item_names[rows] <- paste0("S", factor_names[k], "_I", seq_len(nitems_per_scale))
        idx <- idx + nitems_per_scale
      }
      rownames(A) <- item_names
      colnames(A) <- factor_names
    }
    return(list(
      source = "simIRT",
      A = A,
      B = B,
      C = C,
      D = D,
      Ability = ability,
      corr = if(ncol(ability) > 1) stats::cor(ability, use = "pairwise.complete.obs") else matrix(1, 1, 1)
    ))
  }
  if(is.list(object) && !is.null(object$pars) && !is.null(object$pars$A)){
    A <- as.matrix(object$pars$A)
    if(is.null(dim(A))) A <- matrix(as.numeric(A), ncol = 1)
    ability <- as.matrix(object$pars$Ability)
    if(is.null(dim(ability))) ability <- matrix(as.numeric(ability), ncol = 1)
    return(list(
      source = "bigIRT",
      A = A,
      B = as.numeric(object$pars$B),
      C = if(!is.null(object$pars$C)) as.numeric(object$pars$C) else rep(0, length(object$pars$B)),
      D = if(!is.null(object$pars$D)) as.numeric(object$pars$D) else rep(1, length(object$pars$B)),
      Ability = ability,
      corr = if(!is.null(object$abilityPrior$corr)) as.matrix(object$abilityPrior$corr) else if(ncol(ability) > 1) stats::cor(ability) else matrix(1, 1, 1)
    ))
  }
  if(requireNamespace("mirt", quietly = TRUE) && inherits(object, "SingleGroupClass")){
    coef_s <- mirt::coef(object, simplify = TRUE)
    items <- as.data.frame(coef_s$items)
    a_cols <- grep("^a[0-9]+$", names(items), value = TRUE)
    d_col <- if("d" %in% names(items)) "d" else grep("^d", names(items), value = TRUE)[1]
    if(length(a_cols) == 0L || is.na(d_col)) stop("Could not extract mirt item parameters.")
    return(list(
      source = "mirt",
      A = as.matrix(items[, a_cols, drop = FALSE]),
      B = -as.numeric(items[[d_col]]),
      C = if("g" %in% names(items)) as.numeric(items$g) else rep(0, nrow(items)),
      D = if("u" %in% names(items)) as.numeric(items$u) else rep(1, nrow(items)),
      Ability = as.matrix(mirt::fscores(object, method = score_method, full.scores = TRUE)),
      corr = cov2cor(coef_s$cov)
    ))
  }
  stop("Unsupported object type for extractMIRTpars().")
}

bigIRT_comparison_state_normalised <- function(state, referenceA = NULL,
  normaliseScale = 1, normaliseMean = 0){
  norm <- normaliseMIRT(
    B = state$B,
    Ability = state$Ability,
    A = state$A,
    AbilityCorr = state$corr,
    normaliseScale = normaliseScale,
    normaliseMean = normaliseMean,
    referenceA = referenceA,
    align = if(is.null(referenceA)) "none" else "orthogonal"
  )
  utils::modifyList(state, list(
    A = norm$A,
    B = norm$B,
    Ability = norm$Ability,
    corr = norm$AbilityCorr,
    transform = norm
  ))
}

bigIRT_comparison_standardize_names <- function(state, item_names, factor_names, person_names){
  state$A <- as.matrix(state$A)
  state$Ability <- as.matrix(state$Ability)
  rownames(state$A) <- item_names
  colnames(state$A) <- factor_names
  rownames(state$Ability) <- person_names
  colnames(state$Ability) <- factor_names
  state$corr <- as.matrix(state$corr)
  rownames(state$corr) <- factor_names
  colnames(state$corr) <- factor_names
  state
}

bigIRT_comparison_itempars_dt <- function(state, source, form){
  A <- as.matrix(state$A)
  item_names <- rownames(A)
  if(is.null(item_names)) item_names <- as.character(seq_len(nrow(A)))
  factor_names <- colnames(A)
  if(is.null(factor_names)) factor_names <- paste0("F", seq_len(ncol(A)))
  colnames(A) <- factor_names
  rownames(A) <- item_names

  dt_A <- data.table::as.data.table(as.table(A))
  data.table::setnames(dt_A, c("item", "factor", "value"))
  dt_A[, parameter := "A"]

  dt_B <- data.table::data.table(
    item = item_names,
    factor = NA_character_,
    parameter = "B",
    value = as.numeric(state$B)
  )
  dt_C <- data.table::data.table(
    item = item_names,
    factor = NA_character_,
    parameter = "C",
    value = as.numeric(state$C)
  )
  dt_D <- data.table::data.table(
    item = item_names,
    factor = NA_character_,
    parameter = "D",
    value = as.numeric(state$D)
  )

  out <- data.table::rbindlist(list(dt_A, dt_B, dt_C, dt_D), fill = TRUE, use.names = TRUE)
  out[, `:=`(source = source, form = form)]
  data.table::setcolorder(out, c("source", "form", "parameter", "item", "factor", "value"))
  out[]
}

bigIRT_comparison_personpars_dt <- function(state, source, form){
  Ability <- as.matrix(state$Ability)
  factor_names <- colnames(Ability)
  if(is.null(factor_names)) factor_names <- paste0("F", seq_len(ncol(Ability)))
  person_names <- rownames(Ability)
  if(is.null(person_names)) person_names <- as.character(seq_len(nrow(Ability)))
  colnames(Ability) <- factor_names
  rownames(Ability) <- person_names

  out <- data.table::as.data.table(as.table(Ability))
  data.table::setnames(out, c("person", "factor", "value"))
  out[, `:=`(source = source, form = form)]
  data.table::setcolorder(out, c("source", "form", "person", "factor", "value"))
  out[]
}

bigIRT_comparison_matrix_dt <- function(mat, source, form, value_name = "value"){
  mat <- as.matrix(mat)
  out <- data.table::as.data.table(as.table(mat))
  data.table::setnames(out, c("row", "col", value_name))
  out[, `:=`(source = source, form = form)]
  data.table::setcolorder(out, c("source", "form", "row", "col", value_name))
  out[]
}

bigIRT_comparison_attach_reference <- function(dt, id_cols, ref_name){
  ref <- dt[source == ref_name, c(id_cols, "value"), with = FALSE]
  data.table::setnames(ref, "value", "referenceVal")
  out <- merge(dt, ref, by = id_cols, all.x = TRUE, sort = FALSE)
  out[]
}

#' Compare IRT Model Parameterizations
#'
#' Coerces simulated truth, `bigIRT` fits, and `mirt` fits onto a common raw
#' and normalized parameter representation for direct comparison.
#'
#' @param models A named or unnamed list containing up to one `simIRT` object
#'   and any number of fitted `bigIRT` and/or `mirt` models.
#' @param score_method Character string passed to `mirt::fscores()` when
#'   extracting abilities from `mirt` fits.
#' @param normaliseScale Numeric scalar passed to [normaliseMIRT()].
#' @param normaliseMean Numeric scalar or vector passed to [normaliseMIRT()].
#'
#' @return A list with elements `itempars`, `personpars`, `loading_matrix`,
#'   `ability_corr_matrix`, `raw`, `normalized`, and `reference_model`.
#'   Each table is in long format with a `source` column, a source-specific
#'   `value` column, and a `referenceVal` column holding the aligned value from
#'   the reference model.
#' @export
compareIRTmodels <- function(models, score_method = "EAP", normaliseScale = 1,
  normaliseMean = 0){
  if(!is.list(models) || length(models) == 0L) stop("models must be a non-empty list.")
  if(is.null(names(models))) names(models) <- rep("", length(models))
  empty_names <- names(models) == ""
  names(models)[empty_names] <- paste0("model", which(empty_names))

  sim_idx <- which(vapply(models, inherits, logical(1), what = "bigIRT_simIRT"))
  if(length(sim_idx) > 1L) stop("Provide at most one simIRT object in models.")

  raw_states <- lapply(models, extractMIRTpars, score_method = score_method)
  names(raw_states) <- names(models)
  ref_idx <- if(length(sim_idx) == 1L) sim_idx[1L] else 1L

  ref_state <- raw_states[[ref_idx]]
  ref_A <- as.matrix(ref_state$A)
  ref_Ability <- as.matrix(ref_state$Ability)
  item_names <- rownames(ref_A)
  if(is.null(item_names)) item_names <- as.character(seq_len(nrow(ref_A)))
  factor_names <- colnames(ref_A)
  if(is.null(factor_names)) factor_names <- paste0("F", seq_len(ncol(ref_A)))
  person_names <- rownames(ref_Ability)
  if(is.null(person_names)) person_names <- as.character(seq_len(nrow(ref_Ability)))

  raw_states <- lapply(raw_states, bigIRT_comparison_standardize_names,
    item_names = item_names, factor_names = factor_names, person_names = person_names)

  normalized_states <- vector("list", length(raw_states))
  names(normalized_states) <- names(raw_states)
  normalized_states[[ref_idx]] <- bigIRT_comparison_state_normalised(
    raw_states[[ref_idx]],
    referenceA = NULL,
    normaliseScale = normaliseScale,
    normaliseMean = normaliseMean
  )
  ref_A <- normalized_states[[ref_idx]]$A
  for(i in seq_along(raw_states)){
    if(i == ref_idx) next
    normalized_states[[i]] <- bigIRT_comparison_state_normalised(
      raw_states[[i]],
      referenceA = ref_A,
      normaliseScale = normaliseScale,
      normaliseMean = normaliseMean
    )
  }

  itempars <- data.table::rbindlist(c(
    Map(bigIRT_comparison_itempars_dt, raw_states, names(raw_states), MoreArgs = list(form = "raw")),
    Map(bigIRT_comparison_itempars_dt, normalized_states, names(normalized_states), MoreArgs = list(form = "normalized"))
  ), use.names = TRUE, fill = TRUE)
  itempars <- bigIRT_comparison_attach_reference(
    itempars,
    c("form", "parameter", "item", "factor"),
    ref_name = names(models)[ref_idx]
  )
  data.table::setcolorder(itempars, c("source", "form", "parameter", "item", "factor", "value", "referenceVal"))

  personpars <- data.table::rbindlist(c(
    Map(bigIRT_comparison_personpars_dt, raw_states, names(raw_states), MoreArgs = list(form = "raw")),
    Map(bigIRT_comparison_personpars_dt, normalized_states, names(normalized_states), MoreArgs = list(form = "normalized"))
  ), use.names = TRUE, fill = TRUE)
  personpars <- bigIRT_comparison_attach_reference(
    personpars,
    c("form", "person", "factor"),
    ref_name = names(models)[ref_idx]
  )
  data.table::setcolorder(personpars, c("source", "form", "person", "factor", "value", "referenceVal"))

  loading_matrix <- data.table::rbindlist(c(
    Map(function(state, model, form){
      bigIRT_comparison_matrix_dt(state$A, source = model, form = form, value_name = "value")
    }, raw_states, names(raw_states), MoreArgs = list(form = "raw")),
    Map(function(state, model, form){
      bigIRT_comparison_matrix_dt(state$A, source = model, form = form, value_name = "value")
    }, normalized_states, names(normalized_states), MoreArgs = list(form = "normalized"))
  ), use.names = TRUE, fill = TRUE)
  loading_matrix <- bigIRT_comparison_attach_reference(
    loading_matrix,
    c("form", "row", "col"),
    ref_name = names(models)[ref_idx]
  )
  data.table::setcolorder(loading_matrix, c("source", "form", "row", "col", "value", "referenceVal"))

  ability_corr_matrix <- data.table::rbindlist(c(
    Map(function(state, model, form){
      est <- bigIRT_comparison_matrix_dt(state$corr, source = model, form = form, value_name = "value")
      est[, corr_type := "estimated"]
      emp_corr <- if(ncol(as.matrix(state$Ability)) > 1) {
        stats::cor(as.matrix(state$Ability), use = "pairwise.complete.obs")
      } else {
        matrix(1, 1, 1)
      }
      emp <- bigIRT_comparison_matrix_dt(emp_corr, source = model, form = form, value_name = "value")
      emp[, corr_type := "empirical"]
      data.table::rbindlist(list(est, emp), use.names = TRUE, fill = TRUE)
    }, raw_states, names(raw_states), MoreArgs = list(form = "raw")),
    Map(function(state, model, form){
      est <- bigIRT_comparison_matrix_dt(state$corr, source = model, form = form, value_name = "value")
      est[, corr_type := "estimated"]
      emp_corr <- if(ncol(as.matrix(state$Ability)) > 1) {
        stats::cor(as.matrix(state$Ability), use = "pairwise.complete.obs")
      } else {
        matrix(1, 1, 1)
      }
      emp <- bigIRT_comparison_matrix_dt(emp_corr, source = model, form = form, value_name = "value")
      emp[, corr_type := "empirical"]
      data.table::rbindlist(list(est, emp), use.names = TRUE, fill = TRUE)
    }, normalized_states, names(normalized_states), MoreArgs = list(form = "normalized"))
  ), use.names = TRUE, fill = TRUE)
  ability_corr_matrix <- bigIRT_comparison_attach_reference(
    ability_corr_matrix,
    c("form", "corr_type", "row", "col"),
    ref_name = names(models)[ref_idx]
  )
  data.table::setcolorder(ability_corr_matrix, c("source", "form", "corr_type", "row", "col", "value", "referenceVal"))

  list(
    itempars = itempars,
    personpars = personpars,
    loading_matrix = loading_matrix,
    ability_corr_matrix = ability_corr_matrix,
    # raw = raw_states,
    # normalized = normalized_states,
    reference_model = names(models)[ref_idx]
  )
}



#' Drop subjects and items with all perfect scores
#'
#' This function drops variables/items and subjects that have all perfect scores (either all 0's or all 1's) in a data table.
#'
#' @param dat The input data table
#' @param scoreref The column name of the score variable in \code{dat}
#' @param itemref The column name of the item variable in \code{dat}
#' @param idref The column name of the id variable in \code{dat}
#' @param tol Tolerance level for checking perfect scores -- .01 would drop subjects with less than 1% correct or incorrect
#'
#' @return The input data table (\code{dat}) without variables/items and subjects with all perfect scores.
#'
#' @import data.table
#' @export
#'
#' @examples
#' dat <- data.table(id=c(1,1,1,2,2,2,3,3,3), Item=c('I1','I2','I3','I1','I2','I3','I1','I2','I3'),
#'    score=c(1,0,1,0,0,0,0,1,1))
#' print(dropPerfectScores(dat))
dropPerfectScores <- function(dat,scoreref.='score',itemref.='Item',idref.='id',tol.=.001){
  if(!'data.table' %in% class(dat)) stop('Not a data.table!')

  dropping <- TRUE
  while(dropping){
    dropping <- FALSE
    dat[,itemMean:=mean(get(scoreref.)),by=itemref.]
    if(any((abs(dat$itemMean-.5)+tol.)>= .5)){
      dropping <- TRUE
      warning('Dropping items with all 0 or 1',immediate. = TRUE)
      dat <- dat[(abs(itemMean-.5)+tol.)< .5,]
    }
    dat[,personMean:= mean(get(scoreref.)),by=idref.]
    if(any((abs(dat$personMean-.5)+tol.)>= .5)){
      warning('Dropping subjects with all 0 or 1',immediate. = TRUE)
      dropping <- TRUE
      dat <-dat[(abs(personMean-.5)+tol.)< .5,]
    }
  }
  dat[,itemMean:=NULL]
  dat[,personMean:=NULL]
  dat[1,] #weirdness required to ensure return prints properly
  return(dat)
}

## Map the unconstrained Stan parameter vector into item-side and person-side blocks
## so the outer sampled-ability routine can alternate masked optimizations.
bigIRT_param_layout <- function(sdat){
  take_idx <- function(cursor, n){
    if(n <= 0) return(list(idx=integer(), cursor=cursor))
    idx <- seq.int(cursor, length.out = n)
    list(idx=idx, cursor=cursor + n)
  }

  freeAbility <- sdat$Nsubs * sdat$Nscales - sdat$NfixedAbility
  freeA <- (if(!is.null(sdat$NitemScales)) sdat$NitemScales else (sdat$Nitems * sdat$Nscales)) - sdat$NfixedA
  freeB <- sdat$Nitems - sdat$NfixedB
  freeC <- sdat$Nitems - sdat$NfixedC
  freeD <- sdat$Nitems - sdat$NfixedD
  abilityBetaPerScale <- if(freeAbility > 0) sdat$NpersonPreds else 0
  itemBetaCount <- function(freeN, predN){
    if(freeN <= 0 || predN <= 0) return(0L)
    as.integer((if(sdat$itemSpecificBetas == 1L) freeN else 1L) * predN)
  }

  cursor <- 1L
  out <- list()

  tmp <- take_idx(cursor, freeAbility); out$ability <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, if(sdat$fixedAbilityMean == 0L) sdat$Nscales else 0L); out$ability_mean <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, sdat$Nscales * abilityBetaPerScale); out$ability_beta <- tmp$idx; cursor <- tmp$cursor

  tmp <- take_idx(cursor, freeB); out$B <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, if(sdat$fixedBMean == 0L) 1L else 0L); out$B_mean <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, itemBetaCount(freeB, sdat$NBitemPreds)); out$B_beta <- tmp$idx; cursor <- tmp$cursor

  tmp <- take_idx(cursor, freeA); out$A <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, if(sdat$fixedAMean == 0L) 1L else 0L); out$A_mean <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, itemBetaCount(freeA, sdat$NAitemPreds)); out$A_beta <- tmp$idx; cursor <- tmp$cursor

  tmp <- take_idx(cursor, freeC); out$C <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, if(sdat$fixedCMean == 0L) 1L else 0L); out$C_mean <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, itemBetaCount(freeC, sdat$NCitemPreds)); out$C_beta <- tmp$idx; cursor <- tmp$cursor

  tmp <- take_idx(cursor, freeD); out$D <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, if(sdat$fixedDMean == 0L) 1L else 0L); out$D_mean <- tmp$idx; cursor <- tmp$cursor
  tmp <- take_idx(cursor, itemBetaCount(freeD, sdat$NDitemPreds)); out$D_beta <- tmp$idx

  out$item <- c(out$B,out$B_mean,out$B_beta,out$A,out$A_mean,out$A_beta,out$C,out$C_mean,out$C_beta,out$D,out$D_mean,out$D_beta)
  out$person <- c(out$ability,out$ability_mean,out$ability_beta)
  out
}

## Build and cache the expensive optimization setup for sampled-ability loops.
## This avoids rebuilding Stan target wrappers at each item/person sub-step.
bigIRT_sampled_optimizer_setup <- function(standata, cores=6, verbose=0, plot=0){
  iter <- 0L
  storedLp <- c()

  parlp <- function(parm){
    a <- Sys.time()
    out <- try(rstan::log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)
    attributes(out)$time <- Sys.time() - a
    if("try-error" %in% class(out)) {
      outerr <- out
      out <- -1e100
      attributes(out)$gradient <- rep(NaN, length(parm))
      attributes(out)$err <- outerr
    }
    if(is.null(attributes(out)$gradient)) attributes(out)$gradient <- rep(NaN, length(parm))
    attributes(out)$gradient[is.nan(attributes(out)$gradient)] <-
      rnorm(length(attributes(out)$gradient[is.nan(attributes(out)$gradient)]),0,100)
    out
  }

  if(cores == 1){
    smf <- stan_reinitsf(stanmodels$irt,standata)
    target_full <- function(parm,gradnoise=TRUE){
      iter <<- iter + 1L
      a <- Sys.time()
      out <- try(rstan::log_prob(smf,upars=parm,adjust_transform=TRUE,gradient=TRUE),silent = FALSE)
      if("try-error" %in% class(out) || is.nan(out)) {
        out <- -1e100
        attributes(out) <- list(gradient=rep(0,length(parm)))
      }
      b <- Sys.time()
      evaltime <- b-a
      if(verbose > 0 && (iter %% verbose)==0) print(paste('ll=',out[1],', prob= ',exp(out/standata$Nobs),' ,    iter time = ',round(evaltime,2)),digits=14)
      out
    }
    npars_full <- rstan::get_num_upars(smf)
    standata_genq <- standata
    standata_genq$doGenQuant <- 1L
    smf_genq <- stan_reinitsf(stanmodels$irt,standata_genq)
    cleanup <- function(){ invisible(NULL) }
  } else {
    splitby <- 'id'
    stanindices <- split(sort(unique(standata[[splitby]])),sort(unique(standata[[splitby]]) %% (cores)))
    parcommands <- list(
      "#if(length(stanindices[[nodeid]]) < length(unique(standata[[splitby]]))) ",
      "standata <- standata_specificsubjects(standata,stanindices[[nodeid]])",
      "if(!1 %in% stanindices[[nodeid]]) standata$dopriors <- 0L",
      "g = eval(parse(text=paste0('gl','obalenv()')))",
      "assign('smf',bigIRT:::stan_reinitsf(bigIRT:::stanmodels$irt,standata),pos = g)",
      "NULL"
    )

    benv <- new.env(parent=globalenv())
    benv$cl <- NA
    environment(parlp) <- environment(standata_specificsubjects) <- globalenv()
    assign(x = 'cl',
      parallel::makeCluster(spec = cores,type = "PSOCK",useXDR=FALSE,outfile='',user=NULL),
      envir = benv)
    parallel::clusterExport(benv$cl,
      c('cores','parlp','splitby','standata','stanindices','standata_specificsubjects','parcommands'),envir = environment())

    eval(parse(text=
        "parallel::parLapply(cl = cl,X = 1:cores,function(x){
         assign('nodeid',x,envir=globalenv())
        })"),envir=benv)
    parallel::clusterEvalQ(cl = benv$cl,expr = sapply(parcommands,function(x) eval(parse(text=x),envir = globalenv())))

    target_full <- function(parm,gradnoise=TRUE){
      iter <<- iter + 1L
      a <- Sys.time()
      parallel::clusterExport(benv$cl,'parm',envir = environment())
      out2 <- parallel::clusterEvalQ(benv$cl,parlp(parm))

      sapply(seq_along(out2),function(x){
        if(!is.null(attributes(out2[[x]])$err)){
          if(length(out2) > 1 && as.logical(verbose)) message('Error on core ', x,' but continuing:')
          message(attributes(out2[[x]])$err)
        }
      })

      out <- try(sum(unlist(out2)),silent=TRUE)
      for(i in seq_along(out2)){
        if(i==1) attributes(out)$gradient <- attributes(out2[[1]])$gradient
        if(i>1) attributes(out)$gradient <- attributes(out)$gradient+attributes(out2[[i]])$gradient
      }

      if('try-error' %in% class(out) || is.nan(out)) {
        out <- -1e100
        attributes(out) <- list(gradient=rep(0,length(parm)))
      }

      if(plot > 0){
        storedLp <<- c(storedLp,ifelse(out[1] > (-1e99),out[1],NA))
        if(iter %% plot == 0){
          par(mfrow=c(1,1))
          tmp <- try(plot(tail(1:iter,500), tail(exp(storedLp/standata$Nobs),500),ylab='target',type='l'))
          if('try-error' %in% class(tmp)) browser()
        }
      }
      b <- Sys.time()
      if(verbose > 0  && (iter %% verbose)==0) print(paste0('ll=',out[1],', mean p= ',exp(out/standata$Nobs),' , iter time = ',round(b-a,5),
        ' , core timerange = ',paste0(range(sapply(out2,function(x) round(attributes(x)$time,3))),collapse=' : ')))
      out
    }
    npars_full <- parallel::clusterEvalQ(benv$cl, rstan::get_num_upars(smf))[[1]]
    standata_genq <- standata
    standata_genq$doGenQuant <- 1L
    smf_genq <- stan_reinitsf(stanmodels$irt,standata_genq)
    cleanup <- function() try({parallel::stopCluster(benv$cl)},silent=TRUE)
  }

  list(
    standata = standata,
    target_full = target_full,
    npars_full = npars_full,
    smf_genq = smf_genq,
    cleanup = cleanup
  )
}

## Build a sampled-step objective once so repeated target evaluations inside mize
## do not rebuild block/sample bookkeeping.
bigIRT_sampled_make_objective <- function(engine, free_par_index=NULL,
  fixed_par_full=NULL, fixed_par_samples=NULL, sample_weights=NULL){
  if(is.null(fixed_par_samples)) fixed_par_samples <- list()
  has_mask <- !is.null(free_par_index)
  has_samples <- length(fixed_par_samples) > 0
  Nsamp <- if(has_samples) length(fixed_par_samples) else 1L
  weights <- if(is.null(sample_weights)) rep(1 / Nsamp, Nsamp) else sample_weights / sum(sample_weights)
  npars <- if(has_mask) length(free_par_index) else engine$npars_full

  build_full_par <- function(free_par, sample_id = 1L){
    if(!has_mask) return(free_par)
    base_par <- fixed_par_full
    if(sample_id > 0L && has_samples){
      base_par <- fixed_par_samples[[sample_id]]
    }
    base_par[free_par_index] <- free_par
    base_par
  }

  target <- function(parm){
    if(!has_mask && !has_samples){
      return(engine$target_full(parm,gradnoise=TRUE))
    }

    out <- 0
    grad <- NULL
    for(si in seq_len(Nsamp)){
      tsi <- engine$target_full(build_full_par(parm, si),gradnoise=TRUE)
      gsi <- attributes(tsi)$gradient
      if(!is.null(free_par_index)) gsi <- gsi[free_par_index]
      out <- out + weights[si] * tsi[1]
      if(is.null(grad)) grad <- weights[si] * gsi else grad <- grad + weights[si] * gsi
    }
    attributes(out) <- list(gradient=grad)
    out
  }

  list(
    target = target,
    build_full_par = build_full_par,
    npars = npars,
    Nsamp = Nsamp,
    has_mask = has_mask,
    has_samples = has_samples,
    free_par_index = free_par_index
  )
}

## Run one optimizer call for a pre-built sampled-ability objective. By default
## this can skip generated-quantity materialization so rejected proposals remain cheap.
bigIRT_sampled_optimizer_step <- function(engine, objective, tol=1e-2, Niter=2000, init=NA,
  materialize_fit = TRUE){
  target <- objective$target
  npars <- objective$npars
  eval_count <- 0L
  target_counted <- function(parm){
    eval_count <<- eval_count + 1L
    target(parm)
  }
  if(is.na(init[1])) init <- rnorm(npars,0,.1)
  mizelpg <- list(
    fg=function(pars){
      r <- -target_counted(pars)
      list(fn=r[1],gr= -attributes(r)$gradient)
    },
    fn=function(x) -target_counted(x),
    gr=function(pars) -attributes(target_counted(pars))$gradient
  )
  if(Niter <= 1){
    # Strict budget mode for profiling/debugging: one gradient evaluation and
    # one conservative normalized ascent step (no line-search loop).
    r0 <- target_counted(init)
    g0 <- attributes(r0)$gradient
    gnorm <- sqrt(sum(g0^2))
    if(is.finite(gnorm) && gnorm > 0){
      step_scale <- 0.01
      par1 <- init + step_scale * g0 / gnorm
    } else {
      par1 <- init
    }
    optimfit <- list(
      par = par1,
      value = r0[1],
      niter = 1L,
      convergence = 0L,
      method = "single_grad_step"
    )
    final_grad <- g0
  } else {
    ls_max_fn <- max(1L, min(20L, as.integer(Niter)))
    optimfit <- mize::mize(init, fg=mizelpg, max_iter=Niter,
      method="L-BFGS",memory=100,
      line_search='Schmidt',c1=1e-10,c2=.9,step0='schmidt',ls_max_fn=ls_max_fn,
      abs_tol=tol,grad_tol=0,rel_tol=0,step_tol=0,ginf_tol=0)
    final_grad <- NULL
  }

  final_full_par <- if(!objective$has_mask && !objective$has_samples){
    optimfit$par
  } else if(objective$has_mask) {
    objective$build_full_par(optimfit$par, 0L)
  } else {
    objective$build_full_par(optimfit$par, 1L)
  }
  if(is.null(final_grad)){
    final_eval <- target_counted(if(objective$has_mask) optimfit$par else final_full_par)
    final_grad <- attributes(final_eval)$gradient
    optimfit$logLik <- final_eval[1]
  } else {
    optimfit$logLik <- r0[1]
  }
  optimfit$masked_grad_norm <- sqrt(sum(final_grad^2))
  optimfit$target_evals <- eval_count
  optimfit$logprob_evals <- eval_count * objective$Nsamp
  optimfit$par <- final_full_par

  if(!isTRUE(materialize_fit)){
    return(list(
      optim=optimfit,
      stanfit=engine$smf_genq,
      pars=NULL,
      dat=engine$standata
    ))
  }

  list(
    optim=optimfit,
    stanfit=engine$smf_genq,
    pars=rstan::constrain_pars(object = engine$smf_genq, final_full_par),
    dat=engine$standata
  )
}

## Build sampled-ability tuning settings from flat arguments plus an optional
## override list so the current API stays backward compatible.
## Inputs: user-facing sampled-ability controls.
## Returns: one validated control list; mutates nothing.
bigIRT_sampled_build_control <- function(sampledAbilitySigmaScale = 0.25,
  sampledAbilityStepTol = 1e-3, sampledAbilitySpreadTol = 0.02,
  sampledAbilityPatience = 3L, sampledAbilityControl = NULL, noptimgradtol = 1e-2){

  control <- list(
    sigma_scale_init = as.numeric(sampledAbilitySigmaScale),
    sigma_scale_min = 0.05,
    sigma_scale_max = 0.5,
    sigma_scale_expand = 1.1,
    sigma_scale_shrink = 0.5,
    step_damping_init = 1.0,
    step_damping_min = 0.0625,
    max_backtracks = 5L,
    noptimgradtol = as.numeric(noptimgradtol),
    sampledAbilityStepTol = as.numeric(sampledAbilityStepTol),
    sampledAbilitySpreadTol = as.numeric(sampledAbilitySpreadTol),
    sampledAbilityPatience = as.integer(sampledAbilityPatience),
    max_rejected_outer = 5L,
    max_worsening_outer = 10L
  )
  if(is.list(sampledAbilityControl) && length(sampledAbilityControl)){
    control[names(sampledAbilityControl)] <- sampledAbilityControl
  }

  control$max_backtracks <- max(1L, as.integer(control$max_backtracks))
  control$sampledAbilityPatience <- max(1L, as.integer(control$sampledAbilityPatience))
  control$max_rejected_outer <- max(1L, as.integer(control$max_rejected_outer))
  control$max_worsening_outer <- max(1L, as.integer(control$max_worsening_outer))
  control$sigma_scale_min <- max(1e-8, as.numeric(control$sigma_scale_min))
  control$sigma_scale_max <- max(control$sigma_scale_min, as.numeric(control$sigma_scale_max))
  control$sigma_scale_expand <- max(1, as.numeric(control$sigma_scale_expand))
  control$sigma_scale_shrink <- min(max(as.numeric(control$sigma_scale_shrink), 1e-8), 0.999)
  control$step_damping_init <- max(1e-8, as.numeric(control$step_damping_init))
  control$step_damping_min <- min(control$step_damping_init, max(1e-8, as.numeric(control$step_damping_min)))
  control$sigmaScale <- min(max(as.numeric(control$sigma_scale_init), control$sigma_scale_min), control$sigma_scale_max)
  control
}

## Evaluate a sampled-ability objective at a full unconstrained parameter vector.
## The objective itself may operate on only a masked subset of coordinates.
## Inputs: objective contract from bigIRT_sampled_make_objective() and a full
## unconstrained parameter vector.
## Returns: scalar target value; mutates nothing.
bigIRT_sampled_eval_objective <- function(objective, full_par){
  parm <- if(objective$has_mask) full_par[objective$free_par_index] else full_par
  objective$target(parm)[1]
}

## Materialize a full fit object only once a sampled-ability proposal is accepted,
## or when diagnostics explicitly require generated quantities.
## Inputs: cached optimizer engine plus a full unconstrained parameter vector.
## Returns: fit-like list with constrained pars; mutates nothing outside the
## returned object.
bigIRT_sampled_materialize_fit <- function(engine, full_par, optim=NULL){
  if(is.null(optim)) optim <- list(par = full_par)
  optim$par <- full_par
  list(
    optim = optim,
    stanfit = engine$smf_genq,
    pars = rstan::constrain_pars(object = engine$smf_genq, full_par),
    dat = engine$standata
  )
}

## Apply a damped acceptance rule to an already-optimized block proposal. This
## keeps the outer loop stable without rerunning the optimizer at each backtrack.
## Inputs: current and proposed full parameter vectors plus a sampled objective.
## Returns: accepted/rejected proposal summary and the accepted full vector if any;
## mutates nothing.
bigIRT_sampled_accept_proposal <- function(current_par, proposed_par, objective, control){
  idx <- if(objective$has_mask) objective$free_par_index else seq_along(current_par)
  objective_before <- bigIRT_sampled_eval_objective(objective, current_par)
  damping <- control$step_damping_init
  attempts <- 0L
  accepted <- FALSE
  accepted_par <- current_par
  last_trial <- current_par
  objective_after <- objective_before
  reject_reason <- "objective_decrease"

  while(attempts < control$max_backtracks && damping >= control$step_damping_min){
    attempts <- attempts + 1L
    trial_par <- current_par
    trial_par[idx] <- current_par[idx] + damping * (proposed_par[idx] - current_par[idx])
    trial_obj <- bigIRT_sampled_eval_objective(objective, trial_par)
    last_trial <- trial_par
    objective_after <- trial_obj
    if(is.finite(trial_obj) && trial_obj >= (objective_before - 1e-8 * max(1, abs(objective_before)))){
      accepted <- TRUE
      accepted_par <- trial_par
      reject_reason <- NA_character_
      break
    }
    damping <- damping * 0.5
  }

  if(!accepted && !is.finite(objective_after)) reject_reason <- "non_finite_objective"

  list(
    accepted = accepted,
    full_par = accepted_par,
    trial_par = last_trial,
    damping = if(accepted) damping else max(damping, control$step_damping_min),
    objective_before = objective_before,
    objective_after = objective_after,
    attempts = attempts,
    reject_reason = reject_reason
  )
}

## Compare two gradient norms while handling missing or non-finite values.
## Inputs: current and reference gradient norms.
## Returns: TRUE only when the current norm is finite and no better than the
## reference norm; mutates nothing.
bigIRT_sampled_gradient_worsened <- function(current_grad, reference_grad){
  is.finite(current_grad) && is.finite(reference_grad) &&
    current_grad > (reference_grad + 1e-8 * max(1, abs(reference_grad)))
}

## Compare two gradient norms and treat the current one as improved when it is
## no larger than the reference up to numerical tolerance.
## Inputs: current and reference gradient norms.
## Returns: TRUE when the current norm is no worse than the reference; mutates nothing.
bigIRT_sampled_gradient_improved <- function(current_grad, reference_grad){
  is.finite(current_grad) && (!is.finite(reference_grad) ||
    current_grad <= (reference_grad + 1e-8 * max(1, abs(reference_grad))))
}

## Flag posterior-spread blow-up from the accepted-iteration baseline.
## Inputs: relative posterior-SD summaries.
## Returns: TRUE when either configured spread guard is exceeded; mutates nothing.
bigIRT_sampled_spread_explosion <- function(mean_ratio, max_ratio){
  is.finite(mean_ratio) && is.finite(max_ratio) &&
    (mean_ratio > 1.2 || max_ratio > 1.35)
}

## Check whether posterior spread is stable enough to allow sigma expansion and
## convergence accounting.
## Inputs: relative posterior-SD summaries plus sampled-ability controls.
## Returns: TRUE when posterior spread remains within the configured tolerance;
## mutates nothing.
bigIRT_sampled_spread_stable <- function(mean_ratio, max_ratio, control){
  is.finite(mean_ratio) && abs(mean_ratio - 1) <= control$sampledAbilitySpreadTol &&
    is.finite(max_ratio) && max_ratio <= 1.35
}

## Update the adaptive sigma scale after one outer iteration.
## Inputs: control list, sigma used this iteration, and acceptance diagnostics.
## Returns: next sigma scale clipped to configured bounds; mutates nothing.
bigIRT_sampled_update_sigma_scale <- function(control, sigmaScaleUsed,
  accepted, gradientsImproved = FALSE, gradientsWorsened = FALSE, spreadStable = FALSE){

  if(isTRUE(accepted) && isTRUE(gradientsImproved) && isTRUE(spreadStable)){
    return(min(control$sigma_scale_max, sigmaScaleUsed * control$sigma_scale_expand))
  }
  if(isTRUE(accepted) && isTRUE(gradientsWorsened)){
    return(max(control$sigma_scale_min, sigmaScaleUsed * control$sigma_scale_shrink))
  }
  if(isTRUE(accepted)){
    return(min(max(sigmaScaleUsed, control$sigma_scale_min), control$sigma_scale_max))
  }
  max(control$sigma_scale_min, sigmaScaleUsed * control$sigma_scale_shrink)
}

## Build a small deterministic support around the current person estimate.
## These points are used only in the item step to average over person uncertainty.
bigIRT_sigma_points <- function(mu, Sigma, jitter = 1e-6, sigmaScale = 0.25){
  K <- length(mu)
  if(K == 1){
    Sigma <- matrix(Sigma,1,1)
  }
  Sigma <- (Sigma + t(Sigma)) / 2
  cholSigma <- try(chol(Sigma + diag(jitter,K)), silent = TRUE)
  if("try-error" %in% class(cholSigma)){
    cholSigma <- chol(diag(pmax(diag(Sigma), jitter), K))
  }
  lambda <- 1
  scale <- sigmaScale * sqrt(K + lambda)
  points <- matrix(rep(mu, each = (2 * K + 1)), nrow = 2 * K + 1, byrow = FALSE)
  for(k in seq_len(K)){
    offset <- scale * cholSigma[,k]
    points[1 + k,] <- mu + offset
    points[1 + K + k,] <- mu - offset
  }
  weights <- c(lambda / (K + lambda), rep(1 / (2 * (K + lambda)), 2 * K))
  list(points=points,weights=weights)
}

## Approximate each person's local posterior with a Gaussian centered on the
## current person estimates and scaled by the local curvature returned by Stan.
## Inputs: accepted fit state and standata.
## Returns: person-wise Gaussian posterior summaries; mutates nothing.
bigIRT_person_posterior <- function(fit, sdat, jitter = 1e-6, priorPrec = NULL){
  if(is.null(priorPrec)){
    priorSD <- pmax(as.numeric(sdat$AbilitySD), jitter)
    priorCov <- diag(priorSD, length(priorSD)) %*% sdat$AbilityCorr %*% diag(priorSD, length(priorSD))
    priorPrec <- solve(priorCov + diag(jitter, nrow(priorCov)))
  }
  meanMat <- fit$pars$Ability
  if(is.null(dim(meanMat))) meanMat <- matrix(meanMat, ncol = sdat$Nscales)

  useRowEffective <- all(c("b_row", "c_row", "d_row", "row_loadings") %in% names(fit$pars))
  if(useRowEffective){
    rowLoadings <- fit$pars$row_loadings
    if(nrow(rowLoadings) != sdat$Nobs || ncol(rowLoadings) != sdat$Nscales){
      stop("Unexpected row-effective loading matrix dimensions from Stan.")
    }
    covObj <- personCovarianceMatrices(
      id = sdat$id,
      theta_mean = meanMat,
      b = fit$pars$b_row,
      c = fit$pars$c_row,
      d = fit$pars$d_row,
      loadings = rowLoadings,
      prior_precision = priorPrec,
      jitter = jitter,
      return_precision = FALSE
    )
    covList <- vector("list", sdat$Nsubs)
    for(i in seq_len(sdat$Nsubs)){
      covList[[i]] <- covObj$covariance[,,i]
    }
    backend <- covObj$backend
    rowEff <- list(
      id = sdat$id,
      theta_mean = meanMat,
      b = fit$pars$b_row,
      c = fit$pars$c_row,
      d = fit$pars$d_row,
      loadings = rowLoadings,
      source = "stan_gq"
    )
  } else {
    likeSD <- fit$pars$sAbilitySD
    if(is.null(dim(likeSD))) likeSD <- matrix(likeSD, ncol = sdat$Nscales)
    covList <- vector("list", sdat$Nsubs)
    for(i in seq_len(sdat$Nsubs)){
      likPrecDiag <- rep(0, sdat$Nscales)
      valid <- is.finite(likeSD[i,]) & likeSD[i,] > jitter
      likPrecDiag[valid] <- 1 / (likeSD[i,valid]^2)
      covList[[i]] <- solve(priorPrec + diag(likPrecDiag, sdat$Nscales) + diag(jitter, sdat$Nscales))
    }
    rowEff <- NULL
    backend <- "legacy_sAbilitySD"
  }

  postMean <- colMeans(meanMat)
  postCov <- matrix(0, nrow = sdat$Nscales, ncol = sdat$Nscales)
  for(i in seq_len(sdat$Nsubs)) postCov <- postCov + covList[[i]]
  postCov <- postCov / sdat$Nsubs
  postCov <- postCov + crossprod(meanMat) / sdat$Nsubs - tcrossprod(postMean)
  postSD <- sqrt(pmax(diag(postCov), jitter))
  postCorr <- cov2cor(postCov + diag(jitter, sdat$Nscales))

  list(
    mean = meanMat,
    cov = covList,
    meanPrior = postMean,
    sdPrior = postSD,
    corrPrior = postCorr,
    rowEffective = rowEff,
    backend = backend
  )
}

## Refresh the Laplace posterior and sigma-point templates from the current fit.
## Inputs: accepted fit state, standata, parameter layout, and sigma settings.
## Returns: posterior summaries plus sigma-point full-parameter templates;
## mutates nothing.
bigIRT_person_posterior_and_sigma <- function(fit, sdat, layout, jitter = 1e-6,
  sigmaScale = 0.25, priorPrec = NULL){
  if(is.null(priorPrec)){
    priorSD <- pmax(as.numeric(sdat$AbilitySD), jitter)
    priorCov <- diag(priorSD, length(priorSD)) %*% sdat$AbilityCorr %*% diag(priorSD, length(priorSD))
    priorPrec <- solve(priorCov + diag(jitter, nrow(priorCov)))
  }

  useRowEffective <- all(c("b_row", "c_row", "d_row", "row_loadings") %in% names(fit$pars))
  if(!useRowEffective){
    posterior <- bigIRT_person_posterior(fit, sdat, jitter = jitter, priorPrec = priorPrec)
    sigmaTemplates <- bigIRT_sigma_templates(fit, sdat, posterior, layout, jitter = jitter, sigmaScale = sigmaScale)
    return(list(posterior = posterior, sigmaTemplates = sigmaTemplates))
  }

  rowLoadings <- fit$pars$row_loadings
  if(nrow(rowLoadings) != sdat$Nobs || ncol(rowLoadings) != sdat$Nscales){
    stop("Unexpected row-effective loading matrix dimensions from Stan.")
  }
  meanMat <- fit$pars$Ability
  if(is.null(dim(meanMat))) meanMat <- matrix(meanMat, ncol = sdat$Nscales)
  sigmaObj <- bigIRT_person_sigma_points_cpp(
    id = sdat$id,
    theta_mean = meanMat,
    b = fit$pars$b_row,
    c = fit$pars$c_row,
    d = fit$pars$d_row,
    loadings = rowLoadings,
    prior_precision = priorPrec,
    jitter = jitter,
    sigma_scale = sigmaScale
  )

  abilitySamples <- sigmaObj$ability_samples
  Nsamp <- dim(abilitySamples)[3]
  abilityMask <- fit$dat$Abilityparsindex > 0
  fullSamples <- vector("list", Nsamp)
  for(si in seq_len(Nsamp)){
    fullPar <- fit$optim$par
    fullPar[layout$ability] <- abilitySamples[,,si][abilityMask]
    fullSamples[[si]] <- fullPar
  }
  sigmaTemplates <- list(samples = fullSamples, weights = as.numeric(sigmaObj$weights))

  meanMat <- as.matrix(meanMat)
  postMean <- colMeans(meanMat)
  postCov <- as.matrix(sigmaObj$cov_mean) + crossprod(meanMat) / sdat$Nsubs - tcrossprod(postMean)
  postSD <- sqrt(pmax(diag(postCov), jitter))
  postCorr <- cov2cor(postCov + diag(jitter, sdat$Nscales))
  posterior <- list(
    posteriorSDMat = as.matrix(sigmaObj$posterior_sd),
    meanPrior = postMean,
    sdPrior = postSD,
    corrPrior = postCorr,
    backend = sigmaObj$backend
  )

  list(posterior = posterior, sigmaTemplates = sigmaTemplates)
}

## Extract scalar summaries from the current sampled-ability posterior so the
## outer loop can detect uncertainty blow-up without storing dense matrices.
bigIRT_sampled_posterior_metrics <- function(posterior){
  posteriorSD <- if(!is.null(posterior$posteriorSDMat)){
    as.numeric(posterior$posteriorSDMat)
  } else if(!is.null(posterior$cov)) {
    unlist(lapply(posterior$cov, function(x) sqrt(pmax(diag(x), 0))))
  } else numeric()

  list(
    meanPosteriorSD = if(length(posteriorSD)) mean(posteriorSD, na.rm = TRUE) else NA_real_,
    maxPosteriorSD = if(length(posteriorSD)) max(posteriorSD, na.rm = TRUE) else NA_real_
  )
}

## Update convergence bookkeeping after an accepted sampled-ability outer step.
## Inputs: prior accepted-window list and one accepted-iteration metrics record.
## Returns: truncated accepted-window list; mutates nothing.
bigIRT_sampled_update_window <- function(window, metrics, control){
  window[[length(window) + 1L]] <- metrics
  if(length(window) > control$sampledAbilityPatience){
    window <- window[(length(window) - control$sampledAbilityPatience + 1L):length(window)]
  }
  window
}

## Check whether the accepted sampled-ability outer-step window is small enough
## to treat the alternating scheme as converged.
## Inputs: accepted-window history and sampled-ability controls.
## Returns: TRUE only when the full accepted window satisfies all tolerances;
## mutates nothing.
bigIRT_sampled_window_converged <- function(window, control){
  if(length(window) < control$sampledAbilityPatience) return(FALSE)
  all(vapply(window, function(x){
    isTRUE(x$accepted) &&
      is.finite(x$combinedGradNorm) && x$combinedGradNorm < control$noptimgradtol &&
      is.finite(x$itemStepRms) && x$itemStepRms < control$sampledAbilityStepTol &&
      is.finite(x$personStepRms) && x$personStepRms < control$sampledAbilityStepTol &&
      is.finite(x$meanPosteriorSD_ratio) && abs(x$meanPosteriorSD_ratio - 1) < control$sampledAbilitySpreadTol
  }, logical(1)))
}

## Convert person-level sigma points into full unconstrained parameter vectors
## with only the ability coordinates replaced, leaving item parameters intact.
## Inputs: accepted fit state, posterior object, and parameter layout.
## Returns: weighted full-parameter templates for the item step; mutates nothing.
bigIRT_sigma_templates <- function(fit, sdat, posterior, layout, jitter = 1e-6, sigmaScale = 0.25){
  if(length(layout$ability) == 0) return(list(samples=list(fit$optim$par),weights=1))

  pointTemplate <- bigIRT_sigma_points(
    as.numeric(posterior$mean[1,,drop=FALSE]),
    posterior$cov[[1]],
    jitter = jitter,
    sigmaScale = sigmaScale
  )
  Nsamp <- nrow(pointTemplate$points)
  abilitySamples <- array(0, dim = c(sdat$Nsubs, sdat$Nscales, Nsamp))
  weights <- pointTemplate$weights

  for(i in seq_len(sdat$Nsubs)){
    sp <- bigIRT_sigma_points(
      as.numeric(posterior$mean[i,,drop=FALSE]),
      posterior$cov[[i]],
      jitter = jitter,
      sigmaScale = sigmaScale
    )
    abilitySamples[i,,] <- matrix(t(sp$points), nrow = sdat$Nscales, ncol = Nsamp)
  }

  fullSamples <- lapply(seq_len(Nsamp), function(si){
    fullPar <- fit$optim$par
    sampleSlice <- matrix(abilitySamples[,,si], nrow = sdat$Nsubs, ncol = sdat$Nscales)
    fullPar[layout$ability] <- sampleSlice[fit$dat$Abilityparsindex > 0]
    fullPar
  })

  list(samples=fullSamples,weights=weights)
}

## Summarize each sampled-ability sub-step so failures are visible after fitting.
## Inputs: fit-like object plus outer-loop bookkeeping for one sub-step.
## Returns: one diagnostics row; mutates nothing.
bigIRT_sampled_diag_snapshot <- function(fit, layout, stage, outerIter,
  posterior = NULL, sigmaScale = NA_real_, itemGradNorm = NA_real_,
  personGradNorm = NA_real_, prevPar = NULL,
  accepted = NA, rejected = NA, reject_reason = NA_character_,
  sigmaScaleUsed = NA_real_, personStepDamping = NA_real_,
  itemStepDamping = NA_real_, personObjectiveBefore = NA_real_,
  personObjectiveAfter = NA_real_, itemObjectiveBefore = NA_real_,
  itemObjectiveAfter = NA_real_, meanPosteriorSD_ratio = NA_real_,
  maxPosteriorSD_ratio = NA_real_, cumulativeMeanPosteriorSD_ratio = NA_real_,
  cumulativeMaxPosteriorSD_ratio = NA_real_, sdAbility_ratio = NA_real_){

  abilityMat <- fit$pars$Ability
  if(is.null(dim(abilityMat))) abilityMat <- matrix(abilityMat, ncol = fit$dat$Nscales)
  likeSD <- fit$pars$sAbilitySD
  if(is.null(dim(likeSD))) likeSD <- matrix(likeSD, ncol = fit$dat$Nscales)

  itemStepRms <- NA_real_
  personStepRms <- NA_real_
  if(!is.null(prevPar)){
    if(length(layout$item) > 0) itemStepRms <- sqrt(mean((fit$optim$par[layout$item] - prevPar[layout$item])^2))
    if(length(layout$person) > 0) personStepRms <- sqrt(mean((fit$optim$par[layout$person] - prevPar[layout$person])^2))
  }

  posteriorSD <- if(!is.null(posterior)) {
    if(!is.null(posterior$posteriorSDMat)){
      as.numeric(posterior$posteriorSDMat)
    } else if(!is.null(posterior$cov)) {
      unlist(lapply(posterior$cov, function(x) sqrt(pmax(diag(x), 0))))
    } else numeric()
  } else numeric()

  data.frame(
    outerIter = outerIter,
    stage = stage,
    logLik = if(!is.null(fit$optim$logLik)) fit$optim$logLik else NA_real_,
    meanA = mean(fit$pars$A, na.rm = TRUE),
    medianA = stats::median(fit$pars$A, na.rm = TRUE),
    minA = min(fit$pars$A, na.rm = TRUE),
    maxA = max(fit$pars$A, na.rm = TRUE),
    meanB = mean(fit$pars$B, na.rm = TRUE),
    sdB = stats::sd(fit$pars$B, na.rm = TRUE),
    meanAbility = mean(abilityMat, na.rm = TRUE),
    sdAbility = stats::sd(as.numeric(abilityMat), na.rm = TRUE),
    meanLocalSE = mean(likeSD, na.rm = TRUE),
    meanPosteriorSD = if(length(posteriorSD)) mean(posteriorSD, na.rm = TRUE) else NA_real_,
    maxPosteriorSD = if(length(posteriorSD)) max(posteriorSD, na.rm = TRUE) else NA_real_,
    sigmaScale = sigmaScale,
    accepted = accepted,
    rejected = rejected,
    reject_reason = if(length(reject_reason)) reject_reason else NA_character_,
    sigmaScaleUsed = sigmaScaleUsed,
    personStepDamping = personStepDamping,
    itemStepDamping = itemStepDamping,
    personObjectiveBefore = personObjectiveBefore,
    personObjectiveAfter = personObjectiveAfter,
    itemObjectiveBefore = itemObjectiveBefore,
    itemObjectiveAfter = itemObjectiveAfter,
    meanPosteriorSD_ratio = meanPosteriorSD_ratio,
    maxPosteriorSD_ratio = maxPosteriorSD_ratio,
    cumulativeMeanPosteriorSD_ratio = cumulativeMeanPosteriorSD_ratio,
    cumulativeMaxPosteriorSD_ratio = cumulativeMaxPosteriorSD_ratio,
    sdAbility_ratio = sdAbility_ratio,
    itemGradNorm = itemGradNorm,
    personGradNorm = personGradNorm,
    combinedGradNorm = sqrt(sum(c(itemGradNorm, personGradNorm)^2, na.rm = TRUE)),
    itemStepRms = itemStepRms,
    personStepRms = personStepRms
  )
}

bigIRT_plot_sampled_diag_df <- function(diagdf, logGrad = TRUE,
  main = "Sampled Ability Diagnostics", showExtra = TRUE){
  if(is.null(diagdf) || nrow(diagdf) == 0) return(invisible(NULL))

  stageCols <- c(init = "grey40", item = "firebrick3", person = "steelblue3")
  cols <- stageCols[diagdf$stage]
  x <- seq_len(nrow(diagdf))
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  use_extra <- isTRUE(showExtra) && all(c("accepted", "sigmaScaleUsed", "meanPosteriorSD_ratio") %in% names(diagdf))
  graphics::par(mfrow = if(use_extra) c(3,3) else c(2,2), mar = c(4,4,2,1))

  grady <- diagdf$combinedGradNorm
  if(logGrad) grady <- log(grady+1)
  graphics::plot(x, grady, type = "b", pch = 19, col = cols,
    xlab = "Update", ylab = if(logGrad) "log(gradient norm +1)" else "gradient norm",
    main = paste(main, "Gradients"))
  graphics::legend("topright", legend = names(stageCols), col = stageCols, pch = 19, bty = "n")

  graphics::plot(x, diagdf$meanA, type = "b", pch = 19, col = cols,
    xlab = "Update", ylab = "A summary", main = paste(main, "A Parameters"))
  graphics::lines(x, diagdf$medianA, type = "b", pch = 1, col = cols)
  graphics::lines(x, diagdf$minA, type = "b", pch = 0, col = cols)

  graphics::plot(x, diagdf$sdAbility, type = "b", pch = 19, col = cols,
    xlab = "Update", ylab = "Ability spread", main = paste(main, "Ability Spread"))
  graphics::lines(x, diagdf$meanLocalSE, type = "b", pch = 1, col = cols)
  if(any(is.finite(diagdf$meanPosteriorSD))){
    graphics::lines(x, diagdf$meanPosteriorSD, type = "b", pch = 0, col = cols)
  }

  stepy <- pmax(diagdf$itemStepRms, diagdf$personStepRms, na.rm = TRUE)
  if(all(!is.finite(stepy))) stepy <- rep(NA_real_, nrow(diagdf))
  graphics::plot(x, diagdf$itemStepRms, type = "b", pch = 19, col = cols,
    xlab = "Update", ylab = "Step RMS", main = paste(main, "Parameter Movement"))
  graphics::lines(x, diagdf$personStepRms, type = "b", pch = 1, col = cols)

  if(use_extra){
    accept_y <- ifelse(diagdf$accepted %in% TRUE, 1, ifelse(diagdf$rejected %in% TRUE, 0, NA_real_))
    graphics::plot(x, accept_y, type = "h", lwd = 2, col = cols,
      xlab = "Update", ylab = "Accepted", ylim = c(-0.1, 1.1),
      main = paste(main, "Acceptance"))
    graphics::axis(2, at = c(0, 1), labels = c("reject", "accept"))

    graphics::plot(x, diagdf$sigmaScaleUsed, type = "b", pch = 19, col = cols,
      xlab = "Update", ylab = "Sigma scale", main = paste(main, "Sigma Scale"))

    graphics::plot(x, diagdf$meanPosteriorSD_ratio, type = "b", pch = 19, col = cols,
      xlab = "Update", ylab = "Posterior SD ratio", main = paste(main, "Posterior Spread"))
    graphics::lines(x, diagdf$maxPosteriorSD_ratio, type = "b", pch = 1, col = cols)
    graphics::abline(h = c(1.2, 1.35), lty = 2, col = "grey50")

    valid_ll <- is.finite(diagdf$logLik)
    if(any(valid_ll)){
      ll_min <- min(-diagdf$logLik[valid_ll])
      ll_shift <- log(1+(-diagdf$logLik-min(nll)))
      print(diagdf$logLik)
      graphics::plot(x, ll_shift, type = "b", pch = 19, col = cols,
        xlab = "Update", ylab = "log(1 + logLik - min(logLik))",
        main = paste(main, "Log-Likelihood (scaled)"))
      graphics::legend("topleft", legend = names(stageCols), col = stageCols, pch = 19, bty = "n")

      person_ll <- ifelse(diagdf$stage %in% "person", ll_shift, NA_real_)
      item_ll <- ifelse(diagdf$stage %in% "item", ll_shift, NA_real_)
      graphics::plot(x, person_ll, type = "b", pch = 19, col = stageCols["person"],
        xlab = "Update", ylab = "log(1 + logLik - min(logLik))",
        main = paste(main, "Person vs Item Log-Likelihood"))
      graphics::lines(x, item_ll, type = "b", pch = 1, col = stageCols["item"])
      graphics::legend("topleft", legend = c("person", "item"),
        col = c(stageCols["person"], stageCols["item"]), pch = c(19, 1), bty = "n")
    } else {
      graphics::plot.new()
      graphics::title(main = paste(main, "Log-Likelihood (scaled)"))
      graphics::mtext("No finite logLik values", side = 3, line = -1.5)
      graphics::plot.new()
      graphics::title(main = paste(main, "Person vs Item Log-Likelihood"))
      graphics::mtext("No finite logLik values", side = 3, line = -1.5)
    }
  }

  invisible(diagdf)
}

#' Plot sampled-ability diagnostics
#'
#' @param fit A fitted \code{bigIRT} model returned by \code{fitIRT()} with
#'   \code{sampledAbilityStep=TRUE}.
#' @param logGrad Whether to plot gradient norms on the log10 scale.
#' @param showExtra Whether to include acceptance, sigma-scale, spread-ratio,
#'   and transformed raw log-likelihood panels when those diagnostics are available.
#'
#' @return Invisibly returns the diagnostic data frame used for plotting.
#' @export
plotSampledAbilityDiagnostics <- function(fit, logGrad = TRUE, showExtra = TRUE){
  if(is.null(fit$sampledAbilityDiagnostics) || nrow(fit$sampledAbilityDiagnostics) == 0){
    stop("No sampled-ability diagnostics found on fit object.")
  }
  bigIRT_plot_sampled_diag_df(fit$sampledAbilityDiagnostics, logGrad = logGrad, showExtra = showExtra)
}

bigIRT_plot_laplace_diag_df <- function(diagdf, logGrad = TRUE, showTiming = TRUE){
  if(is.null(diagdf) || nrow(diagdf) == 0) return(invisible(NULL))

  x <- seq_len(nrow(diagdf))
  cols <- grDevices::colorRampPalette(c("#173f5f", "#20639b", "#3caea3", "#f6d55c", "#ed553b"))(max(2, nrow(diagdf)))
  grady <- if(logGrad) log1p(pmax(diagdf$itemGradNorm, 0)) else diagdf$itemGradNorm
  panels <- if(isTRUE(showTiming)) c(2, 3) else c(2, 2)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mfrow = panels, mar = c(4, 4, 2, 1))

  objy <- log1p(-diagdf$objective - min(-diagdf$objective, na.rm = TRUE))
  graphics::plot(x, objy, type = "b", pch = 19, col = cols,
    xlab = "Outer iteration", ylab = "log(1 + objective - min(objective))", main = "Objective")
  if(nrow(diagdf) > 1) graphics::lines(stats::lowess(x, objy, f = 0.6), lwd = 2)

  graphics::plot(x, grady, type = "b", pch = 19, col = cols,
    xlab = "Outer iteration",
    ylab = if(logGrad) "log(1 + gradient norm)" else "Gradient norm",
    main = "Gradient")
  if(nrow(diagdf) > 1) graphics::lines(stats::lowess(x, grady, f = 0.6), lwd = 2)

  ylim_step <- range(c(diagdf$itemStepRms, diagdf$personStepRms), finite = TRUE)
  graphics::plot(x, diagdf$itemStepRms, type = "b", pch = 19, col = "#20639b",
    xlab = "Outer iteration", ylab = "RMS movement", ylim = ylim_step,
    main = "Parameter Movement")
  graphics::lines(x, diagdf$personStepRms, type = "b", pch = 17, col = "#ed553b")
  graphics::legend("topright", legend = c("Item", "Person"),
    col = c("#20639b", "#ed553b"), pch = c(19, 17), bty = "n", cex = 0.85)

  ylim_sd <- range(c(diagdf$meanPosteriorSD, diagdf$maxPosteriorSD), finite = TRUE)
  graphics::plot(x, diagdf$meanPosteriorSD, type = "b", pch = 19, col = "#3caea3",
    xlab = "Outer iteration", ylab = "Posterior SD", ylim = ylim_sd,
    main = "Posterior Spread")
  graphics::lines(x, diagdf$maxPosteriorSD, type = "b", pch = 17, col = "#f6d55c")
  graphics::legend("topright", legend = c("Mean SD", "Max SD"),
    col = c("#3caea3", "#f6d55c"), pch = c(19, 17), bty = "n", cex = 0.85)

  if(isTRUE(showTiming)){
    graphics::plot(x, diagdf$outerIterSec, type = "b", pch = 19, col = "#173f5f",
      xlab = "Outer iteration", ylab = "Seconds",
      ylim = c(0, max(diagdf$outerIterSec, diagdf$itemStepSec, diagdf$personStepSec, diagdf$refreshStepSec, na.rm = TRUE)),
      main = "Iteration Timing")
    graphics::lines(x, diagdf$itemStepSec, type = "b", pch = 17, col = "#20639b")
    graphics::lines(x, diagdf$personStepSec, type = "b", pch = 15, col = "#ed553b")
    graphics::lines(x, diagdf$refreshStepSec, type = "b", pch = 18, col = "#3caea3")
    graphics::legend("topright",
      legend = c("Outer total", "Item", "Person", "Refresh"),
      col = c("#173f5f", "#20639b", "#ed553b", "#3caea3"),
      pch = c(19, 17, 15, 18), bty = "n", cex = 0.75)

    relimp <- ifelse(is.finite(diagdf$relativeImprove), diagdf$relativeImprove, NA_real_)
    ymax <- suppressWarnings(max(relimp, na.rm = TRUE))
    if(!is.finite(ymax) || ymax <= 0) ymax <- 1
    relimp_plot <- relimp
    if(all(!is.finite(relimp_plot))) relimp_plot <- rep(0, length(x))
    graphics::plot(x, relimp_plot, type = "b", pch = 19, col = cols,
      xlab = "Outer iteration", ylab = "Relative improvement",
      ylim = c(0, ymax), main = "Stability")
    graphics::points(x, ifelse(diagdf$strictCriterion, ymax, NA_real_), pch = 15, col = "#ed553b")
    graphics::points(x, ifelse(diagdf$stabilityCriterion, ymax * 0.9, NA_real_), pch = 17, col = "#3caea3")
    graphics::legend("topright", legend = c("strict", "stability"),
      col = c("#ed553b", "#3caea3"), pch = c(15, 17), bty = "n", cex = 0.75)
  }

  invisible(diagdf)
}

#' Plot Laplace diagnostics
#'
#' @param fit A fitted \code{bigIRT} model returned by \code{fitIRT()} with
#'   \code{marginalApprox="laplace_em"} and \code{laplaceDiagnostics=TRUE}.
#' @param logGrad Whether to plot the item-step gradient norm on a log10 scale.
#' @param showTiming Whether to include timing and stability panels.
#'
#' @return Invisibly returns the diagnostic data frame used for plotting.
#' @export
plotLaplaceDiagnostics <- function(fit, logGrad = TRUE, showTiming = TRUE){
  if(is.null(fit$laplaceDiagnostics) || nrow(fit$laplaceDiagnostics) == 0){
    stop("No Laplace diagnostics found on fit object.")
  }
  bigIRT_plot_laplace_diag_df(fit$laplaceDiagnostics, logGrad = logGrad, showTiming = showTiming)
}

# fitIRTstepwise <- function(dat,itemsteps,item='Item',id='id',normalise=FALSE,ebayes=FALSE,...){ #need to rethink...
#   .itemref <- item
#   .idref <- id
#   itemDat <- NA
#   stepseq <- c(1:length(itemsteps))#,length(itemsteps):1)
#   firststep <- TRUE
#
#
#   for(stepi in 1:length(stepseq)){
#     include <- which(dat[[.itemref]] %in% itemsteps[[stepseq[stepi]]]) #which rows to include for current item set
#     stepids <- unique(dat[include,get(.idref)]) #which subjects are relevant
#     if(stepi > 1){ #for subsequent steps,
#       itemDat <- itemDat[!get(.itemref) %in% itemsteps[[stepseq[stepi]]],] #freely estimate current item set
#       include <- unique(c(include, # and use prior step as link
#         which(dat[[.itemref]] %in% itemsteps[[stepseq[stepi-1]]] & dat[[.idref]] %in% stepids)))
#     }
#     smalldat <- dat[include,] #step specific data set
#
#     fit <- fitIRT(dat = smalldat,itemDat=itemDat,normalise=normalise,ebayes=ebayes,item=item,id=id,...)
#     itemDat <- data.table(fit$itemPars)
#
#     if(firststep){
#       itemout <- itemDat
#       personout <- data.table(fit$personPars)
#     }
#     if(!firststep){ #add new items to output
#       itemout <- rbind(itemout,itemDat[!get(.itemref) %in% itemout[[.itemref]]]) #update item output with newest estimates
#       personout <- rbind(personout, data.table(fit$personPars)[!get(.idref) %in% personout[[.idref]],]) #update item output with newest estimates
#     }
#
#     # plot(itemout[order(as.character(get(.itemref))),B],dat[!duplicated(Item) & get(.itemref) %in% itemout[[.itemref]],][order(get(.itemref)),B])
#     # rmse <- sqrt(mean((itemout[order(as.character(get(.itemref))),B]-dat[!duplicated(get(.itemref)) & Item %in% itemout[[.itemref]],][order(get(.itemref)),B])^2))
#     # message(paste('corr =',cor(cbind(itemout[order(as.character(get(.itemref))),B],dat[!duplicated(get(.itemref)) & get(.itemref) %in% itemout[[.itemref]],][order(get(.itemref)),B]))[2,1]))
#     # message(paste('RMSE =',round(rmse,3)))
#
#     firststep <- FALSE
#   }
#
#   if(FALSE){ #for testing
#     fullfit <- fitIRT(dat = dat,normalise=normalise,ebayes=ebayes)
#     #item par comparison to true
#     message(paste('corr =',cor(cbind(
#       data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% fullfit$itemPars[[.itemref]],][order(Item),B])
#     )[2,1]))
#
#     points(data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% itemout[[.itemref]],][order(Item),B],col=2)
#
#     #person par comparison to true
#     message(paste('corr =',cor(cbind( #stepwise fit
#       personout[order(as.character(get(.idref))),s1],
#       dat[!duplicated(get(.idref)) & get(.idref) %in% personout[[.idref]],][order(get(.idref)),Ability])
#     )[2,1]))
#
#     message(paste('corr =',cor(cbind( #full fit
#       data.table(fullfit$personPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% fullfit$itemPars[[.itemref]],][order(Item),B])
#     )[2,1]))
#
#     points(data.table(fullfit$itemPars)[order(as.character(Item)),B],
#       dat[!duplicated(Item) & Item %in% itemout[[.itemref]],][order(Item),B],col=2)
#   }
#
#   personout <- personout[order(get(.idref)),]
#   itemout <- itemout[order(get(.itemref)),]
#
#   return(list(itemPars=itemout,personPars=personout))
# }


#' Fit a binary Item Response Theory (IRT) model
#'
#' This function fits a binary Item Response Theory (IRT) model using various parameters and options.
#'
#' @param dat A data frame containing the data to be analyzed.
#' @param score Character. The name of the column in \code{dat} representing the response. Default is 'score'.
#' @param id Character. The name of the column in \code{dat} representing the individual. Default is 'id'.
#' @param item Character. The name of the column in \code{dat} representing the item. Default is 'Item'.
#' @param scale Character. The name of the column in \code{dat} representing the scale of each item. Default is 'Scale'.
#' @param pl Integer. The number of parameters for the logistic model (1PL, 2PL, 3PL, or 4PL). Default is 1.
#' @param personDat Data frame. Any fixed ability data for persons. Default is NA.
#' @param personPreds Character vector. Names of predictors for person parameters found in data. Default is an empty character vector.
#' @param itemDat Data frame. Any fixed item data. Default is NA.
#' @param loadings Optional numeric matrix (`Nitems x Nscales`) defining
#'   item-by-scale discrimination structure. Each cell can be:
#'   \itemize{
#'   \item a numeric value (fixed loading),
#'   \item `NA` (free loading, estimated),
#'   \item or omitted by leaving the whole argument as `NA`, which uses the
#'   legacy one-hot bridge from `scale`.
#'   }
#'   Row names are matched to item ids and column names are matched to scale ids
#'   when provided.
#' @param loadingsFixed Optional logical matrix (`Nitems x Nscales`) that
#'   forces fixed/free status for each loading entry. `TRUE` means fixed,
#'   `FALSE` means free, `NA` defers to `loadings` values. If omitted, fixed/free
#'   status is inferred from whether `loadings` entries are `NA`.
#' @param AitemPreds Character vector. Names of predictors for item discrimination parameters. Default is an empty character vector.
#' @param BitemPreds Character vector. Names of predictors for item difficulty parameters. Default is an empty character vector.
#' @param CitemPreds Character vector. Names of predictors for item guessing parameters. Default is an empty character vector.
#' @param DitemPreds Character vector. Names of predictors for item upper asymptote (slipping) parameters. Default is an empty character vector.
#' @param itemSpecificBetas Logical. Whether to allow item-specific betas for covariate effects, or simply estimate one effect per covariate. Default is FALSE.
#' @param betaScale Numeric. Scale of the prior for beta parameters. Default is 10.
#' @param invspAMeandat Numeric. Mean for the prior distribution of the raw discrimination parameters,
#' which subsequently have a 'softplus' log(1+exp(x)) applied. Default is 0.542, giving a mean for A pars of ~ 1.
#' @param invspASD Numeric. Standard deviation for the prior distribution of the raw discrimination parameters. Default is 1.
#' @param BMeandat Numeric. Mean for the prior distribution of the item difficulty parameters. Default is 0.
#' @param BSD Numeric. Standard deviation for the prior distribution of the item difficulty parameters. Default is 10.
#' @param logitCMeandat Numeric. Mean for the prior distribution of the item guessing parameters (on logit scale). Default is -4.
#' @param logitCSD Numeric. Standard deviation for the prior distribution of the item guessing parameters (on logit scale). Default is 4.
#' @param logitDMeandat Numeric. Mean for the prior distribution of the item upper asymptote parameters (on logit scale). Default is 4.
#' @param logitDSD Numeric. Standard deviation for the prior distribution of the item upper asymptote parameters (on logit scale). Default is 4.
#' @param AbilityMeandat Numeric array. Mean for the prior distribution of the ability parameters. Default is 0 for each scale.
#' @param AbilitySD Numeric array. Standard deviation for the prior distribution of the ability parameters. Default is 10 for each scale.
#' @param AbilityCorr Matrix. Correlation matrix for the ability parameters. Default is an identity matrix.
#' @param AMeanSD Numeric. Standard deviation for the prior distribution of the discrimination parameters. Default is 1.
#' @param BMeanSD Numeric. Standard deviation for the prior distribution of the difficulty parameters. Default is \code{BSD}.
#' @param logitCMeanSD Numeric. Standard deviation for the prior distribution of the guessing parameters (on logit scale). Default is \code{logitCSD}.
#' @param logitDMeanSD Numeric. Standard deviation for the prior distribution of the upper asymptote parameters (on logit scale). Default is \code{logitDSD}.
#' @param AbilityMeanSD Numeric array. Standard deviation for the prior distribution of the ability parameters. Default is 1 for each scale.
#' @param iter Integer. Maximum number of iterations for the fitting algorithm. Default is 2000.
#' @param cores Integer. Number of cores to use for parallel computation. Default is 6.
#' @param carefulfit Logical. Whether to use a slower, careful fitting procedure. Default is FALSE. Experimental.
#' @param ebayes Logical. Whether to use empirical Bayes estimation. Default is TRUE. With ebayes, the priors are adapted based on a first pass estimate.
#' @param ebayesmultiplier Numeric. Multiplier for the widths of the empirical Bayes priors. Default is 2, as this appears to work better in practice.
#' @param ebayesFromFixed Logical. Whether to initialize empirical Bayes from any specifed fixed values Default is FALSE.
#' @param estMeans Character vector. Which means to estimate from 'ability', 'A', 'B', 'C', 'D'. Default is c('ability', 'B', 'C', 'D'), with
#' discrimination means fixed.
#' @param priors Logical. Whether to use prior distributions. Default is TRUE.
#' @param integrateEachAbility Logical. Whether to integrate across each ability. Default is FALSE.
#' @param integrateEachAbilityFixedSE Logical. Whether to integrate each ability with fixed standard error. Default is FALSE.
#' @param NintegratePoints Integer. Number of integration points for numerical integration. Default is 5.
#' @param sampledAbilityStep Logical. Whether to run the Laplace-EM style outer
#'   updates after the base optimization step. Default is FALSE.
#' @param marginalApprox Character. Marginal approximation backend. Use
#'   \code{"none"} for the legacy Stan/JML path, \code{"sigma_em"} for the
#'   legacy deterministic-support outer loop, \code{"laplace_em"} for the
#'   pure-C++ Laplace marginal-likelihood outer loop, and
#'   \code{"laplace_direct"} for a single-stage direct Laplace optimizer that
#'   recomputes person modes inside each objective evaluation and uses an
#'   approximate gradient that ignores derivatives through those inner solves.
#'   Default is \code{"none"}.
#' @param estimateAbilityCorr Logical. If \code{TRUE}, estimate the latent
#'   ability correlation matrix directly inside \code{marginalApprox =
#'   "laplace_direct"} while keeping \code{AbilitySD} fixed. Ignored for other
#'   backends and for unidimensional fits. Default is FALSE.
#' @param laplaceOuterIter Integer. Maximum number of outer iterations for
#'   \code{marginalApprox="laplace_em"}. This is a fallback limit rather than
#'   the primary convergence criterion. Default is 50.
#' @param laplaceTol Numeric. General outer tolerance for
#'   \code{marginalApprox="laplace_em"}, used for relative objective
#'   improvement and RMS step-size checks. Default is 1e-3.
#' @param laplaceGradTol Numeric. Gradient-norm tolerance for the item-step
#'   Laplace surrogate. Default is 1e-2.
#' @param laplaceStabilityIter Integer. Window size for stability-based
#'   convergence in \code{laplace_em}. If the last
#'   \code{laplaceStabilityIter} outer iterations show negligible objective
#'   change and no meaningful reduction in the item-step gradient norm, the fit
#'   stops even when the gradient norm is still above \code{laplaceGradTol}.
#'   Default is 5.
#' @param laplacePersonTol Numeric. Newton tolerance for person-mode updates in
#'   \code{laplace_em}. Default is 1e-4.
#' @param laplaceKeepCovariance Logical. Whether to keep full person covariance
#'   matrices on the Laplace path. Default is FALSE.
#' @param laplaceDiagnostics Logical. Whether to store outer-loop diagnostics for
#'   \code{laplace_em}. Default is FALSE.
#' @param laplacePlot Logical. Whether to draw the Laplace diagnostic plot during
#'   fitting when \code{marginalApprox="laplace_em"}. Default is FALSE.
#' @param laplacePlotEvery Integer. Plot every N outer iterations when
#'   \code{laplacePlot=TRUE}. Default is 1.
#' @param sampledAbilityOuterIter Integer. Number of Laplace-EM outer iterations. Default is 2.
#' @param sampledAbilityJitter Numeric. Small jitter added to stabilize Laplace
#'   covariance calculations. Default is 1e-6.
#' @param sampledAbilitySigmaScale Numeric. Multiplier applied to the deterministic
#'   Laplace support used in the item-update step. Default is 0.25.
#' @param sampledAbilityDiagnostics Logical. Whether to store Laplace-EM diagnostics in
#'   \code{fit$sampledAbilityDiagnostics}. Default is FALSE.
#' @param sampledAbilityPlot Logical. Whether to plot Laplace-EM diagnostics during outer
#'   iterations. Default is FALSE.
#' @param sampledAbilityPlotEvery Integer. Plot Laplace-EM diagnostics every N outer iterations
#'   when \code{sampledAbilityPlot=TRUE}. Default is 1.
#' @param sampledAbilityStepTol Numeric. RMS movement tolerance used by the
#'   sampled-ability multi-criterion stopping rule. Default is 1e-3.
#' @param sampledAbilitySpreadTol Numeric. Relative posterior-spread tolerance
#'   used by the sampled-ability stopping rule. Default is 0.02.
#' @param sampledAbilityPatience Integer. Number of consecutive accepted outer
#'   iterations required for sampled-ability convergence. Default is 3.
#' @param sampledAbilityControl Optional named list of advanced Laplace-EM
#'   control settings. Flat arguments supply the defaults and entries in
#'   this list override them.
#' @param noptimsteps Integer. Number of optimizer iterations used inside each sampled-ability
#'   item/person sub-step. Default is 10.
#' @param noptimgradtol Numeric. Convergence tolerance for combined sampled-ability gradient norm.
#'   Default is 1e-2.
#' @param normalise Logical. Whether to normalize the output estimates. Default is FALSE.
#' @param normaliseScale Numeric. Scale for normalization. Default is 1.
#' @param normaliseMean Numeric. Mean for normalization. Default is 0.
#' @param dropPerfectScores Logical. Whether to drop perfect scores from each subject and item before estimation. Default is TRUE.
#' @param trainingRows Integer vector. Rows of data to use for estimation of parameters. Default is all rows in \code{dat}.
#' @param init Initial values for the fitting algorithm. Default is NA.
#' @param tol Numeric. Tolerance for convergence. Default attempts to sensibly adjust for amount of data.
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return A list containing the fitted IRT model parameters and additional information about the fit.
#' @export
#'
#' @examples
#' #Generate some data (here 2pl model
#' require(data.table)
#' dat <- simIRT(Nsubs = 50,Nitems = 100,Nscales = 1,
#'   logitCMean = -10,logitCSD = 0,AMean = 1,ASD = .3,
#'   BMean=0,BSD = .5,
#'   AbilityMean = 0,AbilitySD = 1)
#'
#' #fit using bigIRT
#' fit <- fitIRT(dat$dat,cores=2,score = 'score',id = 'id',
#'   scale = 'Scale',item = 'Item', pl=2)
#'
#'   print(fit$personPars)
#'   print(fit$itemPars)
#'
#' #Explicit multidimensional loading specification (2D example)
#' #Rows/cols are aligned by names to item ids and scale ids.
#' item_ids <- as.character(unique(dat$dat$Item))
#' scale_ids <- as.character(unique(dat$dat$Scale))
#' L <- matrix(NA_real_, nrow = length(item_ids), ncol = length(scale_ids),
#'   dimnames = list(item_ids, scale_ids))
#' L[1, ] <- c(0.8, 0.2) #fixed cross-loading
#' L[2, 2] <- 0          #fixed zero loading
#' fit_mirt <- fitIRT(dat$dat, pl = 2, cores = 1, loadings = L)
#' head(fit_mirt$itemPars[, grep("^A(_|$)", names(fit_mirt$itemPars)), drop = FALSE])
fitIRT <- function(dat,score='score', id='id', item='Item', scale='Scale',pl=1,
  personDat=NA, personPreds=character(),
  itemDat=NA,
  loadings=NA, loadingsFixed=NA,
  AitemPreds=character(),
  BitemPreds=character(),
  CitemPreds=character(),
  DitemPreds=character(),
  itemSpecificBetas=FALSE,
  betaScale=10,
  invspAMeandat=.542,invspASD=1,BMeandat=0,BSD=10, logitCMeandat=-4,logitCSD=4,
  logitDMeandat=4,logitDSD=4,
  AbilityMeandat=array(0,dim=c(length(unique(dat[[scale]])))),
  AbilitySD=array(10,dim=c(length(unique(dat[[scale]])))),
  AbilityCorr=diag(1,c(length(unique(dat[[scale]])))),
  AMeanSD=1,BMeanSD=BSD,logitCMeanSD=logitCSD,logitDMeanSD=logitDSD,
  AbilityMeanSD=array(1,dim=c(length(unique(dat[[scale]])))),
  iter=2000,cores=6,carefulfit=FALSE,
  ebayes=TRUE,ebayesmultiplier=2,ebayesFromFixed=FALSE,
  estMeans=c('ability','B','C','D'),priors=TRUE,
  integrateEachAbility=FALSE, integrateEachAbilityFixedSE=FALSE,
  NintegratePoints=5,
  marginalApprox=c("none","sigma_em","laplace_em","laplace_direct"),
  estimateAbilityCorr=FALSE,
  laplaceCorrParam=c("normalized_chol","stan_corsqrt"),
  laplaceOuterIter=50,laplaceTol=1e-3,laplaceGradTol=1e-2,laplaceStabilityIter=5L,laplacePersonTol=1e-4,
  laplaceKeepCovariance=FALSE,laplaceDiagnostics=FALSE,laplacePlot=FALSE,laplacePlotEvery=1L,
  sampledAbilityStep=FALSE,sampledAbilityOuterIter=50,sampledAbilityJitter=1e-6,
  sampledAbilitySigmaScale=0.25,sampledAbilityDiagnostics=FALSE,
  sampledAbilityPlot=FALSE,sampledAbilityPlotEvery=1L,
  sampledAbilityStepTol=1e-3,sampledAbilitySpreadTol=0.02,
  sampledAbilityPatience=3L,sampledAbilityControl=NULL,noptimsteps=10,
  noptimgradtol=1e-2,
  normalise=FALSE,normaliseScale=1,normaliseMean=0,
  dropPerfectScores=TRUE,trainingRows=1:nrow(dat),
  init=NA,tol=1e-8 * 10^(log(nrow(dat), 10)),...){

  sdat <-list() #initialize standata object
  basetol=tol
  marginalApprox <- match.arg(marginalApprox)
  laplaceCorrParam <- match.arg(laplaceCorrParam)
  if(isTRUE(sampledAbilityStep) && identical(marginalApprox, "none")){
    marginalApprox <- "sigma_em"
  }
  sampledAbilityStep <- identical(marginalApprox, "sigma_em")
  if(isTRUE(estimateAbilityCorr) && !identical(marginalApprox, "laplace_direct")){
    warning("estimateAbilityCorr is currently only active for marginalApprox = 'laplace_direct'.")
  }

  itemPreds <- unique(c(AitemPreds,BitemPreds,CitemPreds,DitemPreds))

  #setup unlikely names to use in data.table calls to avoid overlap from user defined names
  idref. <- id; scaleref. <- scale; itemref. <- item;
  scoreref. <- score;
  personPredsref. <- personPreds;
  itemPredsref. <- itemPreds

  if(!'data.table' %in% class(dat)){  #drop unused columns from dat and set to data.table (copy if already data table)
    dat <- as.data.table(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
      itemPredsref.,personPredsref.),with=FALSE])
  } else {
    dat <- data.table::copy(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
      itemPredsref.,personPredsref.),with=FALSE])
  }


  #drop problem people and items
  if(dropPerfectScores)    dat <- dropPerfectScores(dat,scoreref. = scoreref.,itemref. = itemref.,idref. = idref.)

  #sort data by subject
  dat <- dat[order(get(idref.)),]

  #setup indices to map user specified categories to sequential integers for stan
  itemIndex <- data.table(original=as.character(dat[[itemref.]][!duplicated(dat[[itemref.]])]))
  scaleIndex <- data.table(original=as.character(dat[[scaleref.]][!duplicated(dat[[scaleref.]])]))
  idIndex <- data.table(original=as.character(dat[[idref.]][!duplicated(dat[[idref.]])]))


  #convert categories to sequential integers
  indx <- c(idref.,itemref.,scaleref.)
  for( ci in indx) set(dat,j = ci,value = as.integer(factor(dat[[ci]])))
  for( ci in indx) set(dat,j = ci,value = as.integer((dat[[ci]])))

  #include new sequential integers in index lists
  itemIndex$new <- dat[[itemref.]][!duplicated(dat[[itemref.]])]
  itemIndex$scale <- dat[[scaleref.]][!duplicated(dat[[itemref.]])]
  scaleIndex$new <-dat[[scaleref.]][!duplicated(dat[[scaleref.]])]
  idIndex$new <- dat[[idref.]][!duplicated(dat[[idref.]])]

  #order indices by new integer

  itemIndex=itemIndex[order(new),]
  scaleIndex=scaleIndex[order(new),]
  idIndex=idIndex[order(new),]


  #checks...
  if(any(is.na(dat))) stop('Missings found in data! Probably just remove the row/s...')
  if(!is.numeric(dat[[scoreref.]])) stop('Found a non-numeric score column!')
  if(normalise && any(!is.na(c(itemDat,personDat)))) warning(
    'With fixed values provided you might want to set normalise= FALSE',immediate. = TRUE)



  Nitems <- length(unique(dat[[itemref.]]))
  Nsubs=length(unique(dat[[idref.]]))
  Nscales=length(unique(dat[[scaleref.]]))

  #if getting priors from fixed pars, do this before dropping unnecessary items from itemSetup / AbilitySetup
  if(ebayesFromFixed){
    if(length(personDat)==1 && !is.na(personDat)){
      personDat <- as.data.table(personDat)
      sdat$dopriors <- 1L

      sdat$AbilityMeandat <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,mean,na.rm=TRUE))
      sdat$AbilitySD <- array(apply(personDat[,c(scaleIndex$original),with=FALSE],2,sd,na.rm=TRUE))*ebayesmultiplier+1e-5 #maybe need to better account for multiple scales here, but not that important...
      sdat$AbilityCorr <- cor(personDat[,c(scaleIndex$original),with=FALSE],use='pairwise.complete.obs')
    }
    if(length(itemDat)==1 && !is.na(itemDat)){
      itemDat <- as.data.table(itemDat)
      sdat$dopriors <- 1L

      sdat$invspAMeandat <- mean(afunci(itemDat$A),na.rm=TRUE)
      sdat$invspASD <- sd(afunci(itemDat$A),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$BMeandat <- mean(itemDat$B,na.rm=TRUE)
      sdat$BSDx <- sd(itemDat$B,na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$logitCMeandat <- mean(logit(itemDat$C+1e-8),na.rm=TRUE)
      sdat$logitCSD <- sd(logit(itemDat$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5

      sdat$logitDMeandat <- mean(logit(itemDat$D+1e-8),na.rm=TRUE)
      sdat$logitDSD <- sd(logit(itemDat$D+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
    }
  }

  # setup item structure to define fixed / free pars
  itemSetup <- data.table(itemIndex, A=as.numeric(NA), B=as.numeric(NA),
    C=ifelse(pl>2,as.numeric(NA),0),D=ifelse(pl>3,as.numeric(NA),1))
  legacyLoadingMat <- matrix(0, nrow = Nitems, ncol = Nscales)
  legacyLoadingMat[cbind(seq_len(Nitems), itemSetup$scale)] <- if(pl > 1) NA_real_ else 1

  if(!all(is.na(itemDat))){ # if fixed item pars
    if(!'data.table' %in% class(itemDat)) itemDat <- as.data.table(itemDat)
    itemDat <- itemDat[get(itemref.) %in% itemSetup$original,]
    setupRows <- match(itemDat[[itemref.]],itemSetup$original)
    itemSetup[setupRows,c('B','C'):=itemDat[,c('B','C')]]
    if('D' %in% colnames(itemDat)) itemSetup[setupRows,'D':=itemDat[,D]]
    if('A' %in% colnames(itemDat)){
      legacyLoadingMat[cbind(setupRows, itemSetup$scale[setupRows])] <- itemDat$A
    }
    if(pl<3) itemSetup[,'C':=0]
    if(pl<4) itemSetup[,'D':=1]
  }

  loadingIsDefault <- length(loadings) == 1 && is.na(loadings[1])
  if(loadingIsDefault){
    loadingValues <- legacyLoadingMat
  } else {
    if(pl < 2) stop("Custom `loadings` require pl >= 2.")
    loadingsInput <- as.matrix(loadings)
    storage.mode(loadingsInput) <- "double"
    if(length(dim(loadingsInput)) != 2) stop("`loadings` must be a matrix-like object.")
    if(nrow(loadingsInput) != Nitems || ncol(loadingsInput) != Nscales){
      stop("`loadings` must have dimensions Nitems x Nscales after indexing.")
    }
    if(!is.null(rownames(loadingsInput))){
      rowMap <- match(itemSetup$original, rownames(loadingsInput))
      if(any(is.na(rowMap))) stop("Could not align `loadings` row names to items.")
      loadingsInput <- loadingsInput[rowMap,,drop=FALSE]
    }
    if(!is.null(colnames(loadingsInput))){
      colMap <- match(scaleIndex$original, colnames(loadingsInput))
      if(any(is.na(colMap))) stop("Could not align `loadings` column names to scales.")
      loadingsInput <- loadingsInput[,colMap,drop=FALSE]
    }
    loadingValues <- loadingsInput
  }

  loadingFixedFromValues <- !is.na(loadingValues)
  loadingMaskDefault <- length(loadingsFixed) == 1 && is.na(loadingsFixed[1])
  if(!loadingMaskDefault){
    loadingMask <- as.matrix(loadingsFixed)
    if(length(dim(loadingMask)) != 2 || nrow(loadingMask) != Nitems || ncol(loadingMask) != Nscales){
      stop("`loadingsFixed` must have dimensions Nitems x Nscales after indexing.")
    }
    if(!is.null(rownames(loadingMask))){
      rowMap <- match(itemSetup$original, rownames(loadingMask))
      if(any(is.na(rowMap))) stop("Could not align `loadingsFixed` row names to items.")
      loadingMask <- loadingMask[rowMap,,drop=FALSE]
    }
    if(!is.null(colnames(loadingMask))){
      colMap <- match(scaleIndex$original, colnames(loadingMask))
      if(any(is.na(colMap))) stop("Could not align `loadingsFixed` column names to scales.")
      loadingMask <- loadingMask[,colMap,drop=FALSE]
    }
    loadingMask <- as.logical(loadingMask)
    loadingMaskSelector <- !is.na(loadingMask)
    loadingFixedFromValues[loadingMaskSelector] <- loadingMask[loadingMaskSelector]
  }
  if(any(loadingFixedFromValues & is.na(loadingValues))){
    stop("All fixed loading entries must have numeric values.")
  }
  loadingValues[!loadingFixedFromValues] <- NA_real_
  if(pl < 2){
    loadingValues[,] <- 0
    loadingValues[cbind(seq_len(Nitems), itemSetup$scale)] <- 1
    loadingFixedFromValues[,] <- TRUE
  }

  itemSetup$A <- loadingValues[cbind(seq_len(Nitems), itemSetup$scale)]
  itemSetup[,paste0(c('B','C','D'),'data'):= .SD, .SDcols=c('B','C','D')] #create data columns
  setnafill(itemSetup,fill = -99,cols = paste0(c('B','C','D'),'data')) #and fill with arbitrary value to avoid NA in stan
  Adata <- a_matrix_to_vector(loadingValues)
  Adata[is.na(Adata)] <- -99
  fixedAlog <- as.integer(a_matrix_to_vector(loadingFixedFromValues))

  #setup person structure to define fixed / free pars
  AbilitySetup <- data.table(idIndex)
  AbilitySetup[,c(scaleIndex$original):=as.numeric(NA)]

  if(!all(is.na(personDat))){ #if fixed person pars
    if(!'data.table' %in% class(personDat)) personDat <- as.data.table(personDat)
    personDat <- personDat[get(idref.) %in% AbilitySetup$original,]
    setupRows <- match(personDat[[idref.]],AbilitySetup$original)
    AbilitySetup[setupRows,c(scaleIndex$original):=personDat[,c(scaleIndex$original),with=FALSE]]
  }
  AbilitySetup[,paste0(c(scaleIndex$original),'data'):= .SD, .SDcols=c(scaleIndex$original)] #create data columns
  setnafill(AbilitySetup,fill = -99,cols = paste0(c(scaleIndex$original),'data')) #and fill with arbitrary value to avoid NA in stan

  #which abilities are fixed
  fixedAbilityLogical <- AbilitySetup[order(new),c(scaleIndex$original),with=FALSE]
  fixedAbilityLogical<-fixedAbilityLogical[,lapply(.SD,function(x) as.integer(!is.na(x)))]

  #which parameters do the unfixed Ability matrix slots need to refer to
  Abilityparsindex <- matrix(cumsum(1-unlist(fixedAbilityLogical)),Nsubs,Nscales)
  Abilityparsindex[fixedAbilityLogical==1] <- 0

  #which scale is each Ability par for
  Abilityparsscaleindex <- c(col(Abilityparsindex)[Abilityparsindex>0])

  # #include short predictors:
  #
  # if(length(itemPredsref.)==0){
  #   itemPreds <- array(0,dim = c(Nitems,0))
  # } else{
  #   itemPreds <- dat[!duplicated(get(itemref.)),itemPredsref.,with=FALSE]
  #   itemPreds <- itemPreds[order(unique(dat[[itemref.]])),]
  # }
  #
  # if(length(personPredsref.)==0){
  #   personPreds <- array(0,dim = c(Nsubs,0))
  # } else{
  #   personPreds <- dat[!duplicated(get(idref.)),personPredsref.,with=FALSE]
  #   personPreds <- personPreds[order(unique(dat[[idref.]])),]
  # }

  #include long predictors:

  if(length(itemPredsref.)==0){
    itemPreds <- array(0,dim = c(nrow(dat),0))
  } else{
    itemPreds <- dat[,itemPredsref.,with=FALSE]
  }

  if(length(personPredsref.)==0){
    personPreds <- array(0,dim = c(nrow(dat),0))
  } else{
    personPreds <- dat[,personPredsref.,with=FALSE]
  }

  # sdat$NstatePreds <- length(statePreds)
  # sdat$statePreds <- matrix(0, nrow(dat), sdat$NstatePreds)
  # if(sdat$NstatePreds > 0) sdat$statePreds <- as.matrix(dat[,statePredsref.,with=FALSE])

  trainingLogical=array(rep(0L,nrow(dat)))
  trainingLogical[trainingRows] <- 1L

  sdat <- c(sdat,list(
    Nobs=nrow(dat),
    Nsubs=Nsubs,
    Nitems=Nitems,
    Nscales=Nscales,
    NitemScales=as.integer(Nitems * Nscales),
    id=array(dat[[idref.]]),
    dopriors=as.integer(priors),
    outlierfix=0L,
    outlierscale=2,
    NfixedA=as.integer(sum(fixedAlog)),
    NfixedB=as.integer(sum(!is.na(itemSetup$B))),
    NfixedC=as.integer(sum(!is.na(itemSetup$C))),
    NfixedD=as.integer(sum(!is.na(itemSetup$D))),
    NfixedAbility=as.integer(sum(!is.na(unlist(AbilitySetup[,scaleIndex$original,with=FALSE])))),
    whichfixedA=array(as.integer(which(fixedAlog == 1L))),
    whichfixedB=array(as.integer(which(!is.na(itemSetup$B)))),
    whichfixedC=array(as.integer(which(!is.na(itemSetup$C)))),
    whichfixedD=array(as.integer(which(!is.na(itemSetup$D)))),
    fixedAlog=array(as.integer(fixedAlog)),
    fixedB=array(as.integer((!is.na(itemSetup$B)))),
    fixedClogit=array(as.integer((!is.na(itemSetup$C)))),
    fixedDlogit=array(as.integer((!is.na(itemSetup$D)))),
    whichnotfixedA=array(as.integer(which(fixedAlog == 0L))),
    whichnotfixedB=array(as.integer(which(is.na(itemSetup$B)))),
    whichnotfixedC=array(as.integer(which(is.na(itemSetup$C)))),
    whichnotfixedD=array(as.integer(which(is.na(itemSetup$D)))),
    Abilityparsindex=array(as.integer(unlist(Abilityparsindex)),c(Nsubs,Nscales)),
    fixedAbilityLogical=array(unlist(fixedAbilityLogical),c(Nsubs,Nscales)),
    Abilityparsscaleindex=array(as.integer(Abilityparsscaleindex)),
    start=1L,
    end=as.integer(nrow(dat)),
    trainingLogical=trainingLogical,
    score=array(as.integer(dat[[scoreref.]])),
    incorrect=array(as.integer(which(dat[[scoreref.]]==0))),
    item = array(dat[[itemref.]]),
    itemMean = dat$itemMean[!duplicated(dat[[itemref.]])],
    personMean = dat$personMean[!duplicated(dat[[idref.]])],
    scale=array(dat[[scaleref.]]),
    Adata=Adata,
    Bdata=array(itemSetup$Bdata),
    Cdata=array(itemSetup$Cdata),
    Ddata=array(itemSetup$Ddata),
    Abilitydata=matrix(unlist(AbilitySetup[,paste0(c(scaleIndex$original),'data'),with=FALSE]),Nsubs,Nscales),
    NitemPreds=ncol(itemPreds),
    NAitemPreds=length(AitemPreds),
    NBitemPreds=length(BitemPreds),
    NCitemPreds=length(CitemPreds),
    NDitemPreds=length(DitemPreds),
    AitemPreds=array(as.integer(which(colnames(itemPreds) %in% AitemPreds))),
    BitemPreds=array(as.integer(which(colnames(itemPreds) %in% BitemPreds))),
    CitemPreds=array(as.integer(which(colnames(itemPreds) %in% CitemPreds))),
    DitemPreds=array(as.integer(which(colnames(itemPreds) %in% DitemPreds))),
    itemPreds=array(unlist(itemPreds),dim(itemPreds)),
    NpersonPreds=ncol(personPreds),
    personPreds=(array(unlist(personPreds),dim(personPreds))),
    itemSpecificBetas=as.integer(itemSpecificBetas),
    betaScale=betaScale,
    invspAMeandat=invspAMeandat,
    invspASD=invspASD,
    BMeandat=BMeandat,
    BSDx=BSD,
    logitCMeandat=logitCMeandat,
    logitCSD=logitCSD,
    logitDMeandat=logitDMeandat,
    logitDSD=logitDSD,
    AbilityMeandat=AbilityMeandat,
    AbilitySD=array(AbilitySD),
    AbilityCorr=AbilityCorr,
    BMeanSD=BMeanSD,
    logitCMeanSD=logitCMeanSD,
    AbilityMeanSD=array(AbilityMeanSD),
    fixedAMean=as.integer(!'A' %in% estMeans || pl < 2),
    fixedBMean=as.integer(!'B' %in% estMeans),
    fixedCMean=as.integer(!'C' %in% estMeans || pl < 3),
    fixedDMean=as.integer(!'D' %in% estMeans || pl < 4),
    fixedAbilityMean=as.integer(!'Ability' %in% estMeans & !'ability' %in% estMeans),
    rowIndexPar=0L,
    originalRow=dat$`.originalRow`,
    doGenQuant=0L,
    doRowEff=as.integer(isTRUE(sampledAbilityStep) || identical(marginalApprox, "laplace_em")),
    integrateAbility=as.integer(integrateEachAbility),
    integrateAbilityFixedSE=as.integer(integrateEachAbilityFixedSE),
    NintegratePoints=as.integer(NintegratePoints),
    integrateWeights=array(statmod::gauss.quad.prob(n=NintegratePoints,dist='normal')$weights),
    integratePoints=array(statmod::gauss.quad.prob(n=NintegratePoints,dist='normal')$nodes)
  ))

  sdat$freeAref=array(as.integer(cumsum(1-as.numeric(sdat$fixedAlog))))
  sdat$freeBref=array(as.integer(cumsum(1-as.numeric(sdat$fixedB))))
  sdat$freeCref=array(as.integer(cumsum(1-as.numeric(sdat$fixedClogit))))
  sdat$freeDref=array(as.integer(cumsum(1-as.numeric(sdat$fixedDlogit))))

  apply_fit_dimnames <- function(fit){
    rownames(fit$pars$Ability)[idIndex$new] <- idIndex$original
    colnames(fit$pars$Ability)[scaleIndex$new] <- scaleIndex$original
    if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2){
      rownames(fit$pars$A)[itemIndex$new] <- itemIndex$original
      colnames(fit$pars$A)[scaleIndex$new] <- scaleIndex$original
    } else {
      rownames(fit$pars$A)[itemIndex$new] <- itemIndex$original
    }
    rownames(fit$pars$B)[itemIndex$new] <- itemIndex$original
    rownames(fit$pars$C)[itemIndex$new] <- itemIndex$original
    rownames(fit$pars$D)[itemIndex$new] <- itemIndex$original
    fit
  }


  JMLfit <- function(est, sdat, ebayes=FALSE, fit=NA,narrowPriors=FALSE,...){
    skipebayes <- FALSE

    message(paste0(ifelse(narrowPriors,'Narrow priors ', ifelse(ebayes,'Empirical Bayes ','Free estimation ')),'step...'))

    if(!all(is.na(fit))){
      sdat$Adata = if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2){
        a_matrix_to_vector(fit$pars$A)
      } else {
        fit$pars$A
      }
      sdat$Bdata = fit$pars$B
      sdat$Cdata = fit$pars$C
      sdat$Ddata = fit$pars$D
      sdat$Abilitydata = fit$pars$Ability
      if(!ebayes) init = fit$optim$par
    }

    if(ebayes){
      sdat$dopriors <- 1L

      if(pl > 1 &&  length(fit$pars$invspApars) > 2){
        if(sdat$fixedAMean ==0) sdat$invspAMeandat <- mean(fit$pars$invspApars[!fit$pars$invspApars %in% boxplot.stats(fit$pars$invspApars)$out] ) #mean(afunci(fit$pars$A))
        sdat$invspASD <- sd(fit$pars$invspApars[!fit$pars$invspApars %in% boxplot.stats(fit$pars$invspApars)$out])*ebayesmultiplier+1e-5 #afunci(fit$pars$A)
      }

      if(length(fit$pars$Bpars) > 2){
        if(sdat$fixedBMean ==0) sdat$BMeandat <- mean(fit$pars$Bpars[!fit$pars$Bpars %in% boxplot.stats(fit$pars$Bpars)$out])
        sdat$BSDx <- sd(fit$pars$Bpars[!fit$pars$Bpars %in% boxplot.stats(fit$pars$Bpars)$out])*ebayesmultiplier+1e-5
      }

      if(pl > 2 && length(fit$pars$logitCpars) > 2){
        if(sdat$fixedCMean ==0) sdat$logitCMeandat <- mean(fit$pars$logitCpars[!fit$pars$logitCpars %in% boxplot.stats(fit$pars$logitCpars)$out]) #mean(cfunci(fit$pars$C+1e-8))
        sdat$logitCSD <- sd(fit$pars$logitCpars[!fit$pars$logitCpars %in% boxplot.stats(fit$pars$logitCpars)$out],na.rm=TRUE) * ebayesmultiplier+1e-5 #sd(cfunci(fit$pars$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
      }

      if(pl > 3 && length(fit$pars$logitDpars) > 2){
        if(sdat$fixedDMean ==0)  sdat$logitDMeandat <- mean(fit$pars$logitDpars[!fit$pars$logitDpars %in% boxplot.stats(fit$pars$logitDpars)$out]) #mean(cfunci(fit$pars$C+1e-8))
        sdat$logitDSD <- sd(fit$pars$logitDpars[!fit$pars$logitDpars %in% boxplot.stats(fit$pars$logitDpars)$out],na.rm=TRUE) * ebayesmultiplier+1e-5 #sd(cfunci(fit$pars$C+1e-8),na.rm=TRUE)*ebayesmultiplier+1e-5
      }

      # sdat$fixedAMean <- 1L
      # init=NA


      if(length(fit$pars$Abilitypars) > 2){

        sdat$AbilityMeandat <- array(sapply(1:Nscales,function(x){
          mean(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x][
            !fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x] %in%
              boxplot.stats(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x])$out])
        }))

        sdat$AbilitySD <- array(sapply(1:Nscales,function(x){
          sd(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x][
            !fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x] %in%
              boxplot.stats(fit$pars$Abilitypars[sdat$Abilityparsscaleindex %in% x])$out],na.rm=TRUE)
        })) * ebayesmultiplier + 1e-5

        sdat$AbilityCorr= cor(fit$pars$Ability) #inconsistency here -- based on overall ability, rather than conditional ability as for sd / mean.
      }
      if(any(is.na(c(sdat$BSDx,sdat$invspASD,sdat$logitCSD,sdat$AbilitySD)))){
        skipebayes <- TRUE
        warning('NA when computing item sd parameters, ebayes set to FALSE')
      }
    }

    if(narrowPriors){
      sdat$dopriors <- 1L
      sdat$ASD <- .1
      # sdat$BSDx <- 1
      sdat$logitCSD <- .01
      sdat$logitDSD <- .01
      # sdat$AbilitySD <- array(1,sdat$Nscales)
    }
    if(!skipebayes) fit <- optimIRT(standata=sdat,Niter=iter,cores=cores,init = init,...)
    fit <- apply_fit_dimnames(fit)

    return(list(fit=fit,sdat=sdat))
  }

  rm(dat)

  JMLseq <- list()
  if(carefulfit) JMLseq[[1]] <- list(est=c('A','B','C','D','Ability'),ebayes=FALSE,narrowPriors=TRUE)
  JMLseq[[length(JMLseq)+1]] <- list(est=c('A','B','C','D','Ability'),ebayes=FALSE,narrowPriors=FALSE)
  if(ebayes) JMLseq[[length(JMLseq)+1]] <- list(est=c('A','B','C',',D','Ability'),ebayes=TRUE,narrowPriors=FALSE)

  fit <- NA
  if(!sampledAbilityStep && !identical(marginalApprox, "laplace_em") && !identical(marginalApprox, "laplace_direct")){
    for(i in 1:length(JMLseq)){
      if(i < length(JMLseq)) tol= basetol*ifelse(JMLseq[[i]]$narrowPriors,100,10) else tol = basetol
      fit <- JMLfit(est = JMLseq[[i]]$est,sdat = sdat, ebayes=JMLseq[[i]]$ebayes,
        fit = fit,
        narrowPriors = JMLseq[[i]]$narrowPriors,
        tol=tol,...)
      sdat <- fit$sdat
      fit <- fit$fit
    }
  }

  if(identical(marginalApprox, "laplace_direct") && length(which(sdat$Abilityparsindex > 0)) > 0){
    optimdots <- list(...)
    laplaceVerbose <- if("verbose" %in% names(optimdots)) as.integer(optimdots$verbose) else 0L
    if(requireNamespace("RcppParallel", quietly = TRUE)) RcppParallel::setThreadOptions(numThreads = max(1L, as.integer(cores)))
    laplace_trace <- function(level, ...){
      if(laplaceVerbose >= level) message(...)
    }
    wall_time_sec <- function() as.numeric(proc.time()[["elapsed"]])
    estimateAbilityCorrRequested <- isTRUE(estimateAbilityCorr)
    estimateAbilityCorr <- estimateAbilityCorrRequested && sdat$Nscales > 1L
    if(isTRUE(estimateAbilityCorr) && !isTRUE(laplaceKeepCovariance)){
      laplaceKeepCovariance <- TRUE
    }
    if(isTRUE(estimateAbilityCorrRequested) && sdat$Nscales <= 1L){
      warning("estimateAbilityCorr ignored for unidimensional fits.")
      estimateAbilityCorr <- FALSE
    }

    if(sdat$NpersonPreds > 0){
      warning("laplace_direct currently keeps Abilitybeta fixed during direct Laplace optimization when person predictors are present.")
    }

    state <- bigIRT_laplace_initial_state(sdat, eps = sampledAbilityJitter, corr_paramization = laplaceCorrParam)
    priorInfo <- bigIRT_laplace_prior_mats(sdat, jitter = sampledAbilityJitter)
    priorPrecision <- priorInfo$precision_array
    laplace_trace(1, sprintf(
      "Direct Laplace: starting prior-anchored fit with %d persons, %d items, %d dimensions, max_iter=%d.",
      sdat$Nsubs, sdat$Nitems, sdat$Nscales, laplaceOuterIter
    ))
    t_direct <- wall_time_sec()
    directFit <- bigIRT_laplace_optimize_direct(
      state = state,
      sdat = sdat,
      prior_precision = priorPrecision,
      niter = max(2L, as.integer(laplaceOuterIter)),
      tol = laplaceTol,
      jitter = sampledAbilityJitter,
      person_tol = laplacePersonTol,
      keep_covariance = TRUE,
      cores = cores,
      estimateAbilityCorr = estimateAbilityCorr,
      corr_paramization = laplaceCorrParam
    )
    directSec <- wall_time_sec() - t_direct
    state <- directFit$state
    sdat$AbilityCorr <- if(!is.null(state$AbilityCorr)) state$AbilityCorr else sdat$AbilityCorr
    finalPosterior <- directFit$eval$posterior
    state$AbilityBase <- finalPosterior$theta_mode

    fit <- list(pars = list(), optim = list(), dat = sdat)
    fit$pars <- bigIRT_laplace_constrained_pars(state, sdat, posterior = finalPosterior)
    fit$optim <- list(
      method = "laplace_direct",
      logLik = directFit$eval$value,
      par = directFit$optim$par,
      target_evals = directFit$optim$target_evals,
      masked_grad_norm = directFit$optim$masked_grad_norm,
      iter = if(!is.null(directFit$optim$iter)) directFit$optim$iter else NA_integer_,
      terminate = directFit$optim$terminate
    )
    fit$dat <- sdat
    fit <- apply_fit_dimnames(fit)
    fit$personPosterior <- list(
      mode = finalPosterior$theta_mode,
      precision = finalPosterior$precision,
      precision_chol = finalPosterior$precision_chol,
      logdet_precision = finalPosterior$logdet_precision,
      covariance = if("covariance" %in% names(finalPosterior)) finalPosterior$covariance else NULL,
      niter = finalPosterior$niter,
      converged = finalPosterior$converged
    )
    direct_terminate <- if(!is.null(directFit$optim$terminate$what)) as.character(directFit$optim$terminate$what) else "unknown"
    direct_converged <- isTRUE(directFit$optim$masked_grad_norm < laplaceGradTol)
    fit$laplaceStatus <- list(
      converged = isTRUE(direct_converged),
      reason = if(isTRUE(direct_converged)) "approx_gradient" else "max_iter",
      outer_iters = if(!is.null(directFit$optim$iter)) as.integer(directFit$optim$iter) else max(1L, as.integer(laplaceOuterIter)),
      beta_frozen = sdat$NpersonPreds > 0,
      initialized_from = "prior_anchored",
      direct_objective = TRUE,
      approximate_gradient = TRUE,
      gradient_type = "frozen_mode_laplace_item_gradient",
      estimated_corr = estimateAbilityCorr,
      last_item_grad_norm = directFit$optim$masked_grad_norm,
      last_outer_seconds = directSec,
      optimizer_terminate = direct_terminate,
      optimizer_terminate_value = if(!is.null(directFit$optim$terminate$val)) directFit$optim$terminate$val else NA_real_
    )
    fit$abilityPrior <- list(
      sd = as.numeric(sdat$AbilitySD),
      corr = as.matrix(if(!is.null(state$AbilityCorr)) state$AbilityCorr else sdat$AbilityCorr),
      precision = as.matrix(directFit$eval$prior_mats$precision),
      estimated_corr = estimateAbilityCorr
    )
    if(isTRUE(laplaceDiagnostics)){
      fit$laplaceDiagnostics <- data.table::data.table(
        outerIter = 1L,
        objective = directFit$eval$value,
        relativeImprove = NA_real_,
        itemStepRms = NA_real_,
        personStepRms = NA_real_,
        itemGradNorm = directFit$optim$masked_grad_norm,
        meanPosteriorSD = if("covariance" %in% names(finalPosterior)) mean(unlist(lapply(seq_len(dim(finalPosterior$covariance)[3]), function(ii) sqrt(pmax(diag(finalPosterior$covariance[,,ii]), 0)))), na.rm = TRUE) else NA_real_,
        maxPosteriorSD = if("covariance" %in% names(finalPosterior)) max(unlist(lapply(seq_len(dim(finalPosterior$covariance)[3]), function(ii) sqrt(pmax(diag(finalPosterior$covariance[,,ii]), 0)))), na.rm = TRUE) else NA_real_,
        personConverged = all(finalPosterior$converged),
        personStepSec = NA_real_,
        itemStepSec = directSec,
        refreshStepSec = NA_real_,
        objectiveEvalSec = NA_real_,
        outerIterSec = directSec,
        itemTargetEvals = directFit$optim$target_evals,
        itemMaskedGradNorm = directFit$optim$masked_grad_norm,
        optimizerIter = if(!is.null(directFit$optim$iter)) directFit$optim$iter else NA_integer_,
        optimizerTerminate = direct_terminate,
        optimizerTerminateValue = if(!is.null(directFit$optim$terminate$val)) directFit$optim$terminate$val else NA_real_,
        personMeanNiter = mean(finalPosterior$niter, na.rm = TRUE),
        personMaxNiter = max(finalPosterior$niter, na.rm = TRUE),
        strictCriterion = fit$laplaceStatus$converged,
        stabilityCriterion = FALSE,
        recentObjectiveRange = NA_real_,
        recentGradRelChange = NA_real_,
        recentItemStepMean = NA_real_,
        recentPersonStepMean = NA_real_,
        strictStreak = if(fit$laplaceStatus$converged) 1L else 0L,
        stabilityStreak = 0L
      )
    }
    laplace_trace(1, sprintf(
      "Direct Laplace: obj=%.6f | approx_grad=%.3g | target_evals=%d | person_conv=%s | t(total)=%.2fs",
      directFit$eval$value,
      directFit$optim$masked_grad_norm,
      directFit$optim$target_evals,
      if(all(finalPosterior$converged)) "yes" else "no",
      directSec
    ))
  }

  if(identical(marginalApprox, "laplace_em") && length(which(sdat$Abilityparsindex > 0)) > 0){
    optimdots <- list(...)
    laplaceVerbose <- if("verbose" %in% names(optimdots)) as.integer(optimdots$verbose) else 0L
    if(requireNamespace("RcppParallel", quietly = TRUE)) RcppParallel::setThreadOptions(numThreads = max(1L, as.integer(cores)))
    laplace_trace <- function(level, ...){
      if(laplaceVerbose >= level) message(...)
    }
    wall_time_sec <- function() as.numeric(proc.time()[["elapsed"]])

    if(sdat$NpersonPreds > 0){
      warning("laplace_em currently keeps Abilitybeta fixed during the Laplace outer loop when person predictors are present.")
    }

    state <- bigIRT_laplace_initial_state(sdat, eps = sampledAbilityJitter)
    priorPrecision <- bigIRT_laplace_prior_precision_array(sdat, jitter = sampledAbilityJitter)
    laplaceDiag <- list()
    lastObjective <- NA_real_
    strictConvergedStreak <- 0L
    collectLaplaceDiag <- isTRUE(laplaceDiagnostics) || isTRUE(laplacePlot)
    laplaceStatus <- list(
      converged = FALSE,
      reason = "max_outer_iter",
      outer_iters = 0L,
      beta_frozen = sdat$NpersonPreds > 0,
      initialized_from = "prior_anchored",
      strict_streak = 0L,
      stability_streak = 0L
    )
    finalPosterior <- NULL

    laplace_trace(1, sprintf(
      "Laplace EM: starting prior-anchored fit with %d persons, %d items, %d dimensions, max_outer=%d.",
      sdat$Nsubs, sdat$Nitems, sdat$Nscales, laplaceOuterIter
    ))

    laplaceMaterializeFit <- function(base_fit, state, posterior, objective){
      base_fit$pars <- bigIRT_laplace_constrained_pars(state, sdat, posterior = posterior)
      base_fit$optim <- list(
        method = "laplace_em",
        logLik = objective,
        par = numeric()
      )
      base_fit$dat <- sdat
      apply_fit_dimnames(base_fit)
    }

    for(outeri in seq_len(laplaceOuterIter)){
      outerStart <- wall_time_sec()
      personBase <- state$AbilityBase
      itemBase <- bigIRT_laplace_pack_item_state(state, sdat)

      laplace_trace(2, sprintf("Laplace EM iter %d/%d: person mode step", outeri, laplaceOuterIter))
      t_person1 <- wall_time_sec()
      personStep <- bigIRT_laplace_person_step(
        state = state,
        sdat = sdat,
        prior_precision = priorPrecision,
        jitter = sampledAbilityJitter,
        max_iter = max(4, as.integer(noptimsteps * 2L)),
        tol = laplacePersonTol,
        keep_covariance = isTRUE(laplaceKeepCovariance) || isTRUE(laplaceDiagnostics),
        cores = cores
      )
      personStepSec <- wall_time_sec() - t_person1
      state <- personStep$state

      laplace_trace(2, sprintf("Laplace EM iter %d/%d: item step", outeri, laplaceOuterIter))
      t_item <- wall_time_sec()
      itemStep <- bigIRT_laplace_optimize_item(
        state = state,
        sdat = sdat,
        thetaBase = state$AbilityBase,
        prior_precision = priorPrecision,
        niter = max(2L, as.integer(noptimsteps)),
        tol = laplaceTol,
        jitter = sampledAbilityJitter,
        cores = cores
      )
      itemStepSec <- wall_time_sec() - t_item
      state <- itemStep$state

      laplace_trace(2, sprintf("Laplace EM iter %d/%d: posterior refresh", outeri, laplaceOuterIter))
      t_refresh <- wall_time_sec()
      refreshStep <- bigIRT_laplace_person_step(
        state = state,
        sdat = sdat,
        prior_precision = priorPrecision,
        jitter = sampledAbilityJitter,
        max_iter = max(20L, as.integer(noptimsteps * 2L)),
        tol = laplacePersonTol,
        keep_covariance = TRUE,
        cores = cores
      )
      refreshStepSec <- wall_time_sec() - t_refresh
      state <- refreshStep$state
      finalPosterior <- refreshStep$posterior

      t_eval <- wall_time_sec()
      objNow <- bigIRT_laplace_item_objective(
        par = bigIRT_laplace_pack_item_state(state, sdat),
        state = state,
        sdat = sdat,
        thetaBase = state$AbilityBase,
        prior_precision = priorPrecision,
        jitter = sampledAbilityJitter
      )
      objectiveEvalSec <- wall_time_sec() - t_eval
      itemStepRms <- if(length(itemBase)) sqrt(mean((bigIRT_laplace_pack_item_state(state, sdat) - itemBase)^2)) else 0
      personStepRms <- sqrt(mean((state$AbilityBase - personBase)^2))
      relImprove <- if(is.finite(lastObjective)) abs(objNow$value - lastObjective) / max(1, abs(lastObjective)) else Inf
      outerSec <- wall_time_sec() - outerStart
      itemGradNorm <- sqrt(sum(objNow$grad^2))

      posteriorSDVec <- if(!is.null(finalPosterior$covariance)){
        unlist(lapply(seq_len(dim(finalPosterior$covariance)[3]), function(ii){
          sqrt(pmax(diag(finalPosterior$covariance[,,ii]), 0))
        }))
      } else {
        numeric()
      }
      strictCriterion <- is.finite(lastObjective) &&
        relImprove < laplaceTol &&
        itemStepRms < laplaceTol &&
        personStepRms < laplaceTol &&
        itemGradNorm < laplaceGradTol &&
        all(finalPosterior$converged)

      recent_diag <- data.table::rbindlist(c(laplaceDiag, list(data.frame(
        outerIter = as.integer(outeri),
        objective = objNow$value,
        relativeImprove = relImprove,
        itemStepRms = itemStepRms,
        personStepRms = personStepRms,
        itemGradNorm = itemGradNorm,
        personConverged = all(finalPosterior$converged),
        stringsAsFactors = FALSE
      ))), fill = TRUE)
      recent_window_n <- min(nrow(recent_diag), max(1L, as.integer(laplaceStabilityIter)))
      recent_window <- utils::tail(recent_diag, recent_window_n)
      recent_obj_range <- if(recent_window_n >= 2L) {
        diff(range(recent_window$objective, na.rm = TRUE)) / max(1, abs(mean(recent_window$objective, na.rm = TRUE)))
      } else Inf
      recent_grad_rel_change <- if(recent_window_n >= 2L){
        abs(recent_window$itemGradNorm[recent_window_n] - recent_window$itemGradNorm[1]) /
          max(1, abs(recent_window$itemGradNorm[1]))
      } else Inf
      recent_item_step_mean <- mean(recent_window$itemStepRms, na.rm = TRUE)
      recent_person_step_mean <- mean(recent_window$personStepRms, na.rm = TRUE)
      stabilityCriterion <- recent_window_n >= max(1L, as.integer(laplaceStabilityIter)) &&
        recent_obj_range < laplaceTol &&
        recent_grad_rel_change < max(10 * laplaceGradTol, 0.1) &&
        recent_item_step_mean < max(10 * laplaceTol, 1e-2) &&
        recent_person_step_mean < max(200 * laplaceTol, 0.2) &&
        all(recent_window$personConverged %in% TRUE)

      laplaceDiag[[length(laplaceDiag)+1]] <- data.frame(
        outerIter = as.integer(outeri),
        objective = objNow$value,
        relativeImprove = relImprove,
        itemStepRms = itemStepRms,
        personStepRms = personStepRms,
        itemGradNorm = itemGradNorm,
        meanPosteriorSD = if(length(posteriorSDVec)) mean(posteriorSDVec, na.rm = TRUE) else NA_real_,
        maxPosteriorSD = if(length(posteriorSDVec)) max(posteriorSDVec, na.rm = TRUE) else NA_real_,
        personConverged = all(finalPosterior$converged),
        personStepSec = personStepSec,
        itemStepSec = itemStepSec,
        refreshStepSec = refreshStepSec,
        objectiveEvalSec = objectiveEvalSec,
        outerIterSec = outerSec,
        itemTargetEvals = if(!is.null(itemStep$optim$target_evals)) itemStep$optim$target_evals else NA_integer_,
        itemMaskedGradNorm = if(!is.null(itemStep$optim$masked_grad_norm)) itemStep$optim$masked_grad_norm else NA_real_,
        personMeanNiter = mean(finalPosterior$niter, na.rm = TRUE),
        personMaxNiter = max(finalPosterior$niter, na.rm = TRUE),
        strictCriterion = strictCriterion,
        stabilityCriterion = stabilityCriterion,
        recentObjectiveRange = recent_obj_range,
        recentGradRelChange = recent_grad_rel_change,
        recentItemStepMean = recent_item_step_mean,
        recentPersonStepMean = recent_person_step_mean,
        strictStreak = strictConvergedStreak,
        stabilityStreak = if(stabilityCriterion) recent_window_n else 0L,
        stringsAsFactors = FALSE
      )

      if(isTRUE(laplacePlot) && (outeri %% max(1L, as.integer(laplacePlotEvery)) == 0L)){
        bigIRT_plot_laplace_diag_df(data.table::rbindlist(laplaceDiag, fill = TRUE), logGrad = TRUE, showTiming = TRUE)
      }

      laplace_trace(1, sprintf(
        paste(
          "Laplace EM iter %d/%d | obj=%.6f | rel=%.3g | item_rms=%.3g | person_rms=%.3g |",
          "grad=%.3g | postSD(mean/max)=%.3g/%.3g | person_conv=%s |",
          "t(person/item/refresh/eval/total)=%.2fs/%.2fs/%.2fs/%.2fs/%.2fs"
        ),
        outeri, laplaceOuterIter,
        objNow$value, relImprove, itemStepRms, personStepRms,
        sqrt(sum(objNow$grad^2)),
        if(length(posteriorSDVec)) mean(posteriorSDVec, na.rm = TRUE) else NA_real_,
        if(length(posteriorSDVec)) max(posteriorSDVec, na.rm = TRUE) else NA_real_,
        if(all(finalPosterior$converged)) "yes" else "no",
        personStepSec, itemStepSec, refreshStepSec, objectiveEvalSec, outerSec
      ))

      laplaceStatus$outer_iters <- outeri
      laplaceStatus$last_outer_seconds <- outerSec
      laplaceStatus$last_item_grad_norm <- itemGradNorm

      if(strictCriterion){
        strictConvergedStreak <- strictConvergedStreak + 1L
      } else {
        strictConvergedStreak <- 0L
      }

      laplaceStatus$strict_streak <- strictConvergedStreak
      laplaceStatus$stability_streak <- if(stabilityCriterion) recent_window_n else 0L

      if(strictConvergedStreak >= 2L){
        laplaceStatus$converged <- TRUE
        laplaceStatus$reason <- "strict_tolerance"
        break
      }
      if(stabilityCriterion){
        laplaceStatus$converged <- TRUE
        laplaceStatus$reason <- "stability_patience"
        break
      }
      lastObjective <- objNow$value
    }

    if(is.null(finalPosterior)){
      laplace_trace(2, "Laplace EM: no completed outer iteration; running one posterior refresh for final output.")
      refreshStep <- bigIRT_laplace_person_step(
        state = state,
        sdat = sdat,
        prior_precision = priorPrecision,
        jitter = sampledAbilityJitter,
        max_iter = max(20L, as.integer(noptimsteps * 2L)),
        tol = laplacePersonTol,
        keep_covariance = TRUE,
        cores = cores
      )
      state <- refreshStep$state
      finalPosterior <- refreshStep$posterior
      lastObjective <- bigIRT_laplace_item_objective(
        par = bigIRT_laplace_pack_item_state(state, sdat),
        state = state,
        sdat = sdat,
        thetaBase = state$AbilityBase,
        prior_precision = priorPrecision,
        jitter = sampledAbilityJitter
      )$value
    }

    fit <- list(pars = list(), optim = list(), dat = sdat)
    fit <- laplaceMaterializeFit(fit, state, finalPosterior, lastObjective)
    fit$personPosterior <- list(
      mode = finalPosterior$theta_mode,
      precision = finalPosterior$precision,
      precision_chol = finalPosterior$precision_chol,
      logdet_precision = finalPosterior$logdet_precision,
      covariance = if("covariance" %in% names(finalPosterior)) finalPosterior$covariance else NULL,
      niter = finalPosterior$niter,
      converged = finalPosterior$converged
    )
    fit$laplaceStatus <- laplaceStatus
    if(collectLaplaceDiag){
      fit$laplaceDiagnostics <- data.table::rbindlist(laplaceDiag, fill = TRUE)
    }
  }

  if(sampledAbilityStep && length(which(sdat$Abilityparsindex > 0)) > 0){
    # Laplace-EM outer loop:
    # 1. update person modes conditional on the current item state,
    # 2. refresh the Laplace posterior and deterministic support,
    # 3. update item parameters against the frozen posterior support,
    # 4. evaluate convergence on the resulting outer iterate.
    #
    # This intentionally avoids the old sampled-ability acceptance/rejection
    # state machine. The earlier scheme mixed JML-style person updates with
    # uncertainty-aware item updates and required a large amount of damping and
    # sigma-scale bookkeeping to stay stable. Here we use a simpler generalized
    # EM structure: frozen Laplace posterior in the M-step, then refresh.
    layout <- bigIRT_param_layout(sdat)
    sampledAbilityHistory <- list()
    sampledAbilityDiag <- list()
    sampledAbilityTiming <- list()
    collectSampledAbilityDiag <- isTRUE(sampledAbilityDiagnostics) || isTRUE(sampledAbilityPlot)
    optimdots <- list(...)
    sampledVerbose <- if("verbose" %in% names(optimdots)) optimdots$verbose else 0
    sampledPlot <- if("plot" %in% names(optimdots)) optimdots$plot else 0
    sampledOpt <- bigIRT_sampled_optimizer_setup(
      standata = sdat,
      cores = cores,
      verbose = sampledVerbose,
      plot = sampledPlot
    )
    on.exit(sampledOpt$cleanup(), add = TRUE)
    sampled_step <- function(init, objective){
      bigIRT_sampled_optimizer_step(
        engine = sampledOpt,
        objective = objective,
        Niter = noptimsteps,
        tol = basetol,
        init = init,
        materialize_fit = FALSE
      )
    }
    wall_time_sec <- function() as.numeric(proc.time()[["elapsed"]])
    fullInit <- if(is.na(init[1])) rep(0, sampledOpt$npars_full) else init
    if(sampledAbilityOuterIter < 1) stop("sampledAbilityOuterIter must be >= 1 when sampledAbilityStep=TRUE.")
    sampledControl <- bigIRT_sampled_build_control(
      sampledAbilitySigmaScale = sampledAbilitySigmaScale,
      sampledAbilityStepTol = sampledAbilityStepTol,
      sampledAbilitySpreadTol = sampledAbilitySpreadTol,
      sampledAbilityPatience = sampledAbilityPatience,
      sampledAbilityControl = sampledAbilityControl,
      noptimgradtol = noptimgradtol
    )
    currentPar <- fullInit
    posterior <- NULL
    priorSD <- pmax(as.numeric(sdat$AbilitySD), sampledAbilityJitter)
    priorCov <- diag(priorSD, length(priorSD)) %*% sdat$AbilityCorr %*% diag(priorSD, length(priorSD))
    sampledPriorPrec <- solve(priorCov + diag(sampledAbilityJitter, nrow(priorCov)))
    acceptedWindow <- list()
    acceptedOuterIter <- 0L
    rejectedOuterIter <- 0L
    worseningConsecutive <- 0L
    lastObservedCombinedGrad <- NA_real_
    lastAcceptedPosteriorMetrics <- NULL
    sampledStatus <- list(
      converged = FALSE,
      reason = "max_outer_iter",
      accepted_outer_iters = 0L,
      rejected_outer_iters = 0L
    )
    fit <- NULL

    for(outeri in seq_len(sampledAbilityOuterIter)){
      sigmaScaleUsed <- sampledControl$sigmaScale
      basePar <- currentPar
      rejectReason <- ""
      personStepDamping <- 1
      itemStepDamping <- 1
      personObjectiveBefore <- NA_real_
      personObjectiveAfter <- NA_real_
      itemObjectiveBefore <- NA_real_
      itemObjectiveAfter <- NA_real_
      meanPosteriorSD_ratio <- NA_real_
      maxPosteriorSD_ratio <- NA_real_
      personOptElapsed <- 0
      posteriorElapsed <- 0
      itemOptElapsed <- 0
      itemTargetEvals <- 0L
      itemLogProbEvals <- 0L
      itemGradNorm <- NA_real_
      combinedGradNorm <- NA_real_
      acceptedOuter <- TRUE
      personDiagRow <- NULL
      itemDiagRow <- NULL
      personFit <- NULL
      itemFit <- NULL

      message(paste0("Laplace EM outer step ", outeri, ": person update"))
      personObjective <- bigIRT_sampled_make_objective(
        engine = sampledOpt,
        free_par_index = layout$person,
        fixed_par_full = basePar
      )
      t0_person_opt <- wall_time_sec()
      personStep <- sampled_step(
        init = basePar[layout$person],
        objective = personObjective
      )
      personOptElapsed <- wall_time_sec() - t0_person_opt
      personGradNorm <- personStep$optim$masked_grad_norm
      personTargetEvals <- if(!is.null(personStep$optim$target_evals)) personStep$optim$target_evals else NA_integer_
      personLogProbEvals <- if(!is.null(personStep$optim$logprob_evals)) personStep$optim$logprob_evals else NA_integer_
      personObjectiveBefore <- bigIRT_sampled_eval_objective(personObjective, basePar)
      personObjectiveAfter <- bigIRT_sampled_eval_objective(personObjective, personStep$optim$par)
      personFit <- bigIRT_sampled_materialize_fit(
        sampledOpt,
        personStep$optim$par,
        optim = utils::modifyList(personStep$optim, list(par = personStep$optim$par))
      )
      personFit <- apply_fit_dimnames(personFit)

      t0_post <- wall_time_sec()
      postSigma <- bigIRT_person_posterior_and_sigma(
        fit = personFit,
        sdat = sdat,
        layout = layout,
        jitter = sampledAbilityJitter,
        sigmaScale = sigmaScaleUsed,
        priorPrec = sampledPriorPrec
      )
      posteriorCandidate <- postSigma$posterior
      posteriorElapsed <- wall_time_sec() - t0_post
      sigmaTemplates <- postSigma$sigmaTemplates
      posteriorMetrics <- bigIRT_sampled_posterior_metrics(posteriorCandidate)
      if(!is.null(lastAcceptedPosteriorMetrics)){
        meanPosteriorSD_ratio <- posteriorMetrics$meanPosteriorSD / pmax(lastAcceptedPosteriorMetrics$meanPosteriorSD, sampledAbilityJitter)
        maxPosteriorSD_ratio <- posteriorMetrics$maxPosteriorSD / pmax(lastAcceptedPosteriorMetrics$maxPosteriorSD, sampledAbilityJitter)
      } else {
        meanPosteriorSD_ratio <- 1
        maxPosteriorSD_ratio <- 1
      }
      if(collectSampledAbilityDiag && !is.null(personFit)){
        personDiagRow <- bigIRT_sampled_diag_snapshot(
          personFit,
          layout,
          stage = "person",
          outerIter = outeri,
          posterior = posteriorCandidate,
          sigmaScale = sigmaScaleUsed,
          sigmaScaleUsed = sigmaScaleUsed,
          personGradNorm = personGradNorm,
          prevPar = basePar,
          accepted = NA,
          rejected = NA,
          personStepDamping = personStepDamping,
          personObjectiveBefore = personObjectiveBefore,
          personObjectiveAfter = personObjectiveAfter,
          meanPosteriorSD_ratio = meanPosteriorSD_ratio,
          maxPosteriorSD_ratio = maxPosteriorSD_ratio
        )
      }

      message(paste0("Laplace EM outer step ", outeri, ": item update"))
      itemObjective <- bigIRT_sampled_make_objective(
        engine = sampledOpt,
        free_par_index = layout$item,
        fixed_par_full = personStep$optim$par,
        fixed_par_samples = sigmaTemplates$samples,
        sample_weights = sigmaTemplates$weights
      )
      t0_item_opt <- wall_time_sec()
      itemStep <- sampled_step(
        init = personStep$optim$par[layout$item],
        objective = itemObjective
      )
      itemOptElapsed <- wall_time_sec() - t0_item_opt
      itemGradNorm <- itemStep$optim$masked_grad_norm
      itemTargetEvals <- if(!is.null(itemStep$optim$target_evals)) itemStep$optim$target_evals else NA_integer_
      itemLogProbEvals <- if(!is.null(itemStep$optim$logprob_evals)) itemStep$optim$logprob_evals else NA_integer_
      itemObjectiveBefore <- bigIRT_sampled_eval_objective(itemObjective, personStep$optim$par)
      itemObjectiveAfter <- bigIRT_sampled_eval_objective(itemObjective, itemStep$optim$par)
      combinedGradNorm <- suppressWarnings(sqrt(itemGradNorm^2 + personGradNorm^2))
      itemFit <- bigIRT_sampled_materialize_fit(
        sampledOpt,
        itemStep$optim$par,
        optim = utils::modifyList(itemStep$optim, list(par = itemStep$optim$par))
      )
      itemFit <- apply_fit_dimnames(itemFit)

      totalOuterElapsed <- itemOptElapsed + personOptElapsed + posteriorElapsed
      if(collectSampledAbilityDiag && !is.null(personDiagRow)){
        personDiagRow$accepted <- acceptedOuter
        personDiagRow$rejected <- FALSE
        personDiagRow$reject_reason <- ""
        personDiagRow$itemGradNorm <- itemGradNorm
        personDiagRow$combinedGradNorm <- sqrt(sum(c(itemGradNorm, personGradNorm)^2, na.rm = TRUE))
        sampledAbilityDiag[[length(sampledAbilityDiag)+1]] <- personDiagRow
      }
      if(collectSampledAbilityDiag && !is.null(itemFit)){
        itemDiagRow <- bigIRT_sampled_diag_snapshot(
          itemFit,
          layout,
          stage = "item",
          outerIter = outeri,
          posterior = posteriorCandidate,
          sigmaScale = sigmaScaleUsed,
          sigmaScaleUsed = sigmaScaleUsed,
          itemGradNorm = itemGradNorm,
          personGradNorm = personGradNorm,
          prevPar = personStep$optim$par,
          accepted = acceptedOuter,
          rejected = FALSE,
          reject_reason = "",
          personStepDamping = personStepDamping,
          itemStepDamping = itemStepDamping,
          personObjectiveBefore = personObjectiveBefore,
          personObjectiveAfter = personObjectiveAfter,
          itemObjectiveBefore = itemObjectiveBefore,
          itemObjectiveAfter = itemObjectiveAfter,
          meanPosteriorSD_ratio = meanPosteriorSD_ratio,
          maxPosteriorSD_ratio = maxPosteriorSD_ratio
        )
        sampledAbilityDiag[[length(sampledAbilityDiag)+1]] <- itemDiagRow
      }
      currentPar <- itemStep$optim$par
      fit <- itemFit
      posterior <- posteriorCandidate
      acceptedOuterIter <- acceptedOuterIter + 1L
      if(bigIRT_sampled_gradient_worsened(combinedGradNorm, lastObservedCombinedGrad)){
        worseningConsecutive <- worseningConsecutive + 1L
      } else if(is.finite(combinedGradNorm)) {
        worseningConsecutive <- 0L
      }
      if(is.finite(combinedGradNorm)) lastObservedCombinedGrad <- combinedGradNorm

      acceptedMetrics <- list(
        accepted = TRUE,
        combinedGradNorm = combinedGradNorm,
        itemStepRms = if(!is.null(itemDiagRow)) itemDiagRow$itemStepRms else sqrt(mean((currentPar[layout$item] - personStep$optim$par[layout$item])^2)),
        personStepRms = if(!is.null(personDiagRow)) personDiagRow$personStepRms else sqrt(mean((personStep$optim$par[layout$person] - basePar[layout$person])^2)),
        meanPosteriorSD_ratio = meanPosteriorSD_ratio
      )
      acceptedWindow <- bigIRT_sampled_update_window(acceptedWindow, acceptedMetrics, sampledControl)
      sampledAbilityHistory[[length(sampledAbilityHistory)+1]] <- list(
        outerIter = outeri,
        AbilityMeandat = posterior$meanPrior,
        AbilitySD = posterior$sdPrior,
        AbilityCorr = posterior$corrPrior,
        sigmaScale = sigmaScaleUsed,
        itemGradNorm = itemGradNorm,
        personGradNorm = personGradNorm,
        combinedGradNorm = combinedGradNorm,
        meanPosteriorSD_ratio = meanPosteriorSD_ratio,
        maxPosteriorSD_ratio = maxPosteriorSD_ratio,
        timing = list(
          item_opt_sec = itemOptElapsed,
          person_opt_sec = personOptElapsed,
          posterior_sec = posteriorElapsed,
          total_sec = totalOuterElapsed
        )
      )
      lastAcceptedPosteriorMetrics <- posteriorMetrics
      if(bigIRT_sampled_window_converged(acceptedWindow, sampledControl)){
        sampledStatus$converged <- TRUE
        sampledStatus$reason <- "accepted_window_below_tolerance"
      }

      sampledAbilityTiming[[length(sampledAbilityTiming)+1]] <- data.frame(
        outerIter = as.integer(outeri),
        stage = "outer",
        accepted = acceptedOuter,
        rejected = FALSE,
        reject_reason = "",
        sigmaScaleUsed = sigmaScaleUsed,
        personStepDamping = personStepDamping,
        itemStepDamping = itemStepDamping,
        personObjectiveBefore = personObjectiveBefore,
        personObjectiveAfter = personObjectiveAfter,
        itemObjectiveBefore = itemObjectiveBefore,
        itemObjectiveAfter = itemObjectiveAfter,
        meanPosteriorSD_ratio = meanPosteriorSD_ratio,
        maxPosteriorSD_ratio = maxPosteriorSD_ratio,
        cumulativeMeanPosteriorSD_ratio = NA_real_,
        cumulativeMaxPosteriorSD_ratio = NA_real_,
        sdAbility_ratio = NA_real_,
        combinedGradNorm = combinedGradNorm,
        person_opt_sec = personOptElapsed,
        posterior_sec = posteriorElapsed,
        item_opt_sec = itemOptElapsed,
        person_target_evals = as.integer(personTargetEvals),
        item_target_evals = as.integer(itemTargetEvals),
        person_logprob_evals = as.integer(personLogProbEvals),
        item_logprob_evals = as.integer(itemLogProbEvals),
        total_sec = totalOuterElapsed
      )

      if(collectSampledAbilityDiag && isTRUE(sampledAbilityPlot) && outeri %% sampledAbilityPlotEvery == 0){
        diagdf <- data.table::rbindlist(sampledAbilityDiag, fill = TRUE)
        try(bigIRT_plot_sampled_diag_df(as.data.frame(diagdf)), silent = TRUE)
      }

      sampledStatus$accepted_outer_iters <- acceptedOuterIter
      sampledStatus$rejected_outer_iters <- rejectedOuterIter
      if(isTRUE(sampledStatus$converged)){
        message(paste0("Laplace EM outer steps converged at iteration ", outeri,
          " with combined gradient norm ", signif(combinedGradNorm, 4)))
        break
      }
      if(worseningConsecutive >= sampledControl$max_worsening_outer){
        sampledStatus$reason <- "repeated_worsening_combined_gradient"
        break
      }
    }
    if(is.null(fit)){
      fit <- apply_fit_dimnames(bigIRT_sampled_materialize_fit(sampledOpt, currentPar, optim = list(par = currentPar)))
    }
    fit$sampledAbilityHistory <- sampledAbilityHistory
    fit$sampledAbilityTiming <- as.data.frame(data.table::rbindlist(sampledAbilityTiming, fill = TRUE))
    if(collectSampledAbilityDiag){
      fit$sampledAbilityDiagnostics <- as.data.frame(data.table::rbindlist(sampledAbilityDiag, fill = TRUE))
    }
    fit$sampledAbilityControl <- sampledControl
    fit$sampledAbilityStatus <- sampledStatus
    fit$personPosterior <- posterior
  }

  if(normalise){   #normalise pars
    if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2 && ncol(fit$pars$A) > 1){
      abilityCorrNorm <- NULL
      if(!is.null(fit$abilityPrior$corr)) abilityCorrNorm <- fit$abilityPrior$corr
      normpars <- normaliseMIRT(
        B = fit$pars$B,
        Ability = fit$pars$Ability,
        A = fit$pars$A,
        AbilityCorr = abilityCorrNorm,
        normaliseScale = normaliseScale,
        normaliseMean = normaliseMean
      )
      fit$pars$Ability <- normpars$Ability
      fit$pars$B <- normpars$B
      fit$pars$A <- normpars$A

      if(!is.null(fit$personPosterior$mode)){
        fit$personPosterior$mode <- sweep(fit$personPosterior$mode, 2, normpars$center, "-") %*%
          normpars$inv_chol / normpars$global_scale
        fit$personPosterior$mode <- sweep(fit$personPosterior$mode, 2, normpars$latent_shift, "-")
      }
      if(!is.null(fit$personPosterior$covariance)){
        K <- ncol(fit$pars$Ability)
        U <- normpars$chol_cov
        Uinv <- normpars$inv_chol
        scale2 <- normpars$global_scale^2
        for(ii in seq_len(dim(fit$personPosterior$covariance)[3])){
          cov_old <- fit$personPosterior$covariance[,,ii]
          fit$personPosterior$covariance[,,ii] <- t(Uinv) %*% cov_old %*% Uinv / scale2
        }
      }
      if(!is.null(fit$personPosterior$precision)){
        U <- normpars$chol_cov
        scale2 <- normpars$global_scale^2
        for(ii in seq_len(dim(fit$personPosterior$precision)[3])){
          prec_old <- fit$personPosterior$precision[,,ii]
          fit$personPosterior$precision[,,ii] <- scale2 * U %*% prec_old %*% t(U)
        }
      }
      if(!is.null(fit$personPosterior$precision_chol) && !is.null(fit$personPosterior$precision)){
        for(ii in seq_len(dim(fit$personPosterior$precision)[3])){
          llt <- chol(fit$personPosterior$precision[,,ii])
          fit$personPosterior$precision_chol[,,ii] <- t(llt)
          fit$personPosterior$logdet_precision[ii] <- 2 * sum(log(diag(llt)))
        }
      }
      if(!is.null(fit$abilityPrior)){
        fit$abilityPrior$sd <- apply(fit$pars$Ability, 2, stats::sd, na.rm = TRUE)
        fit$abilityPrior$corr <- stats::cor(fit$pars$Ability, use = "pairwise.complete.obs")
        fit$abilityPrior$precision <- solve(
          diag(fit$abilityPrior$sd, ncol(fit$pars$Ability)) %*%
            fit$abilityPrior$corr %*%
            diag(fit$abilityPrior$sd, ncol(fit$pars$Ability)) +
            diag(1e-8, ncol(fit$pars$Ability))
        )
      }
    } else {
      for(i in 1:ncol(fit$pars$Ability)){
        selector <- rownames(fit$pars$B) %in% itemSetup$original[itemSetup$scale %in% i]
        a_for_scale <- if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2){
          fit$pars$A[selector,i]
        } else {
          fit$pars$A[selector]
        }

        normpars <- normaliseIRT(B = fit$pars$B[selector],
          Ability = fit$pars$Ability[,i],
          A=a_for_scale,normaliseScale = normaliseScale, normaliseMean = normaliseMean)

        fit$pars$Ability[,i] <- normpars$Ability

        fit$pars$B[selector]  <- normpars$B
        if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2){
          fit$pars$A[selector,i] <-  normpars$A
        } else {
          fit$pars$A[selector] <-  normpars$A
        }
      }
    }
  }
  ###compute some output details
  itemA <- if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2){
    fit$pars$A[cbind(seq_len(nrow(fit$pars$A)), itemSetup$scale)]
  } else {
    fit$pars$A
  }
  item_labels <- rownames(fit$pars$B)
  if(is.null(item_labels) || length(item_labels) == 0L){
    item_labels <- rownames(fit$pars$A)
  }
  if(is.null(item_labels) || length(item_labels) == 0L){
    item_labels <- names(fit$pars$B)
  }
  if(is.null(item_labels) || length(item_labels) == 0L){
    item_labels <- as.character(unique(itemSetup$original))
  }
  fit$itemPars <- data.frame(item=item_labels,A=itemA,B=fit$pars$B,C=fit$pars$C,D=fit$pars$D)
  colnames(fit$itemPars)[1] <- item
  if(!is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2 && ncol(fit$pars$A) > 1){
    loadingCols <- as.data.frame(fit$pars$A)
    colnames(loadingCols) <- paste0("A_", colnames(fit$pars$A))
    fit$itemPars <- cbind(fit$itemPars, loadingCols)
  }
  if(ncol(itemPreds)>0){
    colnames(fit$pars$itemPredsMean) <- colnames(itemPreds)
    fit$itemPars <- cbind(fit$itemPars, fit$pars$itemPredsMean)
  }

  fit$personPars <- data.frame(id=rownames(fit$pars$Ability),fit$pars$Ability)
  colnames(fit$personPars)[1] = id
  if((isTRUE(sampledAbilityStep) || identical(marginalApprox, "laplace_em")) && !is.null(fit$pars$sAbilitySD)){
    abilitySD <- fit$pars$sAbilitySD
    if(is.null(dim(abilitySD))) abilitySD <- matrix(abilitySD, ncol = ncol(fit$pars$Ability))
    colnames(abilitySD) <- paste0(colnames(fit$pars$Ability), "_SD")
    fit$personPars <- cbind(fit$personPars, abilitySD)
  }
  if(ncol(personPreds)>0){
    colnames(fit$pars$personPredsMean) <- colnames(personPreds)
    fit$personPars <- cbind(fit$personPars, fit$pars$personPredsMean)
  }

  ###Covariate effect summary
  fit$covariateEffects <- list()
  if(fit$dat$NpersonPreds > 0){
    colnames(fit$pars$Abilitybeta) <- colnames(personPreds)
    fit$covariateEffects$Ability <- fit$pars$Abilitybeta
    fit$covariateEffects$AbilityStd <- fit$pars$Abilitybeta * apply(fit$dat$personPreds,2,sd) / sd(fit$pars$Ability)
  }
  if(fit$dat$NAitemPreds > 0 && pl > 1){
    dimnames(fit$pars$Abeta)[[2]] <- (AitemPreds)
    fit$covariateEffects$A <- fit$pars$Abeta
    fit$covariateEffects$AStd <- t(t(fit$pars$Abeta) * apply(fit$dat$itemPreds[,fit$dat$AitemPreds,drop=FALSE],2,sd) / sd(fit$pars$A))
  }
  if(fit$dat$NBitemPreds > 0){
    dimnames(fit$pars$Bbeta)[[2]] <- (BitemPreds)
    fit$covariateEffects$B<- fit$pars$Bbeta
    fit$covariateEffects$BStd <- t(t(fit$pars$Bbeta) * apply(fit$dat$itemPreds[,fit$dat$BitemPreds,drop=FALSE],2,sd) / sd(fit$pars$B))
  }
  if(fit$dat$NCitemPreds > 0 && pl > 2){
    dimnames(fit$pars$Cbeta)[[2]] <- (CitemPreds)
    fit$covariateEffects$C<- fit$pars$Cbeta
    fit$covariateEffects$CStd <- t(t(fit$pars$Cbeta) * apply(fit$dat$itemPreds[,fit$dat$CitemPreds,drop=FALSE],2,sd) / sd(fit$pars$C))
  }
  if(fit$dat$NDitemPreds > 0 && pl > 3){
    dimnames(fit$pars$Dbeta)[[2]] <- (DitemPreds)
    fit$covariateEffects$D<- fit$pars$Dbeta
    fit$covariateEffects$DStd <- t(t(fit$pars$Dbeta) * apply(fit$dat$itemPreds[,fit$dat$DitemPreds,drop=FALSE],2,sd) / sd(fit$pars$D))
  }

  if(all(c("b_row", "c_row", "d_row", "row_loadings") %in% names(fit$pars))){
    fit$rowEffective <- list(
      id = fit$dat$id,
      theta_mean = fit$pars$Ability,
      b = fit$pars$b_row,
      c = fit$pars$c_row,
      d = fit$pars$d_row,
      loadings = fit$pars$row_loadings,
      source = "stan_gq"
    )
  }

  return(fit)
}
