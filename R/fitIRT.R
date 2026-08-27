

## exp(x)/(1+exp(x)) overflows to NaN for x >~ 710.  plogis is the stable
## equivalent and matters whenever a fitted discrimination is extreme.
inv_logit <- function(x) stats::plogis(x)
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

checkP <- function(fit){
  ## Recomputes the per-response probabilities in R, to check the Stan/C++ ones.
  ## Everything here is indexed in fit$dat's internal (person-sorted) order, but
  ## the result is returned in the caller's input row order so that it lines up
  ## element-for-element with fit$pars$p.  Without that final mapping the two
  ## agree only when the input happened to be sorted by person already.
  p=rep(NA,fit$dat$Nobs)
  use_matrix_a <- !is.null(dim(fit$pars$A)) && length(dim(fit$pars$A)) == 2
  for(i in 1:length(p)){
    if(use_matrix_a){
      aload <- fit$pars$A[fit$dat$item[i],]
      eta <- sum(aload * fit$pars$Ability[fit$dat$id[i],]) - fit$itemPars$B[fit$dat$item[i]]
      pmid <- fit$itemPars$C[fit$dat$item[i]] + (fit$itemPars$D[fit$dat$item[i]] - fit$itemPars$C[fit$dat$item[i]]) * inv_logit(eta)
      p[i] <- pmid
    } else {
    ## The package convention is eta = A.theta - B, as in the branch above;
    ## this branch previously wrote A*(theta - B), a different model.  It is
    ## currently unreachable because pars$A is always stored as a matrix.
    eta <- fit$itemPars$A[fit$dat$item[i]] *
      fit$personPars[fit$dat$id[i], 1+fit$dat$scale[i]] -
      fit$itemPars$B[fit$dat$item[i]]
    p[i] <- fit$itemPars$C[fit$dat$item[i]] +
      (fit$itemPars$D[fit$dat$item[i]] - fit$itemPars$C[fit$dat$item[i]]) * inv_logit(eta)
    }

    if(fit$dat$score[i]==0) p[i]= 1.0-p[i];
  }
  orig <- as.integer(fit$dat$originalRow)
  if(length(orig) == length(p) && !anyNA(orig)){
    out <- rep(NA_real_, max(orig))
    out[orig] <- p
    return(out)
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

    ## With eta = A * Ability - B, let Ability' = (Ability - nm) / nsd
    ## + normaliseMean.  The curve-preserving counterpart is A' = A * nsd
    ## and B' = B - A * nm + A' * normaliseMean.  The historical transform
    ## treated B as if it were on the latent scale independently of A, which
    ## changed response curves whenever discriminations differed from one.
    Ability <- (Ability - nm) / nsd + normaliseMean
    A <- A * nsd
    B <- B - A / nsd * nm + A * normaliseMean
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
      normbase = "Ability", normaliseScale = normaliseScale, normaliseMean = normaliseMean, robust = FALSE)
    A1 <- matrix(as.numeric(out$A), ncol = 1L)
    Ability1 <- matrix(as.numeric(out$Ability), ncol = 1L)
    B1 <- as.numeric(out$B)
    sign_val <- 1
    if(!is.null(referenceA) && identical(align, "orthogonal")){
      referenceA <- as.matrix(referenceA)
      if(!all(dim(referenceA) == dim(A1))) stop("referenceA must have the same dimensions as A.")
      align_score <- sum(A1 * referenceA, na.rm = TRUE)
      if(is.finite(align_score) && align_score < 0){
        sign_val <- -1
        A1 <- A1 * sign_val
        Ability1 <- Ability1 * sign_val
      }
    }
    dimnames(A1) <- A_dimnames
    dimnames(Ability1) <- ability_dimnames
    names(B1) <- B_names
    return(list(
      A = A1,
      B = B1,
      Ability = Ability1,
      center = mean(Ability[,1], na.rm = TRUE),
      chol_cov = matrix(stats::sd(Ability[,1], na.rm = TRUE) / normaliseScale, 1, 1),
      inv_chol = matrix(normaliseScale / pmax(stats::sd(Ability[,1], na.rm = TRUE), jitter), 1, 1),
      global_scale = 1,
      latent_shift = 0,
      AbilityCorr = matrix(1, 1, 1),
      sign = sign_val,
      rotation = matrix(sign_val, 1, 1)
    ))
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
  loading_norm <- sqrt(rowSums(A^2))
  loading_norm[loading_norm <= 0] <- NA_real_
  dt_Bcorr <- data.table::data.table(
    item = item_names,
    factor = NA_character_,
    parameter = "B_loading_corrected",
    value = as.numeric(state$B) / loading_norm
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

  out <- data.table::rbindlist(list(dt_A, dt_B, dt_Bcorr, dt_C, dt_D), fill = TRUE, use.names = TRUE)
  out[, `:=`(source = source, form = form)]
  data.table::setcolorder(out, c("source", "form", "parameter", "item", "factor", "value"))
  out[]
}

bigIRT_comparison_theta_sample <- function(state, n_points = 50L, jitter = 1e-8){
  Ability <- as.matrix(state$Ability)
  K <- ncol(Ability)
  mu <- colMeans(Ability, na.rm = TRUE)
  if(K <= 1L){
    sd1 <- stats::sd(Ability[, 1], na.rm = TRUE)
    if(!is.finite(sd1) || sd1 < jitter) sd1 <- 1
    return(matrix(stats::rnorm(as.integer(n_points), mean = mu[1], sd = sd1), ncol = 1L))
  }
  Sigma <- stats::cov(Ability, use = "pairwise.complete.obs")
  Sigma[!is.finite(Sigma)] <- 0
  Sigma <- (Sigma + t(Sigma)) / 2
  diag(Sigma) <- pmax(diag(Sigma), jitter)
  L <- chol(Sigma + diag(jitter, K))
  Z <- matrix(stats::rnorm(as.integer(n_points) * K), ncol = K)
  sweep(Z %*% L, 2, mu, "+")
}

bigIRT_comparison_item_curve_rmse <- function(state, reference_state, theta_sample){
  A <- as.matrix(state$A)
  A_ref <- as.matrix(reference_state$A)
  pred <- IRTcurve(A = A, B = state$B, C = state$C, D = state$D, theta = theta_sample, plot = FALSE)
  ref_pred <- IRTcurve(A = A_ref, B = reference_state$B, C = reference_state$C, D = reference_state$D, theta = theta_sample, plot = FALSE)
  pred <- as.matrix(pred)
  ref_pred <- as.matrix(ref_pred)
  sqrt(colMeans((pred - ref_pred)^2))
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
#' @param curveSampleN Integer. Number of multivariate-normal theta draws used
#'   to compute item-response-curve RMSE against the reference model for each
#'   item and form. Default is 50.
#'
#' @return A list with elements `itempars`, `personpars`, `loading_matrix`,
#'   `ability_corr_matrix`, `raw`, `normalized`, and `reference_model`.
#'   Each table is in long format with a `source` column, a source-specific
#'   `value` column, and a `referenceVal` column holding the aligned value from
#'   the reference model.
#' @export
compareIRTmodels <- function(models, score_method = "EAP", normaliseScale = 1,
  normaliseMean = 0, curveSampleN = 50L){
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

  raw_theta_sample <- bigIRT_comparison_theta_sample(raw_states[[ref_idx]], n_points = curveSampleN)
  normalized_theta_sample <- bigIRT_comparison_theta_sample(normalized_states[[ref_idx]], n_points = curveSampleN)
  raw_curve_rmse <- lapply(raw_states, function(st) bigIRT_comparison_item_curve_rmse(st, raw_states[[ref_idx]], raw_theta_sample))
  normalized_curve_rmse <- lapply(normalized_states, function(st) bigIRT_comparison_item_curve_rmse(st, normalized_states[[ref_idx]], normalized_theta_sample))
  names(raw_curve_rmse) <- names(raw_states)
  names(normalized_curve_rmse) <- names(normalized_states)

  itempars <- data.table::rbindlist(c(
    Map(bigIRT_comparison_itempars_dt, raw_states, names(raw_states), MoreArgs = list(form = "raw")),
    Map(bigIRT_comparison_itempars_dt, normalized_states, names(normalized_states), MoreArgs = list(form = "normalized"))
  ), use.names = TRUE, fill = TRUE)
  itempars <- bigIRT_comparison_attach_reference(
    itempars,
    c("form", "parameter", "item", "factor"),
    ref_name = names(models)[ref_idx]
  )
  curve_dt <- data.table::rbindlist(c(
    Map(function(rmse, source_name){
      data.table::data.table(
        source = source_name,
        form = "raw",
        item = item_names,
        itemCurveRMSE = as.numeric(rmse)
      )
    }, raw_curve_rmse, names(raw_curve_rmse)),
    Map(function(rmse, source_name){
      data.table::data.table(
        source = source_name,
        form = "normalized",
        item = item_names,
        itemCurveRMSE = as.numeric(rmse)
      )
    }, normalized_curve_rmse, names(normalized_curve_rmse))
  ), use.names = TRUE, fill = TRUE)
  itempars <- merge(itempars, curve_dt, by = c("source", "form", "item"), all.x = TRUE, sort = FALSE)
  data.table::setcolorder(itempars, c("source", "form", "parameter", "item", "factor", "value", "referenceVal", "itemCurveRMSE"))

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
          if('try-error' %in% class(tmp)) stop("Unable to evaluate the requested IRT curve.")
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

bigIRT_plot_laplace_diag_df <- function(diagdf, logGrad = TRUE, showTiming = TRUE){
  if(is.null(diagdf) || nrow(diagdf) == 0) return(invisible(NULL))

  x <- seq_len(nrow(diagdf))
  cols <- grDevices::colorRampPalette(c("#173f5f", "#20639b", "#3caea3", "#f6d55c", "#ed553b"))(max(2, nrow(diagdf)))
  grady <- if(logGrad) log1p(pmax(diagdf$itemGradNorm, 0)) else diagdf$itemGradNorm
  finite_range <- function(x, fallback = c(0, 1)){
    x <- x[is.finite(x)]
    if(!length(x)) return(fallback)
    rng <- range(x)
    if(!all(is.finite(rng)) || diff(rng) <= 0) {
      pad <- if(length(x) && is.finite(x[1])) max(1e-8, abs(x[1]) * 0.05) else 1e-8
      return(c(rng[1] - pad, rng[2] + pad))
    }
    rng
  }
  panels <- if(isTRUE(showTiming)) c(2, 3) else c(2, 2)
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))
  graphics::par(mfrow = panels, mar = c(4, 4, 2, 1))

  negobj <- -diagdf$objective
  if(any(is.finite(negobj))){
    objy <- log1p(negobj - min(negobj, na.rm = TRUE))
  } else {
    objy <- rep(NA_real_, nrow(diagdf))
  }
  graphics::plot(x, objy, type = "b", pch = 19, col = cols,
    xlab = "Outer iteration", ylab = "log(1 + objective - min(objective))", main = "Objective")
  if(nrow(diagdf) > 1) graphics::lines(stats::lowess(x, objy, f = 0.6), lwd = 2)

  graphics::plot(x, grady, type = "b", pch = 19, col = cols,
    xlab = "Outer iteration",
    ylab = if(logGrad) "log(1 + gradient norm)" else "Gradient norm",
    main = "Gradient")
  if(nrow(diagdf) > 1) graphics::lines(stats::lowess(x, grady, f = 0.6), lwd = 2)

  ylim_step <- finite_range(c(diagdf$itemStepRms, diagdf$personStepRms))
  graphics::plot(x, diagdf$itemStepRms, type = "b", pch = 19, col = "#20639b",
    xlab = "Outer iteration", ylab = "RMS movement", ylim = ylim_step,
    main = "Parameter Movement")
  graphics::lines(x, diagdf$personStepRms, type = "b", pch = 17, col = "#ed553b")
  graphics::legend("topright", legend = c("Item", "Person"),
    col = c("#20639b", "#ed553b"), pch = c(19, 17), bty = "n", cex = 0.85)

  ylim_sd <- finite_range(c(diagdf$meanPosteriorSD, diagdf$maxPosteriorSD))
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

bigIRT_refresh_plot_device <- function(){
  try(utils::flush.console(), silent = TRUE)
  try(grDevices::dev.flush(), silent = TRUE)
  invisible(NULL)
}

#' Plot Laplace diagnostics
#'
#' @param fit A fitted \code{bigIRT} model returned by \code{fitIRT()} with
#'   either Laplace backend and \code{laplaceDiagnostics=TRUE}. Direct-Laplace
#'   fits do not have blockwise stability panels; those panels are shown as
#'   unavailable rather than interpreted as failures.
#' @param logGrad Whether to plot the item-step gradient norm on a log10 scale.
#' @param showTiming Whether to include timing and stability panels.
#'
#' @return Invisibly returns the diagnostic data frame used for plotting.
#' @export
plotLaplaceDiagnostics <- function(fit, logGrad = TRUE, showTiming = TRUE){
  if(is.null(fit$laplaceDiagnostics) || nrow(fit$laplaceDiagnostics) == 0){
    stop("No Laplace diagnostics found. Refit with laplaceDiagnostics = TRUE.")
  }
  if(identical(fit$backend, "laplace")){
    message("Direct-Laplace diagnostics do not include blockwise stability panels; unavailable values are shown as NA.")
  }
  bigIRT_plot_laplace_diag_df(fit$laplaceDiagnostics, logGrad = logGrad, showTiming = showTiming)
}

`%||%` <- function(x, y) if(is.null(x)) y else x

#' @export
print.bigIRT_fit <- function(x, ...){
  backend <- x$backend %||% x$call$marginalApprox %||% "none"
  dat <- x$dat
  cat(sprintf("bigIRT fit: %s backend | %s persons, %s items, %s dimensions\n",
    backend, dat$Nsubs %||% NA_integer_, dat$Nitems %||% NA_integer_, dat$Nscales %||% NA_integer_))
  if(!is.null(x$optim$logLik)) cat(sprintf("Objective: %.6f\n", x$optim$logLik))
  status <- x$laplaceStatus
  if(!is.null(status)){
    cat(sprintf("Laplace: %s after %s iteration(s) [%s]\n",
      if(isTRUE(status$converged)) "strictly converged" else if(isTRUE(status$stable_plateau)) "stable plateau" else "not strictly converged",
      status$outer_iters %||% NA_integer_, status$reason %||% "unknown"))
    if(isTRUE(status$approximate_gradient)) cat("Note: direct Laplace uses approximate global derivatives.\n")
    if(isTRUE(status$beta_frozen)) cat("Note: person-predictor effects were held fixed.\n")
  }
  invisible(x)
}

#' @export
summary.bigIRT_fit <- function(object, ...){
  ability <- as.matrix(object$pars$Ability)
  items <- as.data.frame(object$itemPars)
  out <- list(
    call = object$call, backend = object$backend, status = object$laplaceStatus,
    item_summary = if(nrow(items)) summary(items[, intersect(c("A", "B", "C", "D"), names(items)), drop = FALSE]) else NULL,
    ability_summary = if(length(ability)) summary(ability) else NULL
  )
  class(out) <- "summary.bigIRT_fit"
  out
}

#' @export
print.summary.bigIRT_fit <- function(x, ...){
  cat(sprintf("bigIRT summary (%s backend)\n", x$backend %||% "none"))
  if(!is.null(x$status)) cat(sprintf("Termination: %s\n", x$status$reason %||% "unknown"))
  if(!is.null(x$item_summary)){ cat("Item parameters:\n"); print(x$item_summary) }
  if(!is.null(x$ability_summary)){ cat("Abilities:\n"); print(x$ability_summary) }
  invisible(x)
}

bigIRT_validate_fit_inputs <- function(dat, score, id, item, scale, pl, controls, trainingRows,
  predictors = character()){
  if(!is.data.frame(dat)) stop("`dat` must be a data frame or data.table.")
  if(!nrow(dat)) stop("`dat` must contain at least one row.")
  required <- unique(c(score, id, item, scale, predictors))
  if(!all(nzchar(required)) || any(!required %in% names(dat)))
    stop("`dat` is missing one or more requested score, id, item, scale, or predictor columns.")
  y <- dat[[score]]
  if(!is.numeric(y) || any(!is.finite(y)) || any(!(y %in% c(0, 1))))
    stop("`score` must be finite numeric binary data (0 or 1).")
  if(length(pl) != 1L || !is.finite(pl) || pl != as.integer(pl) || !(pl %in% 1:4))
    stop("`pl` must be one integer from 1 to 4.")
  bad <- !vapply(controls, function(z) length(z) == 1L && is.finite(z) && z > 0, logical(1))
  if(any(bad)) stop(sprintf("Controls must be finite positive scalars: %s.", paste(names(controls)[bad], collapse = ", ")))
  if(!is.numeric(trainingRows) || !length(trainingRows) || any(!is.finite(trainingRows)) ||
     any(trainingRows != as.integer(trainingRows)) || anyDuplicated(trainingRows) ||
     any(trainingRows < 1L | trainingRows > nrow(dat)))
    stop("`trainingRows` must be a nonempty, unique integer selection in the original input row order.")
  invisible(as.integer(trainingRows))
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
#' @param BSD Numeric. Standard deviation for the prior distribution of the
#'   item difficulty parameters. Default is 2.5, on the scale of
#'   \code{AbilitySD}. The previous default of 10 was effectively flat: on a
#'   standardised ability scale it puts about 69 per cent of its mass beyond
#'   |B| > 4, where an item is answered by almost everybody or almost nobody.
#'   Difficulty is well identified in a 2PL and barely notices, but under a 3PL
#'   or 4PL it absorbs the error in a weakly identified asymptote and runs away
#'   -- measured difficulty RMSE above 3 on sparse 4PL cells, against 0.4 with
#'   this default. Widen it for vertical scales, whose difficulties genuinely
#'   span more than this.
#' @param logitCMeandat Numeric. Mean for the prior distribution of the item guessing parameters (on logit scale). Default is 0.
#' @param logitCSD Numeric. Standard deviation for the prior distribution of
#'   the item guessing parameters (on logit scale). Default is 1. The previous
#'   default of 5 carried a prior precision of 0.04, which is negligible
#'   against any data, so per-item guessing was effectively unconstrained; 1
#'   gives precision comparable to what a handful of responses provide. Note
#'   that guessing is generally not recoverable per item at realistic test
#'   lengths -- fitted values correlate with the truth at around .07 on 20
#'   responses an item -- so this prior mostly decides how firmly items pool
#'   towards a common asymptote.
#' @param logitDMeandat Numeric. Mean for the prior distribution of the item upper asymptote parameters (on logit scale). Default is 0.
#' @param logitDSD Numeric. Standard deviation for the prior distribution of
#'   the item upper asymptote parameters (on logit scale). Default is 1, for
#'   the reasons given under \code{logitCSD}; the upper asymptote is usually
#'   pinned by even fewer responses than the lower one.
#' @param AbilityMeandat Numeric array. Mean for the prior distribution of the ability parameters. Default is 0 for each scale.
#' @param AbilitySD Numeric array. Standard deviation for the prior distribution of the ability parameters. Default is 1 for each scale.
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
#' @param marginalApprox Character. Marginal approximation backend. Use
#'   \code{"none"} for the legacy Stan/JML path, or \code{"laplace"} for the
#'   Laplace backend: a single-stage optimizer over the item block that
#'   re-solves the person modes inside every objective evaluation and
#'   differentiates through them, so the gradient carries the adjoint term
#'   rather than treating the modes as fixed. It accepts item predictors,
#'   person predictors, estimated ability means and estimated latent
#'   correlations, and its gradient agrees with central finite differences in
#'   every one of those blocks. Predictor columns are response-row aligned and
#'   can vary within a person or item; the likelihood retains their exact row
#'   values while reported person/item summaries use within-entity predictor
#'   means. \code{"laplace_fast"} and \code{"laplace_direct"} are accepted as
#'   names for this same backend: they were once two implementations, an
#'   alternating one that froze the modes during an item step and this one, and
#'   the alternating one was withdrawn after it proved slower for the same
#'   answer. Default is \code{"none"}.
#' @param ebayesCoarse Numeric. Tolerance multiplier for the intermediate
#'   empirical-Bayes refits on the Laplace backend. Those rounds exist only to
#'   produce estimates to update the hyperparameters from, so they are run at
#'   \code{laplaceTol * ebayesCoarse}; the final fit always uses
#'   \code{laplaceTol}. Default is 100.
#' @param ebayesMinSD Numeric. Floor on a prior SD estimated by
#'   \code{ebayesMethod = "moment"}. That rule subtracts a sampling-noise term
#'   and reaches zero whenever the data cannot support item-level variation, so
#'   the floor decides how hard those blocks pool. It does not apply to
#'   \code{"laplace"}, which is bounded by its own objective. Default is 0.05.
#' @param ebayesMethod Character. How the Laplace backend estimates prior
#'   hyperparameters when \code{ebayes = TRUE}. \code{"laplace"} (default)
#'   approximates the integral over the item parameters and maximises the
#'   resulting profile marginal, which is bounded and needs no floor or
#'   multiplier. \code{"moment"} keeps the earlier variance-components rule,
#'   which is faster but relies on a floor. Ignored on the JML path.
#' @param ebayesIter Integer. Maximum empirical-Bayes rounds on the Laplace
#'   backend. Each round refits, updates the prior hyperparameters from the fit
#'   by the EM M-step for a normal-normal hierarchy, and stops early once every
#'   hyperparameter moves by less than a thousandth. Ignored when
#'   \code{ebayes = FALSE} or on the JML path, which runs its own single
#'   empirical-Bayes step. Default is 5.
#' @param estimateAbilityCorr Logical. If \code{TRUE}, estimate the latent
#'   ability correlation matrix while keeping \code{AbilitySD} fixed, so that
#'   only the correlation is free (the latent scale is not identified
#'   separately from the loadings). Supported by \code{marginalApprox =
#'   "laplace"}, which optimises
#'   it as part of the parameter vector. Ignored for other backends and for
#'   unidimensional fits. It forces \code{laplaceKeepCovariance}, because the
#'   person posterior covariances are required: modes are shrunk towards zero
#'   by the prior, so their cross-product alone understates the correlation.
#'   The result is returned in \code{fit$abilityPrior$corr}, with
#'   \code{fit$abilityPrior$estimated} recording whether it was estimated or
#'   held fixed. This prior-level correlation is the quantity to report: the
#'   correlation among fitted ability point estimates is biased, downwards
#'   under an independent prior and upwards under a correlated one.
#'   Default is FALSE.
#' @param laplaceOuterIter Integer. Maximum number of outer iterations for a
#'   Laplace backend. This is a fallback limit rather than
#'   the primary convergence criterion. Default is 500.
#' @param laplaceTol Numeric. General outer tolerance for
#'   \code{marginalApprox="laplace"}, used for relative objective
#'   improvement and RMS step-size checks. Default is 1e-3.
#' @param laplaceTolScale Character. What the objective-change tolerances in
#'   \code{laplaceTol} are measured against on the Laplace path.
#'   \code{\"relative\"} (the default, and the historical behaviour) divides
#'   the change by the size of the objective. Because the objective grows
#'   with the number of responses, the same nominal tolerance becomes laxer
#'   as data grow: on several million responses a relative change of 1e-3 is
#'   an absolute change in the thousands, and a fit can stop while it is
#'   still moving. \code{\"per_obs\"} divides by the number of responses
#'   instead, so the quantity thresholded is the change in mean
#'   log-likelihood per response and does not depend on data size. Prefer it
#'   when comparing fits across designs of very different sizes, but note
#'   that it is far stricter at large N: a tolerance chosen for
#'   \code{\"relative\"} should not be carried over unchanged. The scale used
#'   is recorded in \code{laplaceStatus$tolerance_scale}.
#' @param laplaceLogdetScale Numeric. Weight on the Laplace log-determinant,
#'   applied to the objective and to both of its gradient contributions, so
#'   value and gradient always describe the same function. 1 is the Laplace
#'   objective; 0 drops the log-determinant and leaves the joint posterior;
#'   values between damp the Occam penalty that this term places on
#'   discrimination. Use this when shrinkage is wanted, in preference to
#'   detuning \code{laplaceAdjointScale}: damping the gradient alone leaves the
#'   optimiser minimising something it is not evaluating, and makes the
#'   convergence diagnostics report on a function that does not exist.
#' @param laplaceAdjointScale Numeric. Weight on the mode-adjoint correction in
#'   the item gradient. The item step evaluates its objective at frozen person
#'   modes but its gradient carries a correction for how those modes move with
#'   the item parameters, so the two do not describe quite the same function.
#'   Against finite differences the correction is roughly an order of magnitude
#'   too small. Raising this weight improves the Laplace objective monotonically
#'   and degrades recovery of the generating parameters monotonically, so the
#'   default of 1 is kept: it is where recovery is best, not where the gradient
#'   is right. Setting 0 removes the correction, which is worse on both counts.
#'   Exposed for diagnosis; see the package notes before changing it.
#' @param keepInternals Logical. Attach the prepared data (\code{sdat}), the
#'   final parameter state and the prior precision array to the returned fit.
#'   Off by default because these carry copies of the response arrays. Useful
#'   for verifying analytic gradients against finite differences, and for
#'   inspecting a fit that stopped somewhere unexpected.
#' @param laplaceGradTol Numeric. Tolerance for the item-block gradient,
#'   measured relative to the objective rather than as a raw norm: the reported
#'   norm is divided by the same denominator \code{laplaceTolScale} selects for
#'   the objective. An absolute norm grows with both the number of item
#'   parameters and the number of responses, so an absolute threshold cannot be
#'   met by a large sparse fit however well converged it is. Both the raw and
#'   the scaled value are returned in \code{laplaceStatus}. Formerly a
#'   gradient-norm tolerance for the item-step
#'   Laplace surrogate. Default is 1e-2.
#' @param laplacePersonTol Numeric. Newton tolerance for person-mode updates in
#'   \code{"laplace"}. Default is 1e-4.
#' @param laplaceKeepCovariance Logical. Whether to keep full person covariance
#'   matrices on the Laplace path. Covariances are otherwise retained only when
#'   needed to estimate ability correlations. Default is FALSE.
#' @param laplaceDiagnostics Logical. Whether to store outer-loop diagnostics for
#'   \code{"laplace"}. Default is FALSE.
#' @param laplacePlot Logical. Whether to draw the Laplace diagnostic plot during
#'   fitting when \code{marginalApprox="laplace"}. Default is FALSE.
#' @param laplacePlotEvery Integer. Plot every N outer iterations when
#'   \code{laplacePlot=TRUE}. Default is 1.
#' @param laplaceJitter Numeric. Small jitter added to stabilize Laplace
#'   covariance calculations. Default is 1e-6.
#' @param noptimsteps Integer. Number of optimizer iterations used inside each
#'   Laplace item step. Default is 10.
#' @param normalise Logical. Whether to normalize the output estimates. Default is FALSE.
#' @param normaliseScale Numeric. Scale for normalization. Default is 1.
#' @param normaliseMean Numeric. Mean for normalization. Default is 0.
#' @param dropPerfectScores Logical. Whether to drop perfect scores from each subject and item before estimation. Default is TRUE.
#' @param trainingRows Integer vector. Rows of data to use for estimation of parameters. Default is all rows in \code{dat}.
#' @param init Initial values for the fitting algorithm. Default is NA.
#' @param tol Numeric. Tolerance for convergence. Default attempts to sensibly adjust for amount of data.
#' @param ... Additional arguments passed to the fitting function.
#'
#' @return An object of class \code{bigIRT_fit}. All fits retain the legacy
#'   fields. Laplace fits additionally provide \code{laplaceStatus}: strict
#'   convergence, stable plateau, iteration-limit, person-mode-failure,
#'   numerical-failure, frozen-effect, approximate-gradient, and covariance
#'   retention flags; and, when requested, \code{laplaceDiagnostics}. Its
#'   rows describe outer iteration, objective, relative improvement, item and
#'   person RMS steps, item gradient, posterior SD, person convergence, timings,
#'   optimizer work, and strict/stability criteria. Direct-Laplace-specific
#'   stability fields are unavailable (\code{NA}).
#'
#'   \strong{Row ordering.} \code{fitIRT} sorts the data by person before
#'   fitting, so most row-level objects are in that internal order. The two
#'   per-response predictions users normally want, \code{fit$pars$p} (the
#'   probability of the observed response) and \code{fit$pars$pcorrect} (the
#'   probability of a correct response), are mapped back to the order of the
#'   data passed in, so they can be indexed with the same row numbers used
#'   for \code{trainingRows}; rows removed by \code{dropPerfectScores} are
#'   \code{NA}. Every other row-level object -- \code{b_row}, \code{c_row},
#'   \code{d_row}, \code{eta_row}, \code{row_loadings}, \code{row_ability},
#'   and the contents of \code{fit$dat} -- remains in internal order, because
#'   the person-covariance routines pair those with \code{fit$dat$id}. Use
#'   \code{fit$pars$originalRow}, which gives the input row index of each
#'   internal row, to move between the two.
#' @export
#'
#' @examples
#' #Generate some data (here 2pl model
#' require(data.table)
#' dat <- simIRT(Nsubs = 50,Nitems = 100,Nscales = 2,
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
  invspAMeandat=.542,invspASD=1,BMeandat=0,BSD=2.5, logitCMeandat=0,logitCSD=1,
  logitDMeandat=0,logitDSD=1,
  AbilityMeandat=array(0,dim=c(length(unique(dat[[scale]])))),
  AbilitySD=array(1,dim=c(length(unique(dat[[scale]])))),
  AbilityCorr=diag(1,c(length(unique(dat[[scale]])))),
  AMeanSD=1,BMeanSD=BSD,logitCMeanSD=logitCSD,logitDMeanSD=logitDSD,
  AbilityMeanSD=array(1,dim=c(length(unique(dat[[scale]])))),
  iter=2000,cores=6,carefulfit=FALSE,
  ebayes=TRUE,ebayesmultiplier=2,ebayesFromFixed=FALSE,ebayesIter=5L,ebayesMethod=c("laplace","moment"),ebayesMinSD=0.05,ebayesCoarse=100,
  estMeans=c('A','B','C','D'),priors=TRUE,
  marginalApprox=c("none","laplace","laplace_fast","laplace_direct"),
  estimateAbilityCorr=FALSE,
  laplaceCorrParam=c("stan_corsqrt","normalized_chol"),
  keepInternals=FALSE,laplaceAdjointScale=1,laplaceLogdetScale=1,laplaceOuterIter=500,laplaceTol=1e-3,laplaceTolScale=c("relative","per_obs"),laplaceGradTol=1e-4,laplacePersonTol=1e-4,
  laplaceKeepCovariance=FALSE,laplaceDiagnostics=FALSE,laplacePlot=FALSE,laplacePlotEvery=1L,
  laplaceJitter=1e-6,noptimsteps=10,
  normalise=FALSE,normaliseScale=1,normaliseMean=0,
  dropPerfectScores=TRUE,trainingRows=1:nrow(dat),
  init=NA,tol=1e-8 * 10^(log(nrow(dat), 10)),...){

  supplied <- names(match.call(expand.dots = FALSE))
  trainingRows <- bigIRT_validate_fit_inputs(
    dat, score, id, item, scale, pl,
    controls = list(iter = iter, cores = cores, laplaceOuterIter = laplaceOuterIter,
      laplaceTol = laplaceTol, laplaceGradTol = laplaceGradTol,
      laplacePersonTol = laplacePersonTol,
      laplacePlotEvery = laplacePlotEvery,
      laplaceJitter = laplaceJitter, noptimsteps = noptimsteps, tol = tol),
    trainingRows = trainingRows,
    predictors = unique(c(personPreds, AitemPreds, BitemPreds, CitemPreds, DitemPreds)))
  if("estimateAbilityCorr" %in% supplied && isTRUE(estimateAbilityCorr) &&
      !marginalApprox[1] %in% c("laplace", "laplace_direct", "laplace_fast"))
    warning("estimateAbilityCorr is ignored unless marginalApprox is 'laplace'.",
      call. = FALSE)
  ## Estimating the latent correlation needs the person posterior covariances:
  ## the M-step is an average second moment, and without the covariance term it
  ## would use shrunken modes alone and understate the correlation.  Force
  ## retention here so that it applies to whichever backend runs.
  if(isTRUE(estimateAbilityCorr) && length(unique(dat[[scale]])) > 1L) laplaceKeepCovariance <- TRUE
  ## Anything in ... that no downstream consumer accepts is silently discarded.
  ## That turned a stale install into 1,500 fits which ignored the argument they
  ## were varying and looked entirely healthy, so unknown names are reported
  ## rather than dropped. ... is still forwarded to optimIRT, so its formals are
  ## legitimate and only names matching neither function are flagged.
  local({
    dn <- names(list(...))
    dn <- dn[nzchar(dn)]
    if(length(dn)){
      optimFormals <- names(formals(optimIRT))
      ## A downstream function taking ... could accept anything, so there is
      ## nothing to report. The real optimIRT names every argument and has no
      ## dots, so the check still does its job; this only stands down when the
      ## binding is a shim, as under local_mocked_bindings in the tests, where
      ## the mock's sole ... argument would otherwise empty the allowlist and
      ## have us blame the user's install for arguments that are perfectly fine.
      if(!("..." %in% optimFormals)){
        known <- unique(c(names(formals(fitIRT)), optimFormals))
        unknown <- setdiff(dn, known)
        if(length(unknown)){
          warning("fitIRT ignored unrecognised argument(s): ",
            paste(unknown, collapse = ", "),
            ". Arguments in `...` are forwarded to optimIRT and anything it does ",
            "not accept has no effect. Check the spelling, and check that the ",
            "installed bigIRT is recent enough to have the argument.",
            call. = FALSE)
        }
      }
    }
  })

  old_adjoint <- getOption("bigIRT.adjoint_scale", 1)
  old_logdet <- getOption("bigIRT.logdet_scale", 1)
  options(bigIRT.adjoint_scale = laplaceAdjointScale,
          bigIRT.logdet_scale = laplaceLogdetScale)
  on.exit(options(bigIRT.adjoint_scale = old_adjoint,
                  bigIRT.logdet_scale = old_logdet), add = TRUE)
  sdat <-list() #initialize standata object
  basetol=tol
  marginalApprox <- match.arg(marginalApprox)
  ## One Laplace backend. "laplace_fast" and "laplace_direct" both name it now.
  ## The two used to be separate: laplace_fast alternated between a person step
  ## and an item step with the modes frozen, laplace_direct optimised the item
  ## block directly and re-solved the modes inside every objective evaluation.
  ## Head to head the alternating scheme needed about 85 item evaluations where
  ## the direct one needed 32, because it restarted its inner optimiser each
  ## outer iteration and threw away the curvature, and it spent those extra
  ## evaluations against a frozen posterior. Its one apparent advantage --
  ## better parameter recovery on weakly identified 3PL and sparse designs --
  ## turned out to be early stopping rather than a better estimator: driving it
  ## to converge harder moved its likelihood up to the direct one's and its
  ## recovery down to match, monotonically and across seeds. Regularisation
  ## belongs in the priors, not in where an optimiser happens to stall.
  if(marginalApprox %in% c("laplace_fast", "laplace_direct")) marginalApprox <- "laplace"
  ebayesMethod <- match.arg(ebayesMethod)
  laplaceCorrParam <- match.arg(laplaceCorrParam)

  itemPreds <- unique(c(AitemPreds,BitemPreds,CitemPreds,DitemPreds))

  #setup unlikely names to use in data.table calls to avoid overlap from user defined names
  idref. <- id; scaleref. <- scale; itemref. <- item;
  scoreref. <- score;
  personPredsref. <- personPreds;
  itemPredsref. <- itemPreds

  if(!'data.table' %in% class(dat)){  #drop unused columns from dat and set to data.table (copy if already data table)
    dat <- data.table::as.data.table(dat)[, c((idref.), (scoreref.), (itemref.), (scaleref.),
      itemPredsref., personPredsref.), with = FALSE]
  } else {
    dat <- data.table::copy(dat[,c((idref.),(scoreref.),(itemref.),(scaleref.),
      itemPredsref.,personPredsref.),with=FALSE])
  }
  dat[, `__bigIRT_input_row__` := seq_len(.N)]
  dat[, `__bigIRT_training__` := as.integer(get("__bigIRT_input_row__") %in% trainingRows)]


  #drop problem people and items
  if(dropPerfectScores)    dat <- dropPerfectScores(dat,scoreref. = scoreref.,itemref. = itemref.,idref. = idref.)
  if(!nrow(dat)) stop("No observations remain after filtering perfect-score rows.")

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

  trainingLogical <- array(as.integer(dat[["__bigIRT_training__"]]))
  if(!any(trainingLogical)) stop("No selected `trainingRows` remain after filtering.")
  trainDat <- dat[trainingLogical == 1L]
  if(length(unique(trainDat[[idref.]])) < Nsubs || length(unique(trainDat[[itemref.]])) < Nitems)
    stop("`trainingRows` must include every person and item retained for estimation.")

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
    AMeanSD=AMeanSD,
    BMeanSD=BMeanSD,
    logitCMeanSD=logitCMeanSD,
    logitDMeanSD=logitDMeanSD,
    AbilityMeanSD=array(AbilityMeanSD),
    fixedAMean=as.integer(!'A' %in% estMeans || pl < 2),
    fixedBMean=as.integer(!'B' %in% estMeans),
    fixedCMean=as.integer(!'C' %in% estMeans || pl < 3),
    fixedDMean=as.integer(!'D' %in% estMeans || pl < 4),
    fixedAbilityMean=as.integer(!'Ability' %in% estMeans & !'ability' %in% estMeans),
    rowIndexPar=0L,
    originalRow=array(as.integer(dat[["__bigIRT_input_row__"]])),
    doGenQuant=0L,
    doRowEff=as.integer(identical(marginalApprox, "laplace"))
  ))

  sdat$freeAref=array(as.integer(cumsum(1-as.numeric(sdat$fixedAlog))))
  sdat$freeBref=array(as.integer(cumsum(1-as.numeric(sdat$fixedB))))
  sdat$freeCref=array(as.integer(cumsum(1-as.numeric(sdat$fixedClogit))))
  sdat$freeDref=array(as.integer(cumsum(1-as.numeric(sdat$fixedDlogit))))

  ## fitIRT sorts the data by person before fitting, so every row-aligned
  ## internal quantity is in that sorted order.  `p` and `pcorrect` are the
  ## per-response predictions users index with their own row numbers (see the
  ## trainingRows argument), so they are mapped back to the order of the data
  ## that was passed in.  The remaining row-level objects (b_row, row_loadings,
  ## row_ability, ...) stay in internal order because the person-covariance
  ## code pairs them with sdat$id.
  restore_input_row_order <- function(fit){
    if(isTRUE(attr(fit, "bigIRT_rows_restored"))) return(fit)
    orig <- as.integer(sdat$originalRow)
    if(!length(orig) || anyNA(orig)) return(fit)
    n_in <- max(orig)
    for(nm in c("p", "pcorrect")){
      v <- fit$pars[[nm]]
      if(is.null(v) || length(v) != length(orig)) next
      out <- rep(NA_real_, n_in)
      out[orig] <- as.numeric(v)
      fit$pars[[nm]] <- out
    }
    fit$pars$originalRow <- orig
    attr(fit, "bigIRT_rows_restored") <- TRUE
    fit
  }

  apply_fit_dimnames <- function(fit){
    fit <- restore_input_row_order(fit)
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
  if(!identical(marginalApprox, "laplace")){
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

  if(identical(marginalApprox, "laplace") && length(which(sdat$Abilityparsindex > 0)) > 0){
    ## Item predictors are fine here; person predictors are not. The guard used
    ## to refuse both, but the item beta gradients agree with finite differences
    ## to about 2e-9, so refusing them only pushed item-covariate models onto the
    ## slower path for no reason. The ability_beta gradient really is wrong --
    ## against finite differences its ratio wanders between .04 and 3.1 rather
    ## than sitting at any constant -- so person predictors stay blocked until
    ## that derivative is derived properly.
    optimdots <- list(...)
    laplaceVerbose <- if("verbose" %in% names(optimdots)) as.integer(optimdots$verbose) else 0L
    collectDirectDiag <- isTRUE(laplaceDiagnostics) || isTRUE(laplacePlot)
    if(requireNamespace("RcppParallel", quietly = TRUE)) RcppParallel::setThreadOptions(numThreads = max(1L, as.integer(cores)))
    laplace_trace <- function(level, ...){
      if(laplaceVerbose >= level) message(...)
    }
    direct_plot_callback <- if(isTRUE(laplacePlot)) {
      function(history){
        diagdf <- data.table::rbindlist(history, fill = TRUE)
        bigIRT_plot_laplace_diag_df(diagdf)
        bigIRT_refresh_plot_device()
      }
    } else NULL
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

    state <- bigIRT_laplace_initial_state(sdat, eps = laplaceJitter, corr_paramization = laplaceCorrParam)
    priorInfo <- bigIRT_laplace_prior_mats(sdat, jitter = laplaceJitter)
    priorPrecision <- priorInfo$precision_array
    laplace_trace(1, sprintf(
      "Direct Laplace: starting prior-anchored fit with %d persons, %d items, %d dimensions, max_iter=%d.",
      sdat$Nsubs, sdat$Nitems, sdat$Nscales, laplaceOuterIter
    ))
    t_direct <- wall_time_sec()
    ## Empirical Bayes: refit, update the prior hyperparameters from the fit,
    ## repeat until they stop moving. ebayes was previously accepted and then
    ## ignored on this backend -- it appears only in JMLfit and the
    ## ebayesFromFixed block -- so a fit asking for it silently got fixed priors.
    eb_rounds <- if(isTRUE(ebayes)) max(1L, as.integer(ebayesIter)) else 0L
    eb_trace <- list()
    eb_prev_delta <- setNames(rep(NA_real_, 4L), c("B", "C", "D", "A"))
    eb_prev_sdat <- sdat
    eb_frozen <- character(0)
    for(eb_round in seq_len(eb_rounds + 1L)){
    directFit <- bigIRT_laplace_optimize_direct(
      state = state,
      sdat = sdat,
      prior_precision = priorPrecision,
      ## Intermediate empirical-Bayes rounds only need a fit good enough to
      ## estimate the hyperparameters from, so they run at a coarse tolerance;
      ## the last pass through the loop updates nothing and is the one whose
      ## estimates are returned, so it runs at the requested tolerance. Fitting
      ## is 96 per cent of an ebayes run -- the hyperparameter step itself is
      ## under 1 -- so this is where the time is.
      niter = max(2L, as.integer(laplaceOuterIter)),
      tol = if(eb_rounds > 0L && eb_round <= eb_rounds) laplaceTol * ebayesCoarse else laplaceTol,
      jitter = laplaceJitter,
      person_tol = laplacePersonTol,
      ## The ability-mean, ability-beta and correlation gradients all carry an
      ## adjoint term built from H_i^-1, so each of them needs the posterior
      ## covariances retained whether or not the caller asked for them.
      keep_covariance = isTRUE(laplaceKeepCovariance) || isTRUE(estimateAbilityCorr) ||
        sdat$NpersonPreds > 0L || sdat$fixedAbilityMean == 0L ||
        (isTRUE(ebayes) && identical(ebayesMethod, "laplace")),
      cores = cores,
      estimateAbilityCorr = estimateAbilityCorr,
      corr_paramization = laplaceCorrParam,
      collect_history = collectDirectDiag,
      plot_callback = direct_plot_callback,
      plot_every = laplacePlotEvery,
      verbose = laplaceVerbose,
      trace_fn = function(msg) laplace_trace(2, msg),
      stochastic = isTRUE(optimdots$stochastic)
    )

      if(eb_round > eb_rounds) break
      eb_rows <- bigIRT_laplace_row_context(sdat, rows = seq_len(sdat$Nobs))
      eb_ctx <- bigIRT_laplace_item_context(sdat, row_context = eb_rows)
      eb_state <- directFit$state
      eb_theta <- directFit$eval$posterior$theta_mode
      eb_eff <- bigIRT_laplace_row_effective(state = eb_state, sdat = sdat,
        thetaBase = eb_theta, rows = eb_rows$rows, context = eb_rows)
      eb_before <- c(sdat$BSDx[1], sdat$logitCSD[1], sdat$logitDSD[1], sdat$invspASD[1])
      eb_prev_sdat <- sdat
      eb_upd <- if(identical(ebayesMethod, "laplace")){
        ## Laplace over the item block: a real objective in the hyperparameters
        ## rather than a moment rule, so it neither floors nor runs away.
        bigIRT_laplace_hyper_update(state = eb_state, sdat = sdat, context = eb_ctx,
          row_effective = eb_eff, row_context = eb_rows,
          posterior = directFit$eval$posterior, thetaBase = eb_theta,
          fixed = eb_frozen)
      } else {
        bigIRT_laplace_eb_update(state = eb_state, sdat = sdat, context = eb_ctx,
          row_effective = eb_eff, row_context = eb_rows, thetaBase = eb_theta,
          multiplier = ebayesmultiplier, min_sd = ebayesMinSD)
      }
      sdat <- eb_upd$sdat
      eb_after <- c(sdat$BSDx[1], sdat$logitCSD[1], sdat$logitDSD[1], sdat$invspASD[1])
      eb_trace[[eb_round]] <- list(round = eb_round, before = eb_before, after = eb_after,
        iters = as.integer(directFit$optim$iter %||% NA_integer_))
      laplace_trace(1, sprintf("Empirical Bayes round %d: B %.3f->%.3f  logitC %.3f->%.3f  logitD %.3f->%.3f  invspA %.3f->%.3f",
        eb_round, eb_before[1], eb_after[1], eb_before[2], eb_after[2],
        eb_before[3], eb_after[3], eb_before[4], eb_after[4]))
      ## Priors changed, so the person prior has to be rebuilt with them.
      priorInfo <- bigIRT_laplace_prior_mats(sdat, jitter = laplaceJitter)
      priorPrecision <- priorInfo$precision_array
      state <- eb_state
      ## Each round maximises a proper objective, but against the fit from the
      ## round before, so this is a fixed-point iteration and individual blocks
      ## can stop contracting. A 4PL upper asymptote pinned by few responses
      ## does exactly that: its SD descends, turns, and climbs away while
      ## difficulty and discrimination are still converging nicely. Freeze only
      ## the block that reverses and let the others carry on.
      ## Reversal, not rate, is the signal. A converging block can take a bigger
      ## step than the one before -- difficulty ran 10 -> 1.93 -> 1.25 -> 0.78,
      ## converging the whole way -- so freezing on a grown step throws away
      ## good rounds. A block that has been shrinking and turns around is the
      ## one that is not going to settle.
      delta <- eb_after - eb_before
      rel_vec <- abs(delta) / pmax(abs(eb_before), 1e-8)
      names(rel_vec) <- names(delta) <- c("B", "C", "D", "A")
      reversed <- is.finite(eb_prev_delta) & sign(delta) != sign(eb_prev_delta) &
        rel_vec > 1e-3
      grew <- names(rel_vec)[reversed]
      if(length(grew)){
        laplace_trace(1, sprintf("Empirical Bayes: freezing %s at round %d (update reversed direction)",
          paste(grew, collapse = ", "), eb_round))
        for(nm in grew){
          fld <- switch(nm, B = "BSDx", C = "logitCSD", D = "logitDSD", A = "invspASD")
          sdat[[fld]] <- eb_prev_sdat[[fld]]
        }
        eb_frozen <- union(eb_frozen, grew)
        priorInfo <- bigIRT_laplace_prior_mats(sdat, jitter = laplaceJitter)
        priorPrecision <- priorInfo$precision_array
      }
      eb_prev_delta <- delta
      if(max(rel_vec[setdiff(names(rel_vec), eb_frozen)], 0) < 1e-3) break
      if(length(eb_frozen) >= length(rel_vec)) break
    }
    directSec <- wall_time_sec() - t_direct
    state <- directFit$state
    sdat$AbilityCorr <- if(!is.null(state$AbilityCorr)) state$AbilityCorr else sdat$AbilityCorr
    finalPosterior <- directFit$eval$posterior
    state$AbilityBase <- finalPosterior$theta_mode

    fit <- list(pars = list(), optim = list(), dat = sdat)
    fit$pars <- bigIRT_laplace_constrained_pars(state, sdat, posterior = finalPosterior)
    fit$optim <- list(
      method = "laplace",
      logLik = directFit$eval$value,
      par = directFit$optim$par,
      target_evals = directFit$optim$target_evals,
      masked_grad_norm = directFit$optim$masked_grad_norm,
      iter = directFit$optim$iter,
      terminate = directFit$optim$terminate
    )
    fit$dat <- sdat
    fit <- apply_fit_dimnames(fit)
    if(isTRUE(keepInternals)){
      ## Same payload the alternating backend used to expose, so callers that
      ## reach in for sdat/state to drive the backend directly keep working.
      fit$internals <- list(sdat = sdat, state = state,
        priorPrecision = priorPrecision, thetaBase = state$AbilityBase)
    }
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
    directDiag <- if(length(directFit$history)) data.table::rbindlist(directFit$history, fill = TRUE) else NULL
    ## Same rescaling as the blockwise path: the tolerance applies to a
    ## gradient measured against the objective, not to a raw norm whose size
    ## tracks the number of items and responses.
    ## laplaceTolScale is only match.arg'd further down, inside the blockwise
    ## branch; the direct path runs first and would otherwise switch on the
    ## whole default vector.
    directTolScale <- match.arg(laplaceTolScale, c("relative", "per_obs"))
    directGradDenom <- switch(directTolScale,
      relative = max(1, abs(if(!is.null(directFit$optim$logLik)) directFit$optim$logLik else 1)),
      per_obs  = max(1, as.numeric(sdat$Nobs)))
    directGradScaled <- directFit$optim$masked_grad_norm / max(1, directGradDenom)
    strict_direct <- isTRUE(directGradScaled < laplaceGradTol)
    fit$laplaceStatus <- list(
      converged = strict_direct,
      reason = if(isTRUE(strict_direct)) "approx_gradient" else "max_iter",
      outer_iters = if(!is.null(directFit$optim$iter)) directFit$optim$iter else max(2L, as.integer(laplaceOuterIter)),
      beta_frozen = sdat$NpersonPreds > 0,
      initialized_from = "prior_anchored",
      direct_objective = TRUE,
      approximate_gradient = TRUE,
      gradient_type = "mode_adjusted_laplace_item_gradient",
      estimated_corr = estimateAbilityCorr,
      last_item_grad_norm = directFit$optim$masked_grad_norm,
      last_item_grad_scaled = directGradScaled,
      item_grad_denominator = directGradDenom,
      last_outer_seconds = directSec,
      ebayes_rounds = length(eb_trace),
      ebayes_iters = sum(vapply(eb_trace, function(z) z$iters, integer(1)), na.rm = TRUE) +
        as.integer(directFit$optim$iter %||% 0L),
      optimizer_terminate = direct_terminate,
      optimizer_terminate_value = if(!is.null(directFit$optim$terminate$val)) directFit$optim$terminate$val else NA_real_
    )
    fit$abilityPrior <- list(
      sd = as.numeric(sdat$AbilitySD),
      corr = as.matrix(if(!is.null(state$AbilityCorr)) state$AbilityCorr else sdat$AbilityCorr),
      precision = as.matrix(directFit$eval$prior_mats$precision),
      estimated_corr = estimateAbilityCorr
    )
    if(isTRUE(collectDirectDiag)){
      if(length(directFit$history)){
        fit$laplaceDiagnostics <- directDiag
        fit$laplaceDiagnostics[, strictCriterion :=
          itemGradNorm / max(1, directGradDenom) < laplaceGradTol]
        fit$laplaceDiagnostics[, strictStreak := {
          out <- integer(.N)
          streak <- 0L
          for(ii in seq_len(.N)){
            streak <- if(isTRUE(strictCriterion[ii])) streak + 1L else 0L
            out[ii] <- streak
          }
          out
        }]
        fit$laplaceDiagnostics[, `:=`(
          stabilityCriterion = NA,
          recentObjectiveRange = NA_real_,
          recentGradRelChange = NA_real_,
          recentItemStepMean = NA_real_,
          recentPersonStepMean = NA_real_,
          stabilityStreak = NA_integer_
        )]
      } else {
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
          stabilityCriterion = NA,
          recentObjectiveRange = NA_real_,
          recentGradRelChange = NA_real_,
          recentItemStepMean = NA_real_,
          recentPersonStepMean = NA_real_,
          strictStreak = if(fit$laplaceStatus$converged) 1L else 0L,
          stabilityStreak = NA_integer_
        )
      }
      if(isTRUE(laplacePlot)){
        try({
          bigIRT_plot_laplace_diag_df(fit$laplaceDiagnostics)
          bigIRT_refresh_plot_device()
        }, silent = TRUE)
      }
    }
    if(laplaceVerbose >= 2L){
      final_t <- directFit$optim$timings
      laplace_trace(2, sprintf(
        paste0(
          "Direct Laplace optimizer summary: iter=%s | terminate=%s | target_evals=%d | ",
          "person=%.2fs | row=%.2fs | item=%.2fs | post=%.2fs | setup=%.2fs | prior=%.2fs | item_grad=%.2fs | ",
          "mean_grad=%.2fs | corr_grad=%.2fs | total_eval=%.2fs"
        ),
        if(!is.null(directFit$optim$iter)) as.character(directFit$optim$iter) else "NA",
        direct_terminate,
        directFit$optim$target_evals,
        if(!is.null(final_t$personKernelSec)) final_t$personKernelSec else NA_real_,
        if(!is.null(final_t$rowAssemblySec)) final_t$rowAssemblySec else NA_real_,
        if(!is.null(final_t$itemKernelSec)) final_t$itemKernelSec else if(!is.null(final_t$kernelSec)) final_t$kernelSec else NA_real_,
        if(!is.null(final_t$postKernelSec)) final_t$postKernelSec else NA_real_,
        if(!is.null(final_t$setupSec)) final_t$setupSec else NA_real_,
        if(!is.null(final_t$priorSec)) final_t$priorSec else NA_real_,
        if(!is.null(final_t$itemGradSec)) final_t$itemGradSec else NA_real_,
        if(!is.null(final_t$abilityMeanSec)) final_t$abilityMeanSec else 0,
        if(!is.null(final_t$corrGradSec)) final_t$corrGradSec else 0,
        if(!is.null(final_t$totalSec)) final_t$totalSec else NA_real_
      ))
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

  # If laplace_direct is requested but there are no free ability parameters,
  # skip the Laplace optimization and fall back to the legacy JML
  # parameterization so output construction does not crash.
  if(all(is.na(fit)) && identical(marginalApprox, "laplace")){
    warning("marginalApprox='laplace' requested but no free ability parameters were found; falling back to marginalApprox='none' (JML).")
    marginalApprox <- "none"
    for(i in 1:length(JMLseq)){
      if(i < length(JMLseq)) tol= basetol*ifelse(JMLseq[[i]]$narrowPriors,100,10) else tol = basetol
      fit <- JMLfit(est = JMLseq[[i]]$est, sdat = sdat, ebayes = JMLseq[[i]]$ebayes,
        fit = fit,
        narrowPriors = JMLseq[[i]]$narrowPriors,
        tol = tol, ...)
      sdat <- fit$sdat
      fit <- fit$fit
    }
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
        ## Preserve what the model estimated before overwriting it.
        ##
        ## The line below replaces the latent correlation with the empirical
        ## correlation of the fitted ability point estimates. Those are not the
        ## same quantity: point estimates are attenuated by their own
        ## estimation error, so the empirical value is a lower bound on the
        ## latent one. With normalise = TRUE the estimated correlation was
        ## therefore unreachable from the returned object, which is how a
        ## Mindsteps fit came to report .320 for both.
        if(!is.null(fit$laplaceStatus$ability_corr))
          fit$abilityPrior$corr_latent <- as.matrix(fit$laplaceStatus$ability_corr)
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
  if(identical(marginalApprox, "laplace") && !is.null(fit$pars$sAbilitySD)){
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
    ability_sd <- apply(as.matrix(fit$pars$Ability), 2, sd)
    ability_sd[!is.finite(ability_sd) | ability_sd == 0] <- 1
    pred_sd <- apply(fit$dat$personPreds, 2, sd)
    fit$covariateEffects$AbilityStd <- sweep(fit$pars$Abilitybeta, 1, ability_sd, "/")
    fit$covariateEffects$AbilityStd <- sweep(fit$covariateEffects$AbilityStd, 2, pred_sd, "*")
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

  fit$call <- match.call()
  fit$backend <- marginalApprox
  if(!is.null(fit$laplaceStatus)){
    status <- fit$laplaceStatus
    status$strict_convergence <- isTRUE(status$converged)
    status$stable_plateau <- identical(status$reason, "stability_patience")
    ## Two backends reported hitting the cap differently: the outer loop said
    ## "max_outer_iter", the direct optimiser says "max_iter". Both mean the
    ## fit stopped because it ran out of iterations, and both must warn.
    status$iteration_limit <- identical(status$reason, "max_outer_iter") ||
      identical(as.character(status$optimizer_terminate), "max_iter")
    status$person_mode_failures <- if(!is.null(fit$personPosterior$converged))
      sum(!fit$personPosterior$converged) else NA_integer_
    status$numerical_failure <- !is.finite(fit$optim$logLik) ||
      (!is.null(status$last_item_grad_norm) && !is.finite(status$last_item_grad_norm))
    ## A tolerated number of unresolved modes must not relabel a genuine stable
    ## plateau as a failure: the fit stopped because it settled, and the
    ## unresolved count is reported separately.  Only an intolerable share, or
    ## an absent tolerance, overrides the recorded reason.
    status$person_modes_tolerated <- isTRUE(is.finite(status$person_converged_fraction)) &&
      isTRUE(is.finite(status$person_fail_fraction_allowed)) &&
      isTRUE(status$person_converged_fraction >= 1 - status$person_fail_fraction_allowed)
    if(!isTRUE(status$converged) && isTRUE(status$person_mode_failures > 0L) &&
       !isTRUE(status$person_modes_tolerated))
      status$reason <- "person_mode_failures"
    if(isTRUE(status$numerical_failure)) status$reason <- "numerical_failure"
    status$covariance_retained <- !is.null(fit$personPosterior$covariance)
    status$approximate_gradient <- isTRUE(status$approximate_gradient)
    status$frozen_effects <- isTRUE(status$beta_frozen)
    fit$laplaceStatus <- status
    intolerable_modes <- isTRUE(status$person_mode_failures > 0L) &&
      !isTRUE(status$person_modes_tolerated)
    if(isTRUE(status$iteration_limit) || intolerable_modes || isTRUE(status$numerical_failure)){
      warning(sprintf("%s stopped with %s%s.", fit$backend,
        if(isTRUE(status$iteration_limit)) "the iteration limit" else "person-mode failures",
        if(isTRUE(status$person_mode_failures > 0L)) sprintf(" (%d unresolved)", status$person_mode_failures) else ""), call. = FALSE)
    } else if(isTRUE(status$stable_plateau)){
      message(sprintf("%s stopped on a stable plateau; this is a qualified, not strict-convergence result.%s",
        fit$backend,
        if(isTRUE(status$person_mode_failures > 0L))
          sprintf(" %d of %d person modes (%.2f%%) remained unresolved, within the tolerated share.",
            status$person_mode_failures,
            length(fit$personPosterior$converged),
            100 * (1 - status$person_converged_fraction)) else ""))
    }
  }
  class(fit) <- c("bigIRT_fit", "list")

  return(fit)
}
