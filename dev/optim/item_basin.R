# Prototype: item-wise basin search for models with free asymptotes.
#
# Each person answers an item at most once, so with every other parameter held
# fixed the marginal likelihood depends on item j's parameters psi_j exactly as
#
#   sum_{i answered j} log  integral p(y_ij | theta; psi_j) p(theta | y_i,-j) dtheta
#
# where p(theta | y_i,-j) is person i's posterior without item j -- the cavity.
# It does not depend on psi_j, so the item's problem is separable once the
# cavities are known. The cavity is taken Gaussian, from the Laplace posterior
# N(m, 1/H) with item j's own second-order contribution removed:
#
#   tau = H - h_ij,   mu = m - g_ij / tau,
#
# g, h the first and minus-second derivative of log p(y_ij | theta) at m. The
# integral of a response probability against a Gaussian is one-dimensional
# whatever K is (the item sees theta only through a'theta), done by
# Gauss-Hermite. The persons' re-adaptation to psi_j is therefore built in,
# which holding the modes fixed would not do.
#
# The search: from several starts in the asymptote coordinates, maximise each
# item's cavity objective plus its prior; where another start's optimum beats
# the current basin's by more than `min_gain`, propose it. Proposals are then
# checked against the full Laplace objective and kept only if it improves.
# K = 1 in this prototype.

cavity_rows <- function(e, sdat, ctx){
  rows <- ctx$train_rows; rc <- ctx$row_context
  re <- G("bigIRT_laplace_row_effective")(state = e$state, sdat = sdat,
    thetaBase = e$posterior$theta_mode, rows = rows, context = rc)
  ids <- as.integer(rc$ids); y <- sdat$score[rows]; N <- sdat$Nsubs
  a <- re$loadings[, 1]; eta <- re$eta_row; cc <- re$c_row; dd <- re$d_row
  m <- as.numeric(e$posterior$theta_mode[, 1])
  H <- 1 / as.numeric(e$posterior$covariance)[seq_len(N)]
  sg <- plogis(eta); u <- dd - cc
  p <- pmin(pmax(cc + u * sg, 1e-12), 1 - 1e-12)
  p1 <- u * sg * (1 - sg) * a
  p2 <- u * sg * (1 - sg) * (1 - 2 * sg) * a^2
  r <- (y - p) / (p * (1 - p))
  g <- p1 * r
  h <- -(p2 * r - p1^2 * (y / p^2 + (1 - y) / (1 - p)^2))
  P0 <- as.numeric(e$prior_mats$precision)[1]
  tau <- pmax(H[ids] - h, P0)
  list(mu = m[ids] - g / tau, sd = 1 / sqrt(tau), y = y,
    item = as.integer(rc$items), ids = ids, lo = a, int = eta - a * re$row_ability[, 1])
}

## The item's cavity log-likelihood and its gradient in raw coordinates
## psi = (invspA, B, logitC, logitD); `free` masks the coordinates optimised.
item_cavity_fg <- function(psi, mu, sd, y, xq, wq){
  araw <- psi[1]; b <- psi[2]
  a <- log1p(exp(araw)); da <- plogis(araw)
  cx <- psi[3]; dx <- psi[4]
  cc <- plogis(cx) / 2; dd <- plogis(dx) / 2 + 0.5
  dc <- cc * (1 - 2 * cc); ddd <- (2 * dd - 1) * (1 - dd)
  th <- outer(mu, rep(1, length(xq))) + outer(sd, xq)          # n x Q
  sg <- plogis(a * th - b)
  P <- as.numeric((cc + (dd - cc) * sg) %*% wq)
  P <- pmin(pmax(P, 1e-12), 1 - 1e-12)
  f <- sum(y * log(P) + (1 - y) * log1p(-P))
  w <- y / P - (1 - y) / (1 - P)
  ds <- (dd - cc) * sg * (1 - sg)
  gA <- sum(w * as.numeric((ds * th) %*% wq)) * da
  gB <- -sum(w * as.numeric(ds %*% wq))
  Es <- as.numeric(sg %*% wq)
  gC <- sum(w * (1 - Es)) * dc
  gD <- sum(w * Es) * ddd
  list(f = f, g = c(gA, gB, gC, gD))
}

item_basin_search <- function(e, sdat, ctx, L, Q = 9L, min_gain = 0.05,
  start_offsets = c(-2, 0, 2), verbose = TRUE){
  cr <- cavity_rows(e, sdat, ctx)
  gh <- statmod::gauss.quad(Q, "hermite")
  xq <- sqrt(2) * gh$nodes; wq <- gh$weights / sqrt(pi)
  st <- e$state
  J <- length(st$Bpars)
  hasC <- length(L$C) > 0L; hasD <- length(L$D) > 0L
  pri_mean <- c(st$invspAMean, st$BMean, if(hasC) st$logitCMean else NA, if(hasD) st$logitDMean else NA)
  pri_sd <- c(sdat$invspASD, sdat$BSDx, if(hasC) sdat$logitCSD else NA, if(hasD) sdat$logitDSD else NA)
  free <- c(TRUE, TRUE, hasC, hasD)
  by_item <- split(seq_along(cr$y), cr$item)
  out <- vector("list", J)
  for(j in seq_len(J)){
    r <- by_item[[as.character(j)]]
    if(!length(r)) next
    psi0 <- c(st$invspApars[j], st$Bpars[j],
      if(hasC) st$logitCpars[j] else -30, if(hasD) st$logitDpars[j] else 30)
    obj <- function(z){
      psi <- psi0; psi[free] <- z
      fg <- item_cavity_fg(psi, cr$mu[r], cr$sd[r], cr$y[r], xq, wq)
      pr <- -0.5 * sum(((psi - pri_mean) / pri_sd)^2 * free, na.rm = TRUE)
      gp <- -((psi - pri_mean) / pri_sd^2); gp[!free] <- 0
      list(f = -(fg$f + pr), g = -(fg$g + gp)[free])
    }
    run <- function(z0, mask = rep(TRUE, length(z0))){
      ## `mask` optimises a subset, the rest held at z0.
      o <- stats::optim(z0[mask], function(zz){ z <- z0; z[mask] <- zz; obj(z)$f },
        function(zz){ z <- z0; z[mask] <- zz; obj(z)$g[mask] }, method = "BFGS",
        control = list(maxit = 200, reltol = 1e-12))
      z <- z0; z[mask] <- o$par
      list(z = z, f = -o$value)
    }
    z0 <- psi0[free]
    res <- list(run(z0))
    ## Other basins. Moving the asymptotes alone and releasing everything slides
    ## back: a steep, hard item with its guessing moved to zero is far from any
    ## optimum. So profile first -- solve a and b with the asymptotes held at the
    ## start -- then release all of them.
    ab <- c(TRUE, TRUE, FALSE, FALSE)[free]
    for(off in start_offsets){
      z <- psi0
      if(hasC) z[3] <- pri_mean[3] + off * pri_sd[3]
      if(hasD) z[4] <- pri_mean[4] - off * pri_sd[4]
      z[1:2] <- pri_mean[1:2]
      s1 <- run(z[free], ab)
      res[[length(res) + 1L]] <- run(s1$z)
    }
    fs <- vapply(res, `[[`, numeric(1), "f")
    best <- which.max(fs)
    out[[j]] <- list(j = j, f_cur = fs[1], f_best = fs[best], gain = fs[best] - fs[1],
      z_cur = res[[1]]$z, z_best = res[[best]]$z, dist = sqrt(sum((res[[best]]$z - res[[1]]$z)^2)))
  }
  out <- Filter(Negate(is.null), out)
  tab <- data.table::rbindlist(lapply(out, function(o) data.table::data.table(j = o$j, gain = o$gain, dist = o$dist)))
  if(verbose){
    cat(sprintf("item basin search: %d items, %d with another basin better by > %.2g nats\n",
      nrow(tab), sum(tab$gain > min_gain & tab$dist > 0.1), min_gain))
    print(head(tab[order(-gain)], 10))
  }
  list(table = tab, detail = out, free = free)
}

## Apply the proposed switches to a packed parameter vector.
apply_switches <- function(par, search, L, ps, which_items){
  hasC <- length(L$C) > 0L; hasD <- length(L$D) > 0L
  for(o in search$detail){
    if(!(o$j %in% which_items)) next
    psi <- rep(NA_real_, 4); psi[search$free] <- o$z_best
    par[L$A[o$j]] <- psi[1] / ps[L$A[o$j]]
    par[L$B[o$j]] <- psi[2] / ps[L$B[o$j]]
    if(hasC) par[L$C[o$j]] <- psi[3] / ps[L$C[o$j]]
    if(hasD) par[L$D[o$j]] <- psi[4] / ps[L$D[o$j]]
  }
  par
}
