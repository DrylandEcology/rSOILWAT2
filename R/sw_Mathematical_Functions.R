cut0Inf <- function(x, val = NA) {
  x[x < 0] <- val
  x
}

NAto0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

finite01 <- function(x, val_low = 0, val_high = 1) {
  x[x < 0 | is.na(x)] <- val_low
  x[x > 1] <- val_high
  x
}

calc.loess_coeff <- function(N, span) {
  # prevent call to loessc.c:ehg182(104):
  # "span too small.   fewer data values than degrees of freedom"
  lcoef <- list(span = min(1, span), degree = 2)
  if (span <= 1) {
    # see R/trunk/src/library/stats/src/loessf.f:ehg136()
    nf <- floor(lcoef$span * N) - 1
    if (nf > 2) {
      lcoef$degree <- 2
    } else if (nf > 1) {
      lcoef$degree <- 1
    } else {
      lcoef <- Recall(N, lcoef$span + 0.1)
    }
  }
  lcoef
}


calc_starts <- function(x) {
  temp1 <- rle(as.logical(x))
  temp2 <- cumsum(c(0, temp1$lengths)) + 1
  temp2[-length(temp2)][temp1$values]
}
