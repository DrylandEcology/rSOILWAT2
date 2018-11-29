
replace_NAs_with_val <- function(x, val_replace) {
  x[is.na(x)] <- val_replace
  x
}

squash_into_low_high <- function(x, val_low = 0, val_low_replace = val_low,
  val_high = 1, val_high_replace = val_high) {
  if (!is.null(val_low)) x[x < val_low] <- val_low_replace
  if (!is.null(val_high)) x[x > val_high] <- val_high_replace
  x
}

cut0Inf <- function(x, val = NA) {
  squash_into_low_high(x, val_low = 0, val_low_replace = val,
    val_high = NULL)
}

finite01 <- function(x, val_low_replace = 0, val_high_replace = 1) {
  x <- replace_NAs_with_val(x, val_replace = val_low_replace)
  squash_into_low_high(x, val_low = 0, val_low_replace = val_low_replace,
    val_high = 1, val_high_replace = val_high_replace)
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
