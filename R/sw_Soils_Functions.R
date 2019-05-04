getLayersWidth <- function(layers_depth) diff(c(0, layers_depth))


adjustLayer_byImp <- function(depths, imp_depth, sdepths) {
  if (any(imp_depth < depths[1])) {
    depths <- imp_depth
    if (length(sdepths) >= 2) {
      temp <- findInterval(imp_depth, sdepths)
      if (temp > 1) {
        depths <- c(sdepths[temp - 1], imp_depth)
      } else {
        depths <- c(imp_depth, sdepths[temp + 1])
      }
    }
  } else if (any(imp_depth < depths[2])) {
    depths <- c(depths[1], imp_depth)
  }

  depths
}



calc_weights_from_depths <- function(il_new, target_cm, depths_cm) {
  if (il_new == 0) {
    c(target_cm, depths_cm[1] - target_cm)

  } else if (il_new >= length(depths_cm)) {
    c(0, target_cm)

  } else {
    abs(target_cm - depths_cm[il_new + c(1, 0)])

  }
}





#' Split soil layer in two layers
#'
#' @param x A numeric data.frame or matrix. Columns are soil layers.
#' @param il An integer value. The column/soil layer number after which a new
#'   layer is added.
#' @param w A numeric vector of length one or two. The weights used to calculate
#'   the values of the new layer.
#' @param method A character string. See \code{Details}.
#'
#' @section Details: If the weight vector is of length one and \code{x} contains
#'   a row with name 'depth_cm', then it is assumed that the value of \code{w}
#'   corresponds to the weight of the first layer and the weight of the second
#'   layer is calculated as \code{(depth of first layer of x) - (first value of
#'   w)}. If this is case and if the added layer is either more shallow or
#'   deeper than any input layers, then the depth of the added layer is
#'   calculated proportionally if \code{sum(w) <= 1} otherwise additively.
#' @section Details: The method \code{interpolate} calculates the weighted mean
#'   of the columns/layers \code{il} and \code{il + 1}. If \code{il == 0}, i.e.,
#'   add layer at a more shallow depth than any existing layer, then values from
#'   the previously first layer are copied to the newly created layer. The
#'   method \code{exhaust} distributes the value of \code{il + 1} according to
#'   the weights.
#'
#' @return An object like x with one column more at position \code{il + 1}.
add_layer_to_soil <- function(x, il, w, method = c("interpolate", "exhaust")) {
  method <- match.arg(method)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }
  ncols <- dim(x)[2]
  if (length(w) == 1L && "depth_cm" %in% dimnames(x)[[1]] &&
      x["depth_cm", 1] >= w) {
    w <- c(w, x["depth_cm", 1] - w)
  }

  stopifnot(length(w) == 2L, ncols > 0, is.finite(il), il >= 0, il <= ncols)
  w_sum <- sum(w)

  if (ncols > il) {
    # Add layer at an intermediate depth of existing layers
    x <- x[, c(seq_len(il), NA, (il + 1):ncols), drop = FALSE]

    if (method == "interpolate") {
      if (il > 0) {
        x[, il + 1] <- (x[, il] * w[1] + x[, il + 2] * w[2]) / w_sum

      } else {
        # Add layer at a more shallow depth than any existing layer
        x[, 1] <- x[, 2]
        if ("depth_cm" %in% dimnames(x)[[1]])
          x["depth_cm", 1] <- if (w_sum <= 1 || w[1] > x["depth_cm", 2]) {
            x["depth_cm", 2] * w[1] / w_sum
          } else {
            w[1]
          }
      }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il + 2] * w[1] / w_sum
      x[, il + 2] <- x[, il + 2] * w[2] / w_sum
    }

  } else if (ncols == il) {
    # Add a deeper layer than any existing layer
    x <- x[, c(seq_len(ncols), NA), drop = FALSE]

    if (method == "interpolate") {
      x[, il + 1] <- x[, il]
      if ("depth_cm" %in% dimnames(x)[[1]])
        x["depth_cm", il + 1] <- if (w_sum <= 1) {
          x["depth_cm", il] * (1 + w[2] / w_sum)
        } else {
          x["depth_cm", il] + w[2]
        }

    } else if (method == "exhaust") {
      x[, il + 1] <- x[, il] * w[2] / w_sum
      x[, il] <- x[, il] * w[1] / w_sum
    }
  }

  x
}



identify_soillayers <- function(depths, sdepth) {
  it <- findInterval(depths, sdepth)
  if (any(is.na(it))) {
    as.integer(stats::na.exclude(it))
  } else if (length(it) > 1 && diff(it) > 0) {
    (1 + it[1]):(it[2])
  } else {
    it[1]
  }
}
