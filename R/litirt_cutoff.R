#' Optimize Cut Scores for Proficiency Level Classification
#'
#' Determines optimal cut scores on the theta scale for classifying respondents
#' into proficiency levels. Three methods are supported: equal-interval
#' (divides the observed theta range into equal intervals), percentile-based
#' (places cuts at specified percentiles of the theta distribution), and
#' information-based (places cuts at theta values where test information
#' is maximized between adjacent levels).
#'
#' @param est An object of class \code{"litirt_est"} from
#'   \code{litirt_estimate()}.
#' @param n_levels Integer. Number of proficiency levels desired. Default 4.
#' @param method Character. Cut score method:
#'   \code{"equal"} (default) — equal intervals across theta range;
#'   \code{"percentile"} — cuts at equal percentiles;
#'   \code{"information"} — cuts at theta values of maximum information
#'   between adjacent levels (useful for minimizing misclassification).
#' @param range Numeric vector of length 2. Theta range for equal-interval
#'   and information methods. Default: \code{c(-3, 3)}.
#'
#' @return A list of class \code{"litirt_cut"} containing:
#'   \item{cuts}{Numeric vector of cut scores.}
#'   \item{n_levels}{Number of levels.}
#'   \item{method}{Method used.}
#'   \item{labels}{Suggested level labels.}
#'
#' @examples
#' data(pisa_data)
#' items <- pisa_data[, 1:20]
#' est   <- litirt_estimate(items, model = "2PL")
#'
#' # Equal-interval cuts for 5 levels
#' cuts5 <- litirt_cutoff(est, n_levels = 5, method = "equal")
#' print(cuts5)
#'
#' # Percentile-based cuts for 4 levels
#' cuts4p <- litirt_cutoff(est, n_levels = 4, method = "percentile")
#' print(cuts4p)
#'
#' @export
litirt_cutoff <- function(est,
                          n_levels = 4,
                          method   = "equal",
                          range    = c(-3, 3)) {

  if (!inherits(est, "litirt_est"))
    stop("'est' must be a 'litirt_est' object from litirt_estimate().")
  if (n_levels < 2)
    stop("'n_levels' must be at least 2.")
  if (!method %in% c("equal", "percentile", "information"))
    stop("'method' must be 'equal', 'percentile', or 'information'.")

  theta <- est$theta

  cuts <- switch(method,

    equal = {
      seq(range[1], range[2], length.out = n_levels + 1)[2:n_levels]
    },

    percentile = {
      probs <- seq(0, 1, length.out = n_levels + 1)[2:n_levels]
      as.numeric(quantile(theta, probs = probs))
    },

    information = {
      # Place cuts at theta values where test information peaks
      # between adjacent equal-interval points
      theta_seq  <- seq(range[1], range[2], by = 0.01)
      info_vals  <- mirt::testinfo(est$fit, theta_seq)
      anchor_pts <- seq(range[1], range[2], length.out = n_levels + 1)
      sapply(2:n_levels, function(i) {
        idx <- which(theta_seq >= anchor_pts[i-1] &
                     theta_seq <= anchor_pts[i+1])
        theta_seq[idx][which.max(info_vals[idx])]
      })
    }
  )

  cuts <- round(sort(cuts), 3)
  labels <- paste("Level", seq_len(n_levels))

  result <- list(
    cuts     = cuts,
    n_levels = n_levels,
    method   = method,
    labels   = labels,
    range    = range
  )
  class(result) <- "litirt_cut"
  return(result)
}


#' @export
print.litirt_cut <- function(x, ...) {
  cat("=== Optimal Cut Scores (litirt) ===\n\n")
  cat(sprintf("Method:   %s\n", x$method))
  cat(sprintf("N levels: %d\n\n", x$n_levels))
  cat("Cut scores:\n")
  for (i in seq_along(x$cuts))
    cat(sprintf("  Level %d | Level %d boundary: %.3f\n",
                i, i+1, x$cuts[i]))
  invisible(x)
}
