#' Compute Misclassification Probabilities for Proficiency Level Classification
#'
#' Estimates the probability that a respondent is misclassified into an
#' incorrect proficiency level, given their theta estimate and its
#' standard error. Misclassification probability is computed by integrating
#' the normal distribution of theta (centered at the estimated theta with
#' SD equal to SE(theta)) over the incorrect classification regions.
#'
#' @param bench An object of class \code{"litirt_bench"} from
#'   \code{litirt_benchmark()}.
#'
#' @details
#' For each respondent with theta estimate \eqn{\hat\theta} and standard
#' error \eqn{SE(\hat\theta)}, the probability of correct classification
#' into level \eqn{k} (defined by cut scores \eqn{c_{k-1}} and \eqn{c_k})
#' is:
#'
#' \deqn{P(\text{correct}) = \Phi\left(\frac{c_k - \hat\theta}{SE}\right) -
#' \Phi\left(\frac{c_{k-1} - \hat\theta}{SE}\right)}
#'
#' The misclassification probability is \eqn{1 - P(\text{correct})}.
#'
#' @return A list of class \code{"litirt_mis"} containing:
#'   \item{person_table}{Data frame with theta, SE, assigned level, and
#'     misclassification probability per person.}
#'   \item{summary}{Data frame with mean misclassification probability
#'     per proficiency level.}
#'   \item{overall_misclass}{Overall mean misclassification probability.}
#'
#' @references
#'   Livingston, S. A., & Lewis, C. (1995). Estimating the consistency and
#'   accuracy of classifications based on test scores.
#'   \emph{Journal of Educational Measurement, 32}(2), 179--197.
#'   \doi{10.1111/j.1745-3984.1995.tb00462.x}
#'
#' @examples
#' data(pisa_data)
#' items <- pisa_data[, 1:20]
#' est   <- litirt_estimate(items, model = "2PL")
#' bench <- litirt_benchmark(est,
#'   cuts   = c(-1.5, -0.5, 0.5, 1.5),
#'   labels = c("Level 1","Level 2","Level 3","Level 4","Level 5"))
#' mis <- litirt_misclass(bench)
#' print(mis)
#'
#' @export
litirt_misclass <- function(bench) {

  if (!inherits(bench, "litirt_bench"))
    stop("'bench' must be a 'litirt_bench' object from litirt_benchmark().")

  if (is.null(bench$se_theta))
    stop("SE(theta) not available. Re-run litirt_estimate() and ",
         "litirt_benchmark() with SE information.")

  theta    <- bench$theta
  se       <- bench$se_theta
  cuts     <- bench$cuts
  labels   <- bench$labels
  level    <- bench$level

  # Extended cuts including -Inf and +Inf
  cuts_ext <- c(-Inf, cuts, Inf)

  # ── Compute P(correct classification) per person ─────────────────────────────
  p_correct <- sapply(seq_along(theta), function(i) {
    th  <- theta[i]
    s   <- max(se[i], 1e-6)
    lv  <- as.integer(level[i])
    lo  <- cuts_ext[lv]
    hi  <- cuts_ext[lv + 1]
    pnorm(hi, mean=th, sd=s) - pnorm(lo, mean=th, sd=s)
  })

  p_misclass <- round(1 - p_correct, 4)

  # ── Person table ──────────────────────────────────────────────────────────────
  person_df <- data.frame(
    Person       = seq_along(theta),
    Theta        = round(theta, 3),
    SE_theta     = round(se, 3),
    Level        = level,
    P_correct    = round(p_correct, 4),
    P_misclass   = p_misclass
  )

  # ── Summary per level ─────────────────────────────────────────────────────────
  summary_df <- do.call(rbind, lapply(labels, function(lv) {
    idx <- person_df$Level == lv
    data.frame(
      Level            = lv,
      N                = sum(idx),
      Mean_P_misclass  = round(mean(p_misclass[idx]), 4),
      SD_P_misclass    = round(sd(p_misclass[idx]),   4),
      Max_P_misclass   = round(max(p_misclass[idx]),  4)
    )
  }))

  overall <- round(mean(p_misclass), 4)

  result <- list(
    person_table      = person_df,
    summary           = summary_df,
    overall_misclass  = overall
  )
  class(result) <- "litirt_mis"
  return(result)
}


#' @export
print.litirt_mis <- function(x, ...) {
  cat("=== Misclassification Analysis (litirt) ===\n\n")
  cat(sprintf("Overall misclassification probability: %.4f (%.2f%%)\n\n",
              x$overall_misclass, x$overall_misclass * 100))
  cat("Per-level summary:\n\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}
