#' Classify Respondents into Proficiency Levels
#'
#' Classifies respondents into user-defined proficiency levels based on
#' IRT theta estimates and cut scores. Computes classification frequencies,
#' proportions, and summary statistics per level.
#'
#' @param est An object of class \code{"litirt_est"} from
#'   \code{litirt_estimate()}, or a numeric vector of theta estimates.
#' @param cuts Numeric vector of cut scores on the theta scale that define
#'   the boundaries between proficiency levels. For \eqn{k} levels,
#'   provide \eqn{k-1} cut scores in ascending order.
#'   Example: \code{cuts = c(-1, 0, 1)} defines 4 levels.
#' @param labels Character vector of level labels. Length must equal
#'   \code{length(cuts) + 1}. Default: \code{"Level 1"}, \code{"Level 2"}, etc.
#'   For PISA-style: \code{c("Below 1b","1b","1a","2","3","4","5","6")}.
#'   For AKM-style: \code{c("Perlu Intervensi","Dasar","Cakap","Mahir")}.
#' @param se_theta Numeric vector of standard errors of theta estimates.
#'   If provided (or available from \code{est}), used to compute
#'   classification uncertainty. Optional.
#'
#' @return A list of class \code{"litirt_bench"} containing:
#'   \item{level}{Factor vector of proficiency level assignments.}
#'   \item{theta}{Numeric vector of theta estimates used.}
#'   \item{cuts}{The cut scores used.}
#'   \item{labels}{The level labels used.}
#'   \item{summary}{Data frame with frequency and proportion per level.}
#'   \item{person_table}{Data frame with person-level results.}
#'
#' @references
#'   OECD. (2019). \emph{PISA 2018 technical report}. OECD Publishing.
#'
#'   Kemdikbudristek. (2021). \emph{Panduan teknis Asesmen Kompetensi Minimum}.
#'   Pusat Asesmen dan Pembelajaran.
#'
#' @examples
#' # PISA-style classification (5 levels)
#' data(pisa_data)
#' items <- pisa_data[, 1:20]
#' est   <- litirt_estimate(items, model = "2PL")
#' bench <- litirt_benchmark(est,
#'   cuts   = c(-1.5, -0.5, 0.5, 1.5),
#'   labels = c("Level 1", "Level 2", "Level 3", "Level 4", "Level 5"))
#' print(bench)
#'
#' # AKM-style classification (4 levels)
#' data(akm_data)
#' items_akm <- akm_data[, 1:15]
#' est_akm   <- litirt_estimate(items_akm, model = "graded")
#' bench_akm <- litirt_benchmark(est_akm,
#'   cuts   = c(-1.0, 0.0, 1.0),
#'   labels = c("Perlu Intervensi Khusus", "Dasar", "Cakap", "Mahir"))
#' print(bench_akm)
#'
#' @export
litirt_benchmark <- function(est, cuts, labels = NULL, se_theta = NULL) {

  # ── Extract theta ────────────────────────────────────────────────────────────
  if (inherits(est, "litirt_est")) {
    theta    <- est$theta
    se_theta <- if (is.null(se_theta)) est$se_theta else se_theta
  } else if (is.numeric(est)) {
    theta <- est
  } else {
    stop("'est' must be a 'litirt_est' object or a numeric vector of theta.")
  }

  # ── Validate cut scores ───────────────────────────────────────────────────────
  if (!is.numeric(cuts) || length(cuts) < 1)
    stop("'cuts' must be a numeric vector of at least one cut score.")
  if (any(diff(cuts) <= 0))
    stop("'cuts' must be strictly increasing.")

  n_levels <- length(cuts) + 1

  # ── Default labels ────────────────────────────────────────────────────────────
  if (is.null(labels)) {
    labels <- paste("Level", seq_len(n_levels))
  } else {
    if (length(labels) != n_levels)
      stop("Length of 'labels' (", length(labels), ") must equal ",
           "length(cuts) + 1 (", n_levels, ").")
  }

  # ── Classify ─────────────────────────────────────────────────────────────────
  level_num <- findInterval(theta, cuts) + 1
  level     <- factor(labels[level_num], levels = labels)

  # ── Summary table ─────────────────────────────────────────────────────────────
  freq  <- table(level)
  props <- prop.table(freq)

  summary_df <- data.frame(
    Level      = labels,
    Cut_lower  = c(-Inf, cuts),
    Cut_upper  = c(cuts,  Inf),
    N          = as.integer(freq),
    Proportion = round(as.numeric(props), 4),
    Percent    = round(as.numeric(props) * 100, 2)
  )

  # ── Person table ──────────────────────────────────────────────────────────────
  person_df <- data.frame(
    Person = seq_along(theta),
    Theta  = round(theta, 3),
    Level  = level
  )
  if (!is.null(se_theta))
    person_df$SE_theta <- round(se_theta, 3)

  result <- list(
    level        = level,
    theta        = theta,
    se_theta     = se_theta,
    cuts         = cuts,
    labels       = labels,
    n_levels     = n_levels,
    n_persons    = length(theta),
    summary      = summary_df,
    person_table = person_df
  )
  class(result) <- "litirt_bench"
  return(result)
}


#' @export
print.litirt_bench <- function(x, ...) {
  cat("=== Proficiency Level Classification (litirt) ===\n\n")
  cat(sprintf("N persons: %d\n", x$n_persons))
  cat(sprintf("N levels:  %d\n\n", x$n_levels))
  cat("Cut scores:", paste(x$cuts, collapse = ", "), "\n\n")
  cat("Classification Summary:\n\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}
