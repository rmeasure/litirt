#' Estimate IRT Parameters and Person Theta Scores
#'
#' Fits a unidimensional IRT model to item response data and estimates
#' person ability (theta) scores. Supports dichotomous (2PL, 3PL, Rasch)
#' and polytomous (GRM, GPCM, PCM, RSM) models via the \code{mirt} package.
#'
#' @param data A \code{data.frame} or \code{matrix} of item responses.
#'   Rows = persons, columns = items. For dichotomous items: 0/1.
#'   For polytomous items: consecutive integers starting from 0 or 1.
#'   Remove any non-item columns (group, ID) before passing.
#' @param model Character. IRT model type passed to \code{mirt::mirt()}.
#'   Common options: \code{"2PL"} (default), \code{"Rasch"},
#'   \code{"3PL"}, \code{"graded"} (GRM), \code{"gpcm"}, \code{"rsm"}.
#' @param method Character. Theta estimation method: \code{"EAP"} (default),
#'   \code{"MAP"}, \code{"ML"}, or \code{"WLE"}.
#' @param verbose Logical. Print estimation progress. Default \code{FALSE}.
#'
#' @return A list of class \code{"litirt_est"} containing:
#'   \item{fit}{The fitted \code{mirt} object.}
#'   \item{theta}{Numeric vector of person theta estimates.}
#'   \item{se_theta}{Numeric vector of standard errors of theta estimates.}
#'   \item{item_params}{Data frame of item parameters.}
#'   \item{reliability}{Marginal reliability of the theta estimates.}
#'   \item{n_persons}{Number of persons.}
#'   \item{n_items}{Number of items.}
#'   \item{model}{IRT model used.}
#'   \item{method}{Theta estimation method used.}
#'
#' @references
#'   Chalmers, R. P. (2012). mirt: A multidimensional item response theory
#'   package for the R environment. \emph{Journal of Statistical Software,
#'   48}(6), 1--29. \doi{10.18637/jss.v048.i06}
#'
#' @examples
#' # Using built-in PISA data
#' data(pisa_data)
#' items <- pisa_data[, 1:20]
#' est <- litirt_estimate(items, model = "2PL")
#' print(est)
#'
#' # Using built-in AKM data
#' data(akm_data)
#' items_akm <- akm_data[, 1:15]
#' est_akm <- litirt_estimate(items_akm, model = "graded")
#' print(est_akm)
#'
#' @export
litirt_estimate <- function(data,
                            model   = "2PL",
                            method  = "EAP",
                            verbose = FALSE) {

  # ── Input validation ────────────────────────────────────────────────────────
  if (!is.data.frame(data) && !is.matrix(data))
    stop("'data' must be a data.frame or matrix.")

  data <- as.data.frame(data)

  # Check all columns numeric
  not_num <- !sapply(data, function(x) is.numeric(x) || is.integer(x))
  if (any(not_num))
    stop("All columns must be numeric. Non-numeric columns: ",
         paste(names(data)[not_num], collapse = ", "),
         "\nRemove ID or group columns before passing to litirt_estimate().")

  # Check missing values
  if (any(is.na(data)))
    stop("Missing values detected. Remove or impute before estimating.")

  n_persons <- nrow(data)
  n_items   <- ncol(data)

  # Sample size check
  if (n_persons < 100)
    warning("Sample size (N = ", n_persons, ") is below the recommended ",
            "minimum of 100 for stable IRT parameter estimation.")
  if (n_items < 5)
    warning("Number of items (k = ", n_items, ") is below the recommended ",
            "minimum of 5. Parameter estimates may be unstable.")

  # ── Map model alias to mirt itemtype ────────────────────────────────────────
  model_map <- c(
    "2PL"   = "2PL",
    "Rasch" = "Rasch",
    "1PL"   = "Rasch",
    "3PL"   = "3PL",
    "graded"= "graded",
    "GRM"   = "graded",
    "gpcm"  = "gpcm",
    "GPCM"  = "gpcm",
    "pcm"   = "gpcm",
    "rsm"   = "rsm",
    "RSM"   = "rsm"
  )

  itemtype <- model_map[model]
  if (is.na(itemtype))
    stop("Unknown model: '", model, "'. Use one of: ",
         paste(names(model_map), collapse = ", "))

  # ── Fit IRT model ────────────────────────────────────────────────────────────
  message("Fitting ", model, " model to ", n_persons,
          " persons x ", n_items, " items...")

  fit <- tryCatch(
    mirt::mirt(data     = data,
               model    = 1,
               itemtype = itemtype,
               verbose  = verbose,
               SE       = TRUE),
    error = function(e)
      stop("IRT estimation failed: ", e$message,
           "\nCheck that responses are correctly coded for the selected model.")
  )

  # ── Extract theta and SE ─────────────────────────────────────────────────────
  fs      <- mirt::fscores(fit, method = method, full.scores.SE = TRUE)
  theta   <- as.numeric(fs[, 1])
  se_theta <- as.numeric(fs[, 2])

  # ── Extract item parameters ──────────────────────────────────────────────────
  item_params <- tryCatch(
    as.data.frame(round(
      mirt::coef(fit, simplify = TRUE, IRTpars = TRUE)$items, 3)),
    error = function(e) as.data.frame(
      round(mirt::coef(fit, simplify = TRUE)$items, 3))
  )
  item_params$item <- rownames(item_params)
  item_params <- item_params[, c("item",
                                  setdiff(names(item_params), "item"))]
  rownames(item_params) <- NULL

  # ── Marginal reliability ─────────────────────────────────────────────────────
  reliability <- tryCatch(
    as.numeric(mirt::empirical_rxx(fs)),
    error = function(e) {
      var_theta <- var(theta)
      mean_se2  <- mean(se_theta^2)
      (var_theta - mean_se2) / var_theta
    }
  )

  result <- list(
    fit         = fit,
    theta       = theta,
    se_theta    = se_theta,
    item_params = item_params,
    reliability = round(reliability, 3),
    n_persons   = n_persons,
    n_items     = n_items,
    model       = model,
    method      = method
  )
  class(result) <- "litirt_est"
  return(result)
}


#' @export
print.litirt_est <- function(x, ...) {
  cat("=== IRT Estimation Results (litirt) ===\n\n")
  cat(sprintf("Model:        %s\n", x$model))
  cat(sprintf("Method:       %s\n", x$method))
  cat(sprintf("N persons:    %d\n", x$n_persons))
  cat(sprintf("N items:      %d\n", x$n_items))
  cat(sprintf("Reliability:  %.3f\n\n", x$reliability))
  cat("Theta summary:\n")
  print(round(summary(x$theta), 3))
  cat("\nSE(theta) summary:\n")
  print(round(summary(x$se_theta), 3))
  cat("\nItem parameters (first 6 items):\n")
  print(head(x$item_params), row.names = FALSE)
  invisible(x)
}
