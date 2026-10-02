#' Simulated PISA Reading Literacy Dataset
#'
#' A simulated dataset representative of PISA reading literacy item response
#' data. The dataset contains dichotomous item responses for 500 respondents
#' on 20 items, with item parameters calibrated to approximate the
#' psychometric properties of PISA 2018 reading literacy items.
#' PISA defines six proficiency levels for reading literacy with cut scores
#' at approximately -1.50, -0.50, 0.50, 1.50, and 2.50 on the theta scale.
#'
#' @format A data frame with 500 rows and 21 variables:
#'   \describe{
#'     \item{R01--R20}{Dichotomous item responses (0 = incorrect, 1 = correct)}
#'     \item{group}{Country/region group: "Group_A" or "Group_B"}
#'   }
#'
#' @source Simulated data based on PISA 2018 Technical Report (OECD, 2019).
#'
#' @references
#'   OECD. (2019). \emph{PISA 2018 technical report}. OECD Publishing.
#'   \url{https://www.oecd.org/pisa/data/pisa2018technicalreport/}
#'
#' @examples
#' data(pisa_data)
#' head(pisa_data)
#' dim(pisa_data)
"pisa_data"


#' Simulated AKM (Asesmen Kompetensi Minimum) Literacy Dataset
#'
#' A simulated dataset representative of Indonesian AKM (Asesmen Kompetensi
#' Minimum) literacy item response data. AKM is Indonesia's national minimum
#' competency assessment measuring reading literacy and numeracy. The dataset
#' contains polytomous item responses (0-3) for 400 respondents on 15 items.
#' AKM defines four proficiency levels: Perlu Intervensi Khusus (Below Basic),
#' Dasar (Basic), Cakap (Proficient), and Mahir (Advanced), with cut scores
#' at approximately -1.00, 0.00, and 1.00 on the theta scale.
#'
#' @format A data frame with 400 rows and 16 variables:
#'   \describe{
#'     \item{L01--L15}{Polytomous item responses (0-3)}
#'     \item{group}{School level group: "SD" (elementary) or "SMP" (junior high)}
#'   }
#'
#' @source Simulated data based on AKM technical specifications
#'   (Kemdikbudristek, 2021).
#'
#' @references
#'   Kemdikbudristek. (2021). \emph{Panduan teknis Asesmen Kompetensi Minimum}.
#'   Pusat Asesmen dan Pembelajaran, Kemdikbudristek.
#'
#' @examples
#' data(akm_data)
#' head(akm_data)
#' dim(akm_data)
"akm_data"


# ── Internal function: generate built-in datasets ─────────────────────────────
.generate_datasets <- function() {

  set.seed(2024)

  # ── PISA reading literacy (dichotomous, 20 items, 500 persons) ────────────
  n_pisa  <- 500
  n_items <- 20
  theta_pisa <- rnorm(n_pisa, 0, 1)

  # Item parameters approximating PISA reading items
  a_pisa <- c(0.8, 1.0, 1.2, 0.9, 1.1, 1.3, 0.7, 1.0, 1.2, 0.8,
              1.1, 0.9, 1.3, 1.0, 0.8, 1.2, 1.0, 0.9, 1.1, 0.8)
  b_pisa <- c(-2.0, -1.5, -1.0, -0.5,  0.0,  0.5,  1.0,  1.5,
               2.0, -1.8, -0.8,  0.2,  1.2, -1.2,  0.0,  0.8,
               1.8, -0.5,  0.5,  1.5)

  sim_2pl <- function(theta, a, b) {
    p <- 1 / (1 + exp(-a * (theta - b)))
    rbinom(length(theta), 1, p)
  }

  pisa_items <- as.data.frame(
    mapply(function(a, b) sim_2pl(theta_pisa, a, b),
           a_pisa, b_pisa)
  )
  names(pisa_items) <- paste0("R", sprintf("%02d", 1:n_items))
  pisa_items$group <- ifelse(theta_pisa > 0, "Group_A", "Group_B")

  # ── AKM literacy (polytomous 0-3, 15 items, 400 persons) ─────────────────
  n_akm    <- 400
  n_litems <- 15
  theta_akm <- rnorm(n_akm, 0, 1)

  a_akm <- c(0.9, 1.1, 1.3, 0.8, 1.0, 1.2, 0.9, 1.1,
             0.8, 1.0, 1.2, 0.9, 1.1, 0.8, 1.0)
  b_akm <- list(
    c(-2.0,-1.0, 0.0), c(-1.5,-0.5, 0.5), c(-1.0, 0.0, 1.0),
    c(-1.8,-0.8, 0.2), c(-1.2,-0.2, 0.8), c(-0.5, 0.5, 1.5),
    c(-2.2,-1.2,-0.2), c(-1.6,-0.6, 0.4), c(-0.8, 0.2, 1.2),
    c(-1.4,-0.4, 0.6), c(-1.0, 0.0, 1.0), c(-1.8,-0.8, 0.2),
    c(-1.2,-0.2, 0.8), c(-0.6, 0.4, 1.4), c(-1.0, 0.0, 1.0)
  )

  sim_grm_poly <- function(theta, a, b) {
    k <- length(b) + 1
    sapply(theta, function(th) {
      cum_p <- c(1, sapply(b, function(bk) 1/(1+exp(-a*(th-bk)))), 0)
      cat_p <- pmax(diff(-cum_p), 0)
      cat_p <- cat_p / sum(cat_p)
      sample(0:(k-1), 1, prob=cat_p)
    })
  }

  akm_items <- as.data.frame(
    mapply(function(a, b) sim_grm_poly(theta_akm, a, b),
           a_akm, b_akm)
  )
  names(akm_items) <- paste0("L", sprintf("%02d", 1:n_litems))
  akm_items$group <- ifelse(theta_akm > 0, "SMP", "SD")

  list(pisa_data = pisa_items, akm_data = akm_items)
}
