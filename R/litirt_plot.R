#' Visualize Proficiency Level Classification Results
#'
#' Produces visualizations of IRT-based proficiency level classification,
#' including Wright maps with benchmark overlays, theta distribution plots
#' with proficiency level shading, and misclassification probability plots.
#'
#' @param x An object of class \code{"litirt_bench"} from
#'   \code{litirt_benchmark()}, or \code{"litirt_mis"} from
#'   \code{litirt_misclass()}.
#' @param type Character. Plot type:
#'   \code{"distribution"} (default) — theta distribution with proficiency
#'   level shading and cut score lines;
#'   \code{"barplot"} — bar chart of proportion per proficiency level;
#'   \code{"misclass"} — misclassification probability distribution
#'   (requires \code{"litirt_mis"} object).
#' @param colors Character vector of colors for proficiency levels. Length
#'   must equal number of levels. Default: a colorblind-friendly palette.
#' @param title Character. Plot title. Default is auto-generated.
#'
#' @return A \code{ggplot2} object.
#'
#' @examples
#' data(pisa_data)
#' items <- pisa_data[, 1:20]
#' est   <- litirt_estimate(items, model = "2PL")
#' bench <- litirt_benchmark(est,
#'   cuts   = c(-1.5, -0.5, 0.5, 1.5),
#'   labels = c("Level 1","Level 2","Level 3","Level 4","Level 5"))
#'
#' # Distribution plot
#' litirt_plot(bench, type = "distribution")
#'
#' # Bar plot
#' litirt_plot(bench, type = "barplot")
#'
#' # Misclassification plot
#' mis <- litirt_misclass(bench)
#' litirt_plot(mis, type = "misclass")
#'
#' @export
litirt_plot <- function(x, type = "distribution",
                        colors = NULL, title = NULL) {

  if (!type %in% c("distribution", "barplot", "misclass"))
    stop("'type' must be 'distribution', 'barplot', or 'misclass'.")

  # ── Handle litirt_mis input ───────────────────────────────────────────────────
  if (inherits(x, "litirt_mis")) {
    if (type != "misclass")
      message("'litirt_mis' object detected. Switching to type = 'misclass'.")
    type <- "misclass"
  }

  if (type == "misclass") {
    if (!inherits(x, "litirt_mis"))
      stop("For type = 'misclass', provide a 'litirt_mis' object.")

    df    <- x$person_table
    ttl   <- title %||% "Misclassification Probability Distribution"

    p <- ggplot2::ggplot(df,
           ggplot2::aes(x = P_misclass, fill = Level)) +
      ggplot2::geom_histogram(bins = 30, alpha = 0.8, color = "white") +
      ggplot2::facet_wrap(~ Level, scales = "free_y") +
      ggplot2::labs(
        title    = ttl,
        subtitle = sprintf("Overall misclassification: %.2f%%",
                           x$overall_misclass * 100),
        x        = "Misclassification Probability",
        y        = "Count"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
    return(p)
  }

  # ── Validate litirt_bench ─────────────────────────────────────────────────────
  if (!inherits(x, "litirt_bench"))
    stop("'x' must be a 'litirt_bench' or 'litirt_mis' object.")

  n_levels <- x$n_levels
  labels   <- x$labels
  cuts     <- x$cuts

  # Default colors
  if (is.null(colors)) {
    default_cols <- c(
      "#d73027","#f46d43","#fdae61","#fee08b",
      "#d9ef8b","#a6d96a","#66bd63","#1a9850"
    )
    colors <- default_cols[seq_len(n_levels)]
  }
  if (length(colors) != n_levels)
    stop("Length of 'colors' must equal number of levels (", n_levels, ").")

  names(colors) <- labels

  if (type == "distribution") {

    df  <- data.frame(theta = x$theta, Level = x$level)
    ttl <- title %||% "Theta Distribution by Proficiency Level"

    # Build shaded regions
    cuts_ext <- c(-Inf, cuts, Inf)
    shade_df <- data.frame(
      xmin  = cuts_ext[-length(cuts_ext)],
      xmax  = cuts_ext[-1],
      Level = factor(labels, levels = labels)
    )
    # Clip for display
    shade_df$xmin[is.infinite(shade_df$xmin)] <- -4
    shade_df$xmax[is.infinite(shade_df$xmax)] <-  4

    p <- ggplot2::ggplot(df, ggplot2::aes(x = theta)) +
      ggplot2::geom_rect(
        data = shade_df,
        ggplot2::aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf,
                     fill=Level),
        alpha = 0.15, inherit.aes = FALSE
      ) +
      ggplot2::geom_histogram(
        ggplot2::aes(fill = Level),
        bins  = 40, alpha = 0.8, color = "white"
      ) +
      ggplot2::geom_vline(
        xintercept = cuts,
        linetype = "dashed", color = "black", linewidth = 0.7
      ) +
      ggplot2::annotate("text",
        x     = cuts,
        y     = Inf,
        label = paste0("c=", cuts),
        vjust = 1.5, hjust = -0.1,
        size  = 3, color = "black"
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        title    = ttl,
        subtitle = paste0(n_levels, " proficiency levels | ",
                          "N = ", x$n_persons),
        x        = expression(theta ~ "(Latent Trait)"),
        y        = "Count",
        fill     = "Proficiency Level"
      ) +
      ggplot2::theme_minimal(base_size = 12)

  } else if (type == "barplot") {

    df  <- x$summary
    df$Level <- factor(df$Level, levels = labels)
    ttl <- title %||% "Proficiency Level Distribution"

    p <- ggplot2::ggplot(df,
           ggplot2::aes(x = Level, y = Percent, fill = Level)) +
      ggplot2::geom_bar(stat = "identity", width = 0.65) +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(Percent, "%\n(n=", N, ")")),
        vjust = -0.3, size = 3.5
      ) +
      ggplot2::scale_fill_manual(values = colors) +
      ggplot2::labs(
        title    = ttl,
        subtitle = paste0("N = ", x$n_persons),
        x        = "Proficiency Level",
        y        = "Percentage (%)"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  }

  return(p)
}


# Null coalescing operator
`%||%` <- function(a, b) if (!is.null(a)) a else b
