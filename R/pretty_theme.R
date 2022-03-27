#' Pretty ggplot2 Theme for Plots
#' @keywords internal

pretty_theme <- function(c_bg = "white", ...) {

  ## Colors — stick with the ggplot2() greys
  c_bg    <- c_bg
  c_grid  <- "grey80"
  c_btext <- "grey5"
  c_mtext <- "grey30"

  # Begin construction of chart
  ggplot2::theme_bw(base_size = 12, base_family = "Helvetica") +

    # Region
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = c_bg, color = c_bg),
      plot.background  = ggplot2::element_rect(fill = c_bg, color = c_bg),
      panel.border     = ggplot2::element_rect(color = c_bg)
    ) +

    # Grid
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = c_grid, size = .25),
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks       = ggplot2::element_blank()
    ) +

    # Legend
    ggplot2::theme(
      legend.position = c(0, 1),
      legend.justification = c(0, 1),
      legend.key           = ggplot2::element_rect(fill = NA, color = NA),
      legend.background    = ggplot2::element_rect(fill = "transparent", color = NA),
      legend.text          = ggplot2::element_text(color = c_mtext)
    ) +

    # Titles, labels, etc.
    ggplot2::theme(
      plot.title     = ggplot2::element_text(
        color = c_btext,
        vjust = 1.25,
        face = "bold",
        size = 18
      ),
      axis.text      = ggplot2::element_text(size = 10, color = c_mtext),
      axis.title.x   = ggplot2::element_text(
        size = 12,
        color = c_mtext,
        hjust = 0.5
      ),
      axis.title.y   = ggplot2::element_text(
        size = 12,
        color = c_mtext,
        hjust = 0.5
      )
    ) +
    # Facets
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = c_bg, color = c_btext),
      strip.text = ggplot2::element_text(size = 10, color = c_btext)
    ) +

    # Plot margins
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.35, 0.2, 0.3, 0.35), "cm")) +

    # Additionals
    ggplot2::theme(...)
}
