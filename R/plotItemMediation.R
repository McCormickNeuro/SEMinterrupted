#' Plot Item Mediation Effects
#' @param paramTables m
#' @return Returns ggplot object

plotItemMediation <- function(paramTables  = NULL,
                              m            = NULL){

  if (is.null(m)){warning("A name for the composite was not provided. Please ensure that this was intended.")}

  `%>%` <- magrittr::`%>%`
  effects <- c("a", "b", "c", "indirect", "total")

  gList <- lapply(effects, function(eff){
    effs <- data.frame(
      y = sapply(paramTables, '[[',
                 pmatch("est", names(paramTables[[1]])))[which(effects == eff),],
      ymin = sapply(paramTables, '[[', "ci.lower")[which(effects == eff),],
      ymax = sapply(paramTables, '[[', "ci.upper")[which(effects == eff),])

    effs <- effs %>%
      dplyr::mutate(
        type = ifelse(rownames(effs) == m,
                      "composite", "item"),
        x1 = ifelse(rownames(effs) == m, 0,
                    stats::reorder(rownames(effs),
                                   abs(effs[m, "y"] - effs$y))),
        x2 = ifelse(rownames(effs) == m, 0,
                    abs(match(rownames(effs),
                              sort(rownames(effs))) -
                          max(match(rownames(effs),
                                    sort(rownames(effs)))))),
        var = rownames(effs)
      )

    g <- ggplot2::ggplot(effs) +
      ggplot2::geom_pointrange(
        mapping=ggplot2::aes(x=stats::reorder(var, x1),
                             y=y,
                             ymin=ymin,
                             ymax=ymax,
                             color=type)) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0,
                          color = "black",
                          linetype="dashed") +
      ggplot2::labs(y=paste0(tools::toTitleCase(eff),
                             " Effect Estimate"),
                    x="Item") +
      ggplot2::theme(legend.position = 'none') +
      ggsci::scale_color_npg() +
      pretty_theme(legend.position='none')
    q <- ggplot2::ggplot(effs) +
      ggplot2::geom_pointrange(
        mapping=ggplot2::aes(x=stats::reorder(var, x2),
                             y=y,
                             ymin=ymin,
                             ymax=ymax,
                             color=type)) +
      ggplot2::coord_flip() +
      ggplot2::geom_hline(yintercept = 0,
                          color = "black",
                          linetype="dashed") +
      ggplot2::labs(y=paste0(tools::toTitleCase(eff),
                             " Effect Estimate"),
                    x="Item") +
      ggplot2::theme(legend.position = 'none') +
      ggsci::scale_color_npg() +
      pretty_theme(legend.position='none')

    return(list(g = g, q = q))
  })

  gq <- ((gList[[1]]$g + ggplot2::labs(title="Items by Effect Discrepancy") +
           ggplot2::theme(plot.title=ggplot2::element_text(size=14))) |
          gList[[2]]$g |
          gList[[4]]$g) /
    ((gList[[1]]$q +
        ggplot2::ggtitle("Items by Name") +
        ggplot2::labs(title="Items by Name") +
        ggplot2::theme(plot.title=ggplot2::element_text(size=14))) |
       gList[[2]]$q |
       gList[[4]]$q)

  gq <- gq +
    patchwork::plot_annotation(title = "Distribution of Mediation Effects",
                    theme = ggplot2::theme(
                      plot.title = ggplot2::element_text(
                        size = 18,
                        face = "bold"))) &
    ggplot2::theme(plot.background = ggplot2::element_rect(
      fill = "white", color = "white"))

  return(gq)
}
