


plotTidyMIs <- function(fitObj  = NULL,
                        x       = NULL,
                        y       = NULL,
                        m       = NULL,
                        items   = NULL,
                        filter  = NULL){
  #library(dplyr)
  `%>%` <- magrittr::`%>%`

  nItems <- length(items)
  layout <- matrix(NA,
                   nrow = 5,
                   ncol = (ceiling(nItems) - ceiling(nItems) %% 2) + 1)

  layout[5, 1] <- paste0(x)
  layout[5, ncol(layout)] <- paste0(y)
  layout[3, median(1:ncol(layout))] <- paste0("lv", m)
  itemseq <- seq(1, ncol(layout), by = 1)
  if ((nItems %% 2) == 0) itemseq <- itemseq[-median(1:ncol(layout))]
  for (i in 1:length(itemseq)){layout[1, itemseq[i]] <- items[i]}

  tidyPlot <- tidySEM::prepare_graph(fitObj,
                                     layout = layout,
                                     rect_width = 1,
                                     rect_height = 1,
                                     ellipses_width = 1.25,
                                     ellipses_height = 1.25,
                                     variance_diameter = 0.5,
                                     angle = 180)

  tidySEM::nodes(tidyPlot) %>%
    dplyr::mutate(colour = "black",
                  fill = "grey95",
                  label_fill = NA) -> tidySEM::nodes(tidyPlot)

  tidySEM::edges(tidyPlot) %>%
    dplyr::mutate(colour = "black",
                  label_colour = "grey90",
                  label_fill = "white",
                  linetype = 1,
                  size = 0.5,
                  alpha = ifelse(est == "0.00", 0, 0.1),
                  label = replace(label, label == "0.00", ""),
                  label_location = 0.5
    ) -> tidySEM::edges(tidyPlot)

  miTable <- tableMIs(fitObj = fitObj,
                      standardize = TRUE,
                      std.nox = FALSE,
                      pretty = FALSE)

  miTable <- miTable[miTable$mi > 5.00,]
  x <- miTable$mi
  miTable$normMI <- (2 - 0.1)*((x - min(x)) / (max(x) - min(x))) + .5

  if (filter == "itemMed"){
    miTable <- miTable[
      (miTable$rhs == x |
         miTable$lhs == y) &
        miTable$op == "~",
    ]
  }

  tidySEM::edges(tidyPlot) %>%
    dplyr::add_row(
      from = miTable$rhs, rhs = miTable$rhs,
      to = miTable$lhs, lhs = miTable$lhs, op = miTable$op,
      arrow = ifelse(miTable$op == "~~", "both", "last"),
      label = paste0(miTable$mi),
      connect_from = ifelse(miTable$rhs %in% items, "bottom", "top"),
      connect_to = ifelse(miTable$lhs %in% items, "bottom", "top"),
      est = paste0(miTable$mi), se = "0.00", pval = NA,
      confint = "[0.00, 0.00]", est_sig = paste0(miTable$mi),
      est_std = paste0(miTable$sepc.all), se_std = "0.00", pval_std = NA,
      confint_std = "[0, 0]", est_sig_std = paste0(miTable$mi),
      label_results = "TEST", lavaan_label = "", show = TRUE,
      label_colour = "black", label_fill = "white",
      label_location = stats::runif(length(miTable$mi), (1/3), (2/3)),
      curvature = 75,
      colour = "red",
      linetype = 1,
      size = miTable$normMI,
      alpha = 1
    ) -> tidySEM::edges(tidyPlot)


  return(tidyPlot)
}
