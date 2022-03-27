#' Compute Composite Mediator
#' @param data items type nickname
#' @return Returns data with composite mediator attached.
#' @keywords internal

computeComposite <- function(data      = NULL,
                             items     = NULL,
                             type      = NULL,
                             nickname  = NULL){

  temp <- data.frame(M = double(nrow(data)))

  # Compute Based on Type
  if (type == "sum"){temp$M <- rowSums(data[,items], na.rm = TRUE)}
  if (type == "mean"){temp$M <- rowMeans(data[,items], na.rm = TRUE)}
  if (type == "difference"){temp$M <- data[,items[1]] - data[,items[2]]}
  if (type == "product"){temp$M <- data[,items[1]] * data[,items[2]]}
  if (type == "ratio"){temp$M <- data[,items[1]] / data[,items[2]]}

  # Rename with User-provided Mediator Name or Standard Name
  if (!is.null(nickname) & nickname != ""){names(temp) <- nickname
  } else {names(temp) <- paste0(tools::toTitleCase(type), "Score")}

  # Merge Mediator with Data
  data <- cbind(data, temp)

  return(data)
}
