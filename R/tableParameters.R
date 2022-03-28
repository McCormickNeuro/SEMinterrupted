#' Generate Tables of Parameter Estimates
#' @param data x y items m estimator missing fixed.x se bootstrap
#' @return Returns individual fit objects.
#' @keywords internal

tableParameters <- function(fits         = NULL,
                            standardize  = FALSE,
                            filter       = FALSE){

  if (!is.list(fits)){fits <- list(fits)}

  paramTables <- lapply(fits, function(x){
    if (standardize){
      tab <- lavaan::standardizedSolution(x)
    } else {
      tab <- lavaan::parameterEstimates(x)
    }
    if (filter){tab <- tab[tab$label != "",]}
    return(tab)
  })

  return(paramTables)
}
