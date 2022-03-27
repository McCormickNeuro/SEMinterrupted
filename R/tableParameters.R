#' Generate Tables of Parameter Estimates
#' @param data x y items m estimator missing fixed.x se bootstrap
#' @return Returns individual fit objects.
#' @keywords internal

tableParameters <- function(fits         = NULL,
                            standardize  = FALSE){

  if (!is.list(fits)){fits <- list(fits)}

  paramTables <- lapply(fits, function(x){
    if (standardize){
      lavaan::standardizedSolution(x)
    } else {
      lavaan::parameterEstimates(x)
    }
  })

  return(paramTables)
}
