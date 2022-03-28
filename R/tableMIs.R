#' Generate Tables of Model Modification Indices
#' @param
#' @return Returns modification index table.
#' @keywords internal

tableMIs <- function(fitObj      = NULL,
                     standardize = TRUE,
                     std.nox     = FALSE,
                     pretty      = FALSE){

  `%>%` <- magrittr::`%>%`

  table <- lavaan::modindices(object = fitObj, sort = TRUE) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), round, 3))

  if (pretty){
    table <- table %>%
      dplyr::rowwise() %>%
      dplyr::mutate(term = paste(c(lhs, op, rhs), collapse=" ")) %>%
      dplyr::select(-lhs, -op, -rhs) %>%
      dplyr::relocate(term)

    colnames(table) <- c("Parameter","MI","EPC","EPC Std. LV",
                         "EPC Std. All","EPC Std. NoX")
    table <- table[,c(rep(TRUE,3), standardize, standardize, std.nox)]
  } else {
    table <- table[,c(rep(TRUE,5), standardize, standardize, std.nox)]
  }

  return(table)
}
