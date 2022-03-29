#' Replication of tidyselect:::where() internal function
#' @keywords internal

where <- function(fn) {
  predicate <- as.function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!is.logical(out)) {
      abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}
