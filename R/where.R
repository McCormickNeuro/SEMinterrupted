#' Replication of tidyselect:::where() internal function
#' @keywords internal

where <- function(fn) {
  predicate <- as_function(fn)

  function(x, ...) {
    out <- predicate(x, ...)

    if (!is_bool(out)) {
      abort("`where()` must be used with functions that return `TRUE` or `FALSE`.")
    }

    out
  }
}
