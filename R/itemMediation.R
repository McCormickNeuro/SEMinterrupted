#' Run Individual Item Mediation Models
#' @param data x y items m estimator missing fixed.x se bootstrap
#' @return Returns individual fit objects.
#' @keywords internal

itemMediation <- function(data       = NULL,
                          x          = NULL,
                          y          = NULL,
                          items      = NULL,
                          m          = NULL,
                          estimator  = NULL,
                          missing    = NULL,
                          fixed.x    = TRUE,
                          se         = NULL,
                          bootstrap  = 1000){

  fits <- lapply(c(items, m), function(m){
    model <- paste0("
                    # A Path
                    ", m," ~ a*", x,"

                    # B Path
                    ", y," ~ b*", m,"

                    # Direct Effect
                    ", y," ~ c*", x,"

                    # Computed Parameters
                    indirect := a*b
                    total := c + (a*b)
                    ")
    lavaan::sem(model = model, data = data, estimator = estimator,
                missing = missing, fixed.x = fixed.x, se = se,
                bootstrap = bootstrap)
  })
  names(fits) <- c(items, m)

  return(fits)
}
