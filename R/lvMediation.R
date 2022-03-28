#' Generate Latent Variable Model Syntax
#' @param
#' @return Returns syntax objects for lavaan and Mplus.

lvMediation <- function(model = NULL,
                        data = NULL,
                        estimator = "ML",
                        missing = "FIML",
                        fixed.x = TRUE,
                        se = "standard",
                        bootstrap = 1000){

  fit <- lavaan::sem(model = model,
                     data = data,
                     estimator = estimator,
                     missing = missing,
                     fixed.x = fixed.x,
                     se = se,
                     bootstrap = bootstrap)

  return(fit)
}
