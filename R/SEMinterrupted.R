#' @name SEMinterupted
#' @aliases seminterrupted
#' @title Detecting Interrupted Mediation with Structural Equation Models
#' @description Main function for SEMinterrupted
#' @usage
#' SEMinterrupted(data              = NULL,
#'                x                 = NULL,
#'                y                 = NULL,
#'                items             = NULL,
#'                compositeType     = NULL,
#'                estimator         = "ML",
#'                missing           = "FIML",
#'                fixed.x           = FALSE,
#'                se                = "standard",
#'                bootstrap         = 1000)
#'
#' @import lavaan ggplot2 patchwork
#' @importFrom tools toTitleCase
#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter mutate select rowwise relocate across
#' @importFrom stats reorder
#' @importFrom ggsci scale_color_npg
#'
#' @return Object containing SEM, interrupted outputs
#' @author Ethan M. McCormick
#' @keywords SEMinterrupted
#' @export SEMinterrupted

SEMinterrupted <- function(data                  = NULL,
                           id                    = NULL,
                           x                     = NULL,
                           y                     = NULL,
                           items                 = NULL,
                           compositeType         = c("sum", "mean", "difference", "product", "ratio"),
                           compositeName         = NULL,
                           latentSpec            = c("both", "composite", "cfa"),
                           estimator             = "ML",
                           missing               = "FIML",
                           fixed.x               = FALSE,
                           se                    = "standard",
                           bootstrap             = 1000,
                           standardize           = TRUE,
                           program               = c("all", "lavaan", "Mplus")){

  #### Error Checks ####
  if (is.null(data)){stop("Cannot detect valid data...")}
  if (!is.data.frame(data)){data <- as.data.frame(data)}
  if (is.null(id)){data <- cbind(data.frame(id = 1:nrow(data)), data); id <- "id"}
  if (is.null(x)){stop("No valid X variable named...")}
  if (is.null(y)){stop("No valid Y variable named...")}
  if (is.null(items)){stop("No valid item variables named...")}
  compositeType <- match.arg(compositeType)
  if (is.null(latentSpec)){latentSpec <- "both"}
  latentSpec <- match.arg(latentSpec)
  if (is.null(program)){program <- "both"}
  program <- match.arg(program)
  if (type == "product" | type == "ratio"){program <- "Mplus"}

  #### Organize Data ####
  dat <- data[,(names(data) %in% c(id, x, y, items))]
  temp <- data[,!(names(data) %in% c(id, x, y, items)), drop = FALSE]
  dat <- computeComposite(data      = dat,
                          items     = items,
                          type      = compositeType,
                          nickname  = compositeName)
  m <- names(dat[,!(names(dat) %in% c(id, x, y, items)), drop = FALSE])
  print(m)

  #### Fit Individual Item Mediation Models ####
  itemFitList <- itemMediation(data       = dat,
                               x          = x,
                               y          = y,
                               items      = items,
                               m          = m,
                               estimator  = estimator,
                               missing    = missing,
                               fixed.x    = fixed.x,
                               se         = se,
                               bootstrap  = bootstrap)

  itemParamTables <- tableParameters(fits         = itemFitList,
                                     standardize  = standardize)

  itemPlot <- plotItemMediation(paramTables  = itemParamTables,
                                m            = m)


  #### Generate Latent Variable Syntax ####
  if (latentSpec != "cfa"){
    composite_syntax <- generateLVSyntax(x            = x,
                                         y            = y,
                                         items        = items,
                                         m            = m,
                                         type         = compositeType,
                                         latentSpec   = "composite",
                                         estimator    = estimator,
                                         fixed.x      = fixed.x,
                                         se           = se,
                                         bootstrap    = bootstrap,
                                         program      = program)
  }

  if (latentSpec != "composite"){
    cfa_syntax <- generateLVSyntax(x            = x,
                                   y            = y,
                                   items        = items,
                                   m            = m,
                                   type         = compositeType,
                                   latentSpec   = "cfa",
                                   estimator    = estimator,
                                   fixed.x      = fixed.x,
                                   se           = se,
                                   bootstrap    = bootstrap,
                                   program      = program)
  }


  #### Fit Composite Models ####
  composite_fit <- NULL
  if ((program == "lavaan" | program == "both") &
      !grepl("Warning:", composite_syntax$lavaan_syntax)){
    composite_fit <- lavaan::sem(model      = composite_syntax$lavaan_syntax,
                                 data       = data,
                                 estimator  = estimator,
                                 missing    = missing,
                                 fixed.x    = fixed.x,
                                 se         = se,
                                 bootstrap  = bootstrap)
  }

  cfa_fit <- NULL
  if ((program == "lavaan" | program == "both") &
      !grepl("Warning:", cfa_syntax$lavaan_syntax)){
    cfa_fit <- lavaan::sem(model      = cfa_syntax$lavaan_syntax,
                           data       = data,
                           estimator  = estimator,
                           missing    = missing,
                           fixed.x    = fixed.x,
                           se         = se,
                           bootstrap  = bootstrap)
  }

  #### Generate Latent Variable Model Diagnostics ####




  #### Function Output ####
  return(list(data = cbind(dat, temp),
              itemFitList = itemFitList,
              itemParamTables = itemParamTables,
              itemPlot = itemPlot,
              syntax = if(latentSpec == "composite"){composite_syntax
                } else if (latentSpec == "cfa"){cfa_syntax
                } else {list(composite_syntax, cfa_syntax)
                },
              lvFits = if(!is.null(composite_fit) &
                          is.null(cfa_fit)){composite_fit
              } else if (is.null(composite_fit) &
                         !is.null(cfa_fit)){cfa_fit
              } else if (!is.null(composite_fit) &
                         !is.null(cfa_fit)){list(composite_fit, cfa_fit)
              } else {NULL
              },
              lvDiagnostics = NULL
              ))
}


# Temp for function checking
# data <- read.csv("data/depression-10items.csv")
# x = "X"; y = "Y"; items = names(data)[4:13]; compositeType = "mean"
# compositeName = "Nickname"
#output <- SEMinterrupted(data = data, x = "X", y = "Y", items = names(data)[4:13], compositeType = "mean", compositeName = "Nickname", standardize = FALSE)
