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
#' @importFrom dplyr filter mutate
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
                           compositeType         = c("sum","mean","difference", "product", "ratio"),
                           compositeName         = NULL,
                           estimator             = "ML",
                           missing               = "FIML",
                           fixed.x               = FALSE,
                           se                    = "standard",
                           bootstrap             = 1000,
                           standardize           = TRUE){

  #### Error Checks ####
  if (is.null(data)){stop("Cannot detect valid data...")}
  if (!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  if (is.null(id)){data <- cbind(data.frame(id = 1:nrow(data)), data); id <- "id"}
  if (is.null(x)){stop("No valid X variable named...")}
  if (is.null(y)){stop("No valid Y variable named...")}
  if (is.null(items)){stop("No valid item variables named...")}
  compositeType <- match.arg(compositeType)

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

  #### Function Output ####
  return(list(data = cbind(dat, temp),
              itemFitList = itemFitList,
              itemParamTables = itemParamTables,
              itemPlot = itemPlot))
}


# Temp for function checking
# data <- read.csv("data/depression-10items.csv")
# x = "X"; y = "Y"; items = names(data)[4:13]; compositeType = "mean"
# compositeName = "Nickname"
#output <- SEMinterrupted(data = data, x = "X", y = "Y", items = names(data)[4:13], compositeType = "mean", compositeName = "Nickname", standardize = FALSE)
