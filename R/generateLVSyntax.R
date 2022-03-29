#' Generate Latent Variable Model Syntax
#' @param
#' @return Returns syntax objects for lavaan and Mplus.

generateLVSyntax <- function(x            = NULL,
                             y            = NULL,
                             items        = NULL,
                             m            = NULL,
                             type         = c("sum", "mean", "difference", "product", "ratio"),
                             latentSpec   = c("composite", "cfa"),
                             estimator    = "ML",
                             fixed.x      = FALSE,
                             se           = "standard",
                             bootstrap    = 1000,
                             program      = c("all", "lavaan", "Mplus")){

  # Error Checks #
  if (is.null(x)){stop("No valid X variable named...")}
  if (is.null(y)){stop("No valid Y variable named...")}
  if (is.null(items)){stop("No valid item variables named...")}
  if (is.null(m)){stop("No valid M variable named...")}
  type <- match.arg(type)
  latentSpec <- match.arg(latentSpec)
  program <- match.arg(program)

  # Make Variable Names Compatible with Mplus #
  mplus_items <- abbreviate(items, minlength = 4)
  mplus_m <- abbreviate(m, minlength = 4)
  inv <- ""
  lv_m <- paste0("lv", m)

  # Generate Measurement Model Syntax #
  if (type == "sum"){

    lavLVsyntax <- paste0(
      lv_m,
      ifelse(latentSpec == "composite", " =~ 1*", " =~ NA*"),
      paste(items, collapse = ifelse(
        latentSpec == "composite", " + 1*", " + ")),
      "\n", lv_m, " ~ 0*1",
      ifelse(latentSpec == "composite",
             paste0("\n", lv_m, " ~~ ", lv_m),
             paste0("\n", lv_m, " ~~ 1*", lv_m)))

    mplusLVsyntax <- paste0(
      mplus_m, " BY ",
      paste(mplus_items, collapse = ifelse(
        latentSpec == "composite", "@1 ", "* ")),
      ifelse(latentSpec == "composite", "@1; \n", "*; \n"),
      "\t[", mplus_m, "@0]; ",
      ifelse(latentSpec == "composite",
             paste0(mplus_m, ";"),
             paste0(mplus_m, "@1;")))

    mplusDefinesyntax <- ""

  } else if (type == "mean"){

    nItems <- length(items)

    lavLVsyntax <- paste0(
      lv_m,
      ifelse(latentSpec == "composite", paste0(" =~ (1/", nItems,")*"), " =~ NA*"),
      paste(items, collapse = ifelse(
        latentSpec == "composite", paste0(" + (1/", nItems,")*"), " + ")),
      "\n", lv_m, " ~ 0*1",
      ifelse(latentSpec == "composite",
             paste0("\n", lv_m, " ~~ ", lv_m),
             paste0("\n", lv_m, " ~~ 1*", lv_m)))

    mplusLVsyntax <- paste0(
      mplus_m, " BY ",
      paste(mplus_items, collapse = ifelse(
        latentSpec == "composite", paste0("@", round(1/nItems, 6) , " "), "* ")),
      ifelse(latentSpec == "composite",
                paste0("@", round(1/nItems, 6), "; \n"),
                "*; \n"),
      "\t[", mplus_m, "@0]; ",
      ifelse(latentSpec == "composite",
             paste0(mplus_m, ";"),
             paste0(mplus_m, "@1;")))

    mplusDefinesyntax <- ""

  } else if (type == "difference"){

    lavLVsyntax <- paste0(
      lv_m, " =~ 1*", items[1],
      "\n", items[1], " ~ 1*", items[2])

    mplusLVsyntax <- paste0(
      mplus_m, " BY ", mplus_items[1], "@1;\n",
      "\t", mplus_items[1], " ON ", mplus_items[2], "@1;")

    mplusDefinesyntax <- ""

  } else if (type == "product"){

    lavLVsyntax <- ""

    mplusLVsyntax <- paste0(
      mplus_m, " | ", mplus_items[1],
      " ON ", mplus_items[2], ";")

    mplusDefinesyntax <- ""

  } else if (type == "ratio"){

    lavLVsyntax <- ""

    inv <- paste0("i.", mplus_items[2])

    mplusLVsyntax <- paste0(
      mplus_m, " | ", mplus_items[1],
      " ON ", inv, ";")

    mplusDefinesyntax <- paste0(
      "DEFINE:\n",
      "\t", inv, " = (1 / ", mplus_items[2], ");\n\n")

  }

  # Generate Item Mediation Syntax #
  if (type == "sum" | type == "mean"){

    lavItemsyntax <- paste(
      sapply(items, function(item){
        paste0(item, ifelse(latentSpec == "composite",
                            " ~~ theta*",
                            " ~~ "), item)
      }),
      collapse = "\n")

    mplusItemsyntax <- paste(
      sapply(mplus_items, function(item){
        paste0("\t", item, ifelse(latentSpec == "composite",
                                  " (theta);",
                                  ";"))
      }),
      collapse = "\n")

  } else if (type == "difference"){

    # lavItemsyntax <- paste(
    #   sapply(items, function(item){
    #     paste0(item, " ~~ ", item)
    #   }),
    #   collapse = "\n")

    # mplusItemsyntax <- paste(
    #   sapply(mplus_items, function(item){
    #     paste0("\t", item, ";")
    #   }),
    #   collapse = "\n")

    lavItemsyntax <- paste0(
      items[2], " ~~ ", items[2], "\n"
    )

    mplusItemsyntax <- paste0(
      "\t", items[2], ";\n"
    )

  } else if (type == "product"){

    lavItemsyntax <- ""

    mplusItemsyntax <- paste(
      sapply(mplus_items, function(item){
        paste0("\t", item, ifelse(type == "composite",
                                  " (theta);",
                                  ";"))
      }),
      collapse = "\n")

  } else if (type == "ratio"){

    lavItemsyntax <- ""

    mplusItemsyntax <- paste(
      sapply(c(mplus_items[1], inv), function(item){
        paste0("\t", item, ifelse(type == "composite",
                                  " (theta);",
                                  ";"))
      }),
      collapse = "\n")

  }

  lavItemMedsyntax <- paste(
    sapply(items, function(item){
      paste0(item, " ~ 0*", x, "; ", y, " ~ 0*", item)
    }),
    collapse = "\n")

  if (type != "product" & type != "ratio"){

    lavaan_syntax <- paste0(
      "# Latent Variable\n",
      lavLVsyntax,

      "\n\n# Item Variables\n",
      lavItemsyntax,

      "\n\n# Constrain Item Mediations\n",
      lavItemMedsyntax,

      "\n\n# A Path\n",
      lv_m," ~ a*", x,

      "\n\n# B Path\n",
      y," ~ b*", lv_m,

      "\n\n# Direct Effect\n",
      y," ~ c*", x,

      "\n\n# Computed Parameters\n",
      "indirect := a*b\n",
      "total := c + (a*b)"
    )

  } else {
    lavaan_syntax <- paste0("Warning: Lavaan does not currently support the ability to model ",
                            type, "composites.Please see the Mplus syntax to run these models.")
  }

  mplus_syntax <- list()
  temp <- paste0(
    "TITLE: Latent Variable Mediation: ", mplus_m," (", type, ")\n\n",
    "DATA: FILE = ./path/to/file/mediation.dat;\n\n",
    "VARIABLE: \n",
    "\tNAMES = ", paste(c("id", x, y, mplus_items,
                          paste0("ov", mplus_m)),
                        collapse = " "), ";\n",
    "\tUSEVARIABLES = ",
    ifelse(type == "ratio",
           paste(c(x, y, mplus_items[1], inv),
                 collapse = " "),
           paste(c(x, y, mplus_items),
                 collapse = " ")), ";\n",
    "\tMISSING = .;\n\n",
    ifelse(type == "product" | type == "ratio",
           mplusDefinesyntax, ""),
    "ANALYSIS:\n",
    "\tESTIMATOR = ", estimator, ";",
    ifelse(se == "bootstrap", paste0("\n\tBOOTSTRAP = ", bootstrap, ";\n"), ""),
    ifelse(type != "product" | type != "ratio",
           "\n\tTYPE = RANDOM;\n\n", "\n\n"),
    "MODEL:\n",
    "\t", mplusLVsyntax, "\n\n",
    mplusItemsyntax, "\n\n",
    "\t", mplus_m, " ON ", x, " (a);\n",
    "\t", y, " ON ", mplus_m, " (b);\n",
    "\t", y, " ON ", x, " (c);\n",
    ifelse(fixed.x, paste0("\t", x,";\n\n"), "\n")
  )

  mplus_syntax$modifications <- paste0(
    temp,
    "OUTPUT:\n",
    "\tSTDYX;\n", "\tMODINDICES (ALL);"
  )

  mplus_syntax$indirect <- paste0(
    temp,
    "MODEL CONSTRAINT:\n",
    "\tnew(indirect total);\n",
    "\tindirect = a * b;\n",
    "\ttotal = c + (a * b);\n\n",
    "OUTPUT:\n",
    "\tSTDYX;"
  )

  return(if(program == "all"){list(lavaan_syntax = lavaan_syntax,
                                    mplus_syntax = mplus_syntax)
         } else if (program == "lavaan"){list(lavaan_syntax = lavaan_syntax)
         } else if (program == "Mplus"){list(mplus_syntax = mplus_syntax)})
}
