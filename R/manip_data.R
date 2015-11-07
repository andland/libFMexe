#' Converts a data frame into a format digestable for libFM
#'
#' This can only deal with factors as covariate.
#' It is important to make sure your covariates are all factors (not indicators),
#' or else it will not work correctly
#'
#' @param formula the formula with the response on the left hand side
#'   and the covariates on the right hand side
#' @param data data.frame
#' @param ... optional, currently unused, arguments
#'
#' @return a character vector with one value per observation
#' @export
model.frame.libFM <- function(formula, data, ...) {
  # the independent variables should all be factors
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data.frame")
  }
  is.response = attr(terms(formula), "response")
  if (is.response) {
    response_name = attr(attr(terms(formula), "factors"), "dimnames")[[1]][1]
    if (any(colnames(data) == response_name)) {
      out.string = paste0(data[[response_name]])
    } else {
      warning("Response variable not in dataset. Using constant 1 as response.")
      out.string = paste0(rep(1, nrow(data)))
    }
  } else {
    warning("No response variable. Using constant 1 as response")
    out.string = paste0(rep(1, nrow(data)))
  }

  vars = attr(attr(terms(formula), "factors"), "dimnames")[[2]]
  col.num = 0
  for (var in vars) {
    out.string = paste0(out.string, " ", col.num + as.numeric(data[[var]]) - 1, ":", 1)
    col.num = col.num + nlevels(data[[var]])
  }
  return(out.string)
}


matrix.libFM <- function(mat, y) {
  if (!("matrix" %in% class(mat) || inherits(mat, "sparseMatrix"))) {
    stop("mat must be an object of class \"matrix\" or \"Matrix\"")
  }
  if (!missing(y)) {
    out.string = paste0(y)
  } else {
    warning("No response variable. Using constant 1 as response")
    out.string = paste0(rep(1, nrow(mat)))
  }

  for (c in 1:ncol(mat)) {
    out.string = ifelse(
      mat[, c] != 0,
      paste0(out.string, " ", c, ":", mat[, c]),
      out.string
    )
  }
  return(out.string)
}

libFM.groups <- function(formula, data) {
  # data should be a data.frame
  if (!("data.frame" %in% class(data))) {
    stop("data must be a data.frame")
  }

  vars = attr(attr(terms(formula), "factors"), "dimnames")[[2]]
  vars_char = vapply(
    1:length(vars),
    function(x)
      !is.factor(data[[vars[x]]]) & is.character(data[[vars[x]]]),
    TRUE
  )
  if (any(vars_char)) {
    warning(sum(vars_char), " variable(s) have character class and are not factors.\n",
            "This can cause issues if not all of the levels are present in a subset ",
            "of the data.")
  }

  group_lengths = vapply(
    1:length(vars),
    function(x) {
      ifelse(
        is.factor(data[[vars[x]]]),
        nlevels(data[[vars[x]]]),
        ifelse(
          is.character(data[[vars[x]]]),
          length(unique(data[[vars[x]]])),
          1L
        )
      )
    },
    1L
  )

  groups = rep(0:(length(vars) - 1), group_lengths)

  return(groups)
}
