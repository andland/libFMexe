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
  # TODO: check data is a data.frame
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


matrix.libFM <- function(matrix, y) {
  # TODO: check matrix is a matrix
  if (!missing(y)) {
    out.string = paste0(y)
  } else {
    warning("No response variable. Using constant 1 as response")
    out.string = paste0(rep(1, nrow(matrix)))
  }

  for (c in 1:ncol(matrix)) {
    out.string = ifelse(
      matrix[, c] != 0,
      paste0(out.string, " ", c, ":", matrix[, c]),
      out.string
    )
  }
  return(out.string)
}

libFM.groups <- function(formula, data) {
  # the independent variables should all be factors
  # data should be a data.frame
  # TODO: check data is a data.frame
  vars = attr(attr(terms(formula), "factors"), "dimnames")[[2]]
  # TODO: give length of 1 to non-factors
  groups = rep(
    0:(length(vars) - 1),
    as.numeric(apply(
      data[[vars]],
      2,
      function(x) nlevels(x)
    ))
  )
  return(groups)
}
