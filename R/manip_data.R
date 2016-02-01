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
#'
#' @details If your data is sparse, \code{sp_matrix_libFM} is about 100 times faster than
#'   \code{matrix_libFM}. I recommend using the sparse version over the standard version
#'   whenever possible. If your data consists of factor variables with a lot of levels,
#'   \code{model_frame_libFM} is faster than \code{sp_matrix_libFM}.
#'
#' @examples
#' data(movie_lens)
#'
#' movie_lens_libFM = model_frame_libFM(Rating ~ User + Movie, movie_lens)
#' tail(movie_lens_libFM, 10)
#'
#' @seealso \code{\link{sp_matrix_libFM}}, \code{\link{matrix_libFM}}
#' @export
model_frame_libFM <- function(formula, data, ...) {
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
    var_data = data[[var]]
    if (is.factor(var_data) || is.character(var_data)) {
      if (!is.factor(var_data)) {
        warning(var, " has character class and is not a factor.\n",
                "This can cause issues if not all of the levels are present in a subset ",
                "of the data.")
        var_data = factor(var_data)
      }
      out.string = paste0(out.string, " ", sprintf("%i", col.num + as.numeric(var_data) - 1), ":", 1)
      col.num = col.num + nlevels(var_data)
    } else if (is.numeric(var_data) | is.logical(var_data)) {
      out.string = paste0(out.string, " ", sprintf("%i", col.num), ":", as.numeric(var_data))
      col.num = col.num + 1
    } else {
      stop(var, " has an unknown variable type.")
    }
  }
  return(out.string)
}

#' Converts a matrix to libFM format
#'
#' @param mat matrix to be converted. Can either be a standard matrix
#'   or a sparse matrix
#' @param y The response variable. If unavailable (for test data), a
#'   1 is used because libFM requires a response.
#'
#' @return a character vector with one value per observation
#'
#' @details If your data is sparse, \code{sp_matrix_libFM} is about 100 times faster than
#'   \code{matrix_libFM}. I recommend using the sparse version over the standard version
#'   whenever possible. If your data consists of factor variables with a lot of levels,
#'   \code{model_frame_libFM} is faster than \code{sp_matrix_libFM}.
#'
#' @examples
#' data(movie_lens)
#' movie_lens_sub = tail(movie_lens, 10)
#'
#' # model.matrix will remove the reference level
#' # which may not be desireable is some situations
#' movie_lens_mm = model.matrix(Rating ~ User + Movie - 1, data = movie_lens_sub)
#'
#' movie_lens_libFM = matrix_libFM(movie_lens_mm, movie_lens_sub$Rating)
#' movie_lens_libFM
#'
#' @seealso \code{\link{sp_matrix_libFM}}, \code{\link{model_frame_libFM}}
#' @export
matrix_libFM <- function(mat, y) {
  if (!("matrix" %in% class(mat) || inherits(mat, "sparseMatrix"))) {
    stop("mat must be an object of class \"matrix\" or \"Matrix\"")
  }
  if (inherits(mat, "sparseMatrix")) {
    warning("You are using a sparseMatrix. It is highly recommended that you use ",
            "sp_matrix_libFM() instead.")
  }
  if ("matrix" %in% class(mat) & !is.numeric(mat)) {
    stop("mat must be a numeric matrix")
  }
  if (!missing(y)) {
    if (length(y) != nrow(mat) & length(y) != 1) {
      stop("length of y does not match the number of rows in mat")
    } else if (length(y) == 1) {
      out.string = paste0(rep(y, nrow(mat)))
    } else {
      out.string = paste0(y)
    }
  } else {
    warning("No response variable. Using constant 1 as response")
    out.string = paste0(rep(1, nrow(mat)))
  }

  for (c in 1:ncol(mat)) {
    out.string = ifelse(
      mat[, c] != 0,
      paste0(out.string, " ", sprintf("%i", c - 1), ":", mat[, c]),
      out.string
    )
  }
  names(out.string) <- NULL
  return(out.string)
}

#' Converts a sparse matrix to libFM format
#'
#' @param mat sparse matrix to be converted. Must be a sparse matrix
#'   from the package \code{Matrix}
#' @param y The response variable. If unavailable (for test data), a
#'   1 is used because libFM requires a response.
#'
#' @return a character vector with one value per observation
#'
#' @details If your data is sparse, \code{sp_matrix_libFM} is about 100 times faster than
#'   \code{matrix_libFM}. I recommend using the sparse version over the standard version
#'   whenever possible. If your data consists of factor variables with a lot of levels,
#'   \code{model_frame_libFM} is faster than \code{sp_matrix_libFM}.
#'
#' @examples
#' data(movie_lens)
#' movie_lens_sub = tail(movie_lens, 1000)
#'
#' # model.matrix will remove the reference level
#' # which may not be desireable is some situations
#' movie_lens_mm = Matrix::sparse.model.matrix(Rating ~ User + Movie - 1, data = movie_lens_sub)
#'
#' movie_lens_libFM = sp_matrix_libFM(movie_lens_mm, movie_lens_sub$Rating)
#' head(movie_lens_libFM)
#'
#' @seealso \code{\link{model_frame_libFM}}, \code{\link{matrix_libFM}}
#' @export
sp_matrix_libFM <- function(mat, y) {
  if (!inherits(mat, "sparseMatrix")) {
    stop("mat must be a sparse matrix from the package Matrix")
  }
  
  if (!missing(y)) {
    if (length(y) != nrow(mat) & length(y) != 1) {
      stop("length of y does not match the number of rows in mat")
    }
  } else {
    warning("No response variable. Using constant 1 as response")
    y = 1
  }
  
  tuples = Matrix::summary(mat)
  tt = dplyr::summarize(
    dplyr::group_by_(tuples, "i"),
    tup = paste(
      paste0(sprintf("%i", j - 1), ":", x),
      collapse = " "
    )
  )

  out.string = paste(y, tt[["tup"]])
  names(out.string) <- NULL
  return(out.string)
}

#' Extract grouping of variables based on formula
#'
#' @param formula the formula with the response on the left hand side
#'   and the covariates on the right hand side
#' @param data data.frame
#'
#' @return vector of integers with group labels of the variables
#' @export
libFM_groups <- function(formula, data) {
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
