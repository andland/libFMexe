#' Cross validation for rank and initialization standard deviation
#' with libFM factorization machines
#'
#' @param x data.frame, (sparse) matrix, or character vector used for cross validation
#' @param ... arguments passed to libFM
#'
#' @details
#'   This function attempts to speed things up by converting all the data to libFM format
#'   first, so that there are not repetitive and computationally-costly data conversions.
#'
#'   If unspecified, \code{loss_function} will be mean squared error for regression or
#'   mean Bernoulli deviance for classification. You can specify your own function also.
#'   \code{loss_function} must be a function that takes two vector arguments and returns a scalar numeric.
#'   The first argument is the predicted value / probability and the second is the true value.
#'
#' @examples
#' \dontrun{
#' data(movie_lens)
#' mses = cv_libFM(movie_lens, Rating ~ User + Movie,
#'                 task = "r", dims = c(0, 5, 10), cv_verbosity = 1)
#' mses
#'
#' # with a sparse matrix
#' movie_mat = Matrix::sparse.model.matrix(Rating ~ User + Movie - 1, movie_lens)
#' mses = cv_libFM(movie_lens, Rating ~ User + Movie,
#'                 task = "r", dims = c(0, 5, 10), cv_verbosity = 1)
#' mses
#' }
#'
##' @return
#' A matrix of the cross validated \code{loss_function}. There are \code{length(dims)}
#'   rows and \code{length(init_stdevs)} columns.
#' @seealso \code{\link{libFM}}
#' @export
cv_libFM <- function(x, ...) {
  UseMethod("cv_libFM")
}

#' @describeIn cv_libFM
#'
#' @param formula formula of response and covariates included
#' @param validation validation data.frame, (sparse) matrix, or character vector used for
#'   adaptive SGD
#' @param grouping logical scalar or integer vector
#' @param cv_verbosity how much feedback to give on the progress of cross validation
#'
#' @export
cv_libFM.data.frame <- function(x, formula, validation, grouping, cv_verbosity = 0, ...) {
  if (!missing(grouping)) {
    if (!(is.logical(grouping) & length(grouping) == 1)) {
      stop("when specifying a model with a formula, grouping needs to be a logical")
    }
    if (grouping) {
      grouping = libFM_groups(formula, x)
      include_grouping = TRUE
    } else {
      include_grouping = FALSE
    }
  } else {
    include_grouping = TRUE
  }

  if (cv_verbosity > 0) {
    cat("Converting data to libFM format...\n")
  }

  x = model_frame_libFM(formula, x)

  if (!missing(validation)) {
    if (!inherits(validation, "data.frame")) {
      stop("x is a data.frame but validation is not")
    }
    validation = model_frame_libFM(formula, validation)
  }

  if (include_grouping) {
    cv_libFM.default(x, validation = validation, grouping = grouping,
                     cv_verbosity = cv_verbosity,...)
  } else {
    cv_libFM.default(x, validation = validation, cv_verbosity = cv_verbosity, ...)
  }
}

#' @describeIn cv_libFM
#'
#' @param y,y_validation numeric vectors of responses when train
#'   and validation are matrices
#'
#' @export
cv_libFM.matrix <- function(x, y, validation, y_validation, grouping, cv_verbosity = 0, ...) {
  if (inherits(x, "matrix") & missing(y)) {
    stop("y is missing")
  }
  if (nrow(x) != length(y)) {
    stop("x and y must have the same number of observations")
  }

  if (!missing(grouping)) {
    # TODO: better check that integers and no missing groups
    if (!(is.numeric(grouping) & length(grouping) == ncol(x))) {
      stop("when specifying a model with a matrix, grouping must ",
           "be a numeric vector")
    }
  }

  if (cv_verbosity > 0) {
    cat("Converting data to libFM format...\n")
  }

  x = matrix_libFM(x, y)

  if (!missing(validation)) {
    if (missing(y_validation)) {
      stop("validation argument present but y_validation is missing")
    }
    if (!inherits(validation, "matrix")) {
      stop("x is a matrix but validation is not")
    }
    if (ncol(x) != ncol(validation)) {
      stop("x and validation must have the same number of columns")
    }
    validation = matrix_libFM(validation, y_validation)
  }

  cv_libFM.default(x, validation = validation, grouping = grouping,
                   cv_verbosity = cv_verbosity, ...)
}

#' @describeIn cv_libFM
#'
#' @export
cv_libFM.dgCMatrix <- function(x, y, validation, y_validation, grouping, cv_verbosity = 0, ...) {
  if (missing(y)) {
    stop("y_train is missing")
  }
  if (nrow(x) != length(y)) {
    stop("x and y must have the same number of observations")
  }

  if (!missing(grouping)) {
    # TODO: better check that integers and no missing groups
    if (!(is.numeric(grouping) & length(grouping) == ncol(x))) {
      stop("when specifying a model with a matrix, grouping must ",
           "be a numeric vector")
    }
  }

  if (cv_verbosity > 0) {
    cat("Converting data to libFM format...\n")
  }

  x = sp_matrix_libFM(x, y)

  if (!missing(validation)) {
    if (missing(y_validation)) {
      stop("validation argument present but y_validation is missing")
    }
    if (!inherits(validation, "sparseMatrix")) {
      stop("x is a sparse matrix but validation is not")
    }
    if (ncol(x) != ncol(validation)) {
      stop("x and validation must have the same number of columns")
    }
    validation = sp_matrix_libFM(validation, y_validation)
  }

  cv_libFM.default(x, validation = validation, grouping = grouping,
                   cv_verbosity = cv_verbosity, ...)
}

#' @describeIn cv_libFM
#'
#' @param dims vector of the two-way interaction dimensions to try
#' @param init_stdevs vector of the initialization standard deviations to try
#' @param folds number of cross validation folds or integer vector giving the
#'   fold ids for each row
#' @param task classifcation or regression
#' @param loss_function loss function to evaluate. See details for more information
#'
#' @export
cv_libFM.default <- function(x, dims = c(0, 8), init_stdevs = 0.1, folds = 5,
                             validation, grouping, task = c("c", "r"), loss_function,
                             cv_verbosity = 0, ...) {
  task = match.arg(task)
  n = length(x)
  if (length(folds) > 1) {
    # does this work if factor?
    if (length(unique(folds)) <= 1) {
      stop("If inputing CV split, must be more than one level")
    }
    if (length(folds) != n) {
      stop("if folds is a vector, it should be of same length as nrow(x)")
    }
    fold_ids = folds
  } else {
    fold_ids = sample(1:folds, n, replace = TRUE)
  }

  # retrieve the responses
  labels = vapply(strsplit(x, " "), function(xxx) as.numeric(xxx[1]), 0.0)

  if (missing(loss_function)) {
    if (task == "r") {
      loss_function <- function(pred, truth) {
        mean((pred - truth)^2)
      }
    } else {
      loss_function <- function(pred, truth) {
        - 2 * (sum(log(pred)[truth == 1]) +
                 sum(log(1 - pred)[truth == 0])) / length(truth)
      }
    }
  }

  results = matrix(0, length(dims), length(init_stdevs),
                   dimnames = list(dim = dims, inti_stdev = init_stdevs))

  for (d in dims) {
    if (cv_verbosity > 0) {
      cat("dim", which(d == unique(dims)), "of", length(unique(dims)), "")
    }
    for (sd in init_stdevs) {
      if (cv_verbosity > 0) {
        cat(".")
      }
      pred_cv = rep(NA, n)
      for (cv in unique(fold_ids)) {
        pred_cv[fold_ids == cv] = libFM.default(x[fold_ids != cv], x[fold_ids == cv],
                             dim = d, init_stdev = sd, validation = validation,
                             grouping = grouping, task = task, ...)
      }
      results[d == dims, sd == init_stdevs] =
        loss_function(pred_cv, labels)
      if (cv_verbosity > 0) {
        cat("\n")
      }
    }
  }
  results
}
