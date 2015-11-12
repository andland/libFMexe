#' Cross validation for libFM factorization machines
#'
#' @param x data.frame, matrix, or character vector used for cross validation
#' @param ... arguments passed to libFM
#'
#' @details
#'  This function attempts to speed things up by converting data to libFM format
#'  first, and then
#'
#'   If unspecified, loss_function will be sum of squared error for regression or
#'   Bernoulli deviance for classification. You can specify your own function also.
#'   loss_function must be a function that takes two vector arguments and returns a scalar numeric.
#'   The first argument is the predicted value / probability and the second is the true value.
#'
#' @examples
#' \dontrun{
#' data(movie_lens)
#' sses = cv_libFM(movie_lens, Rating ~ User + Movie, task = "r", dims = c(0, 5, 10))
#' sses / nrow(movie_lens)
#' }
#'
##' @return
#' A vector of the predicted values/probabilities
#' @seealso \code{\link{libFM}}
#' @export
cv_libFM <- function(x, ...) {
  UseMethod("cv_libFM")
}

#' @describeIn cv_libFM
#'
#' @param formula formula of response and covariates included
#' @param validation validation data.frame, matrix, or character vector used for
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
    cv_libFM.default(x, grouping = grouping, cv_verbosity = cv_verbosity, ...)
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
    validation = matrix_libFM(validation, y_validation)
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
  if (length(folds) > 1) {
    # does this work if factor?
    if (length(unique(folds)) <= 1) {
      stop("If inputing CV split, must be more than one level")
    }
    if (length(folds) != length(x)) {
      stop("if folds is a vector, it should be of same length as nrow(x)")
    }
    fold_ids = folds
  } else {
    fold_ids = sample(1:folds, length(x), replace = TRUE)
  }

  # retrieve the responses
  labels = vapply(strsplit(x, " "), function(xxx) as.numeric(xxx[1]), 0.0)

  if (missing(loss_function)) {
    if (task == "r") {
      loss_function <- function(pred, truth) {
        sum((pred - truth)^2)
      }
    } else {
      loss_function <- function(pred, truth) {
        - 2 * (sum(log(pred)[truth == 1]) +
                 sum(log(1 - pred)[truth == 0]))
      }
    }
  }

  results = matrix(0, length(dims), length(init_stdevs),
                   dimnames = list(dim = dims, inti_stdev = init_stdevs))
  for (cv in unique(fold_ids)) {
    if (cv_verbosity > 0) {
      cat("fold", which(cv == unique(fold_ids)), "of", length(unique(fold_ids)), "\n")
    }
    for (d in dims) {
      for (sd in init_stdevs) {
        pred_cv = libFM.default(x[fold_ids != cv], x[fold_ids == cv],
                             dim = d, init_stdev = sd, validation = validation,
                             grouping = grouping, task = task, ...)
        results[d == dims, sd == init_stdevs] =
          results[d == dims, sd == init_stdevs] +
          loss_function(pred_cv, labels[fold_ids == cv])
      }
    }
  }
  results
}
