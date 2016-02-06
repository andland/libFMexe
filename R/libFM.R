#' libFM factorization machines
#'
#' @param train training data.frame, (sparse) matrix, or character vector
#' @param test testing data.frame, (sparse) matrix, or character vector
#' @param ... other, unused, arguments
#'
#' @details See the libFM manual, \url{http://www.libfm.org/libfm-1.42.manual.pdf},
#'  for details on the parameters.
#'
#'  For grouping, if specifying model with a formula, this should be a logical
#'  of whether to group levels of a factor variable. If specifying the model with
#'  a design matrix, this should be an integer vector of the same length as the
#'  number of columns in the design matix, where each integer specifies the group
#'  which the variable belongs to.
#'
#'  If the function is not working, make sure that the directory is in the PATH
#'  by running \code{Sys.getenv('PATH')}. It is assumed that the executable is named
#'  \code{libFM} on. You can verify that the executable is being found and works by
#'  running \code{system("libFM -help")}.
#'
#'  See the README on \url{https://github.com/andland/libFMexe} for some more
#'  information on installation.
#'
#' @examples
#' \dontrun{
#' data(movie_lens)
#' train_rows = sample.int(nrow(movie_lens), nrow(movie_lens) * 2 / 3)
#' train = movie_lens[train_rows, ]
#' test  = movie_lens[-train_rows, ]
#'
#' predFM = libFM(train, test, Rating ~ User + Movie,
#'                task = "r", dim = 10, iter = 500)
#'
#' mean((predFM - test$Rating)^2)
#'
#' # the same can be done slightly more slowly with sparse matrices
#'
#' train_mat = Matrix::sparse.model.matrix(Rating ~ User + Movie - 1, train)
#' test_mat = Matrix::sparse.model.matrix(Rating ~ User + Movie - 1, test)
#'
#' predFM = libFM(train_mat, test_mat, train$Rating, test$Rating,
#'                task = "r", dim = 10, iter = 500)
#'
#' mean((predFM - test$Rating)^2)
#' }
#'
#' @references
#' Steffen Rendle (2012): Factorization Machines with libFM, in ACM Trans.
#' Intell. Syst. Technol., 3(3), May.
#'
#' @return
#' A vector of the predicted values/probabilities
#' @export
libFM <- function(train, test, ...) {
  UseMethod("libFM")
}

#' @describeIn libFM
#'
#' @param formula formula of covariates included
#' @param validation validation data.frame, (sparse) matrix, or character vector used for
#'   adaptive SGD
#' @param grouping logical scalar or integer vector
#'
#' @export
libFM.data.frame <- function(train, test, formula, validation, grouping, ...) {
  if (!missing(grouping)) {
    if (!(is.logical(grouping) & length(grouping) == 1)) {
      stop("when specifying a model with a formula, grouping needs to be a logical")
    }
    if (grouping) {
      grouping = libFM_groups(formula, train)
      include_grouping = TRUE
    } else {
      include_grouping = FALSE
    }
  } else {
    include_grouping = TRUE
  }

  train = model_frame_libFM(formula, train)
  if (!inherits(test, "data.frame")) {
    stop("train is a data.frame but test is not")
  }
  test = model_frame_libFM(formula, test)
  if (!missing(validation)) {
    if (!inherits(validation, "data.frame")) {
      stop("train is a data.frame but validation is not")
    }
    validation = model_frame_libFM(formula, validation)
  }

  if (include_grouping) {
    libFM.default(train, test, validation = validation, grouping = grouping, ...)
  } else {
    libFM.default(train, test, grouping = grouping, ...)
  }
}

#' @describeIn libFM
#'
#' @param y_train,y_test,y_validation numeric vectors of responses when train,
#'   test, and validation are matrices
#'
#' @export
libFM.matrix <- function(train, test, y_train, y_test,
                         validation, y_validation, grouping, ...) {
  if (inherits(train, "matrix") & missing(y_train)) {
    stop("y_train is missing")
  }
  if (!inherits(test, "matrix")) {
    stop("train is a matrix but test is not")
  }
  if (ncol(train) != ncol(test)) {
    stop("train and test must have the same number of columns")
  }
  if (nrow(train) != length(y_train)) {
    stop("train and y_train must have the same number of observations")
  }
  if (!missing(y_test) && nrow(test) != length(y_test)) {
    stop("test and y_test must have the same number of observations")
  }

  if (!missing(grouping)) {
    # TODO: better check that integers and no missing groups
    if (!(is.numeric(grouping) & length(grouping) == ncol(train))) {
      stop("when specifying a model with a matrix, grouping must ",
           "be a numeric vector")
    }
  }

  train = matrix_libFM(train, y_train)
  test = matrix_libFM(test, y_test)
  if (!missing(validation)) {
    if (missing(y_validation)) {
      stop("validation argument present but y_validation is missing")
    }
    if (!inherits(validation, "matrix")) {
      stop("train is a matrix but validation is not")
    }
    if (ncol(train) != ncol(validation)) {
      stop("train and validation must have the same number of columns")
    }
    validation = matrix_libFM(validation, y_validation)
  }

  libFM.default(train, test, validation = validation, grouping = grouping, ...)
}

#' @describeIn libFM
#'
#' @export
libFM.dgCMatrix <- function(train, test, y_train, y_test,
                            validation, y_validation, grouping, ...) {
  if (missing(y_train)) {
    stop("y_train is missing")
  }
  if (!inherits(test, "sparseMatrix")) {
    stop("train is a sparse matrix but test is not")
  }
  if (ncol(train) != ncol(test)) {
    stop("train and test must have the same number of columns")
  }
  if (nrow(train) != length(y_train)) {
    stop("train and y_train must have the same number of observations")
  }
  if (!missing(y_test) && nrow(test) != length(y_test)) {
    stop("test and y_test must have the same number of observations")
  }

  if (!missing(grouping)) {
    # TODO: better check that integers and no missing groups
    if (!(is.numeric(grouping) & length(grouping) == ncol(train))) {
      stop("when specifying a model with a matrix, grouping must ",
           "be a numeric vector")
    }
  }

  train = sp_matrix_libFM(train, y_train)
  test = sp_matrix_libFM(test, y_test)
  if (!missing(validation)) {
    if (missing(y_validation)) {
      stop("validation argument present but y_validation is missing")
    }
    if (!inherits(validation, "sparseMatrix")) {
      stop("train is a sparse matrix but validation is not")
    }
    if (ncol(train) != ncol(validation)) {
      stop("train and validation must have the same number of columns")
    }
    validation = sp_matrix_libFM(validation, y_validation)
  }

  libFM.default(train, test, validation = validation, grouping = grouping, ...)
}


#' @describeIn libFM
#'
#' @param global_bias whether to include an overall/global bias term
#' @param variable_bias whether to include variable main effects/biases
#' @param dim dimension of the two-way interaction
#' @param task classifcation or regression
#' @param method learning method
#' @param init_stdev standard deviation used for initialization
#'  of 2-way factors
#' @param regular length 3 vector of regularization parameters for
#'  global bias, variable biases, and interactions, respectively. Used with
#'  SGD and ALS
#' @param learn_rate learning rate used for SGD and adaptive SGD
#' @param verbosity how much feedback to give
#' @param iter number of iterations
#' @param exe_loc location of libFM.exe executable (if not in the PATH)
#'
#' @export
libFM.default <- function(train, test, global_bias = TRUE, variable_bias = TRUE, dim = 8,
                task = c("c", "r"), method = c("mcmc", "sgd", "als", "sgda"),
                init_stdev = 0.1, regular = c(0, 0, 0), learn_rate = 0.1, validation,
                verbosity = 0, iter = 100, exe_loc, grouping, ...) {
  method = match.arg(method)
  task = match.arg(task)
  if (missing(exe_loc)) {
    libfm_exe = "libFM"
  } else {
    libfm_exe = paste0("\"", file.path(exe_loc, "libFM"), "\"")
  }

  # the following will give an error if it cannot find libFM
  tmp = system(libfm_exe, intern = TRUE)

  if (method %in% c("sgd", "als")) {
    if (method == "als" & !is.missing(grouping)) {
      if (!(length(regular) %in% c(1, 3, 1 + 2 * length(unique(grouping))))) {
        stop("With grouping, regular must be of either length 1, 3, or ",
             "1 + # of groups")
      }
    } else if (!(length(regular) %in% c(1, 3))) {
      stop("regular must be a scalar or a vector of length 3")
    }
    regular_txt = paste(regular, collapse = ",")
  } else {
    regular_txt = "0"
  }

  dim_txt = paste0(ifelse(global_bias, 1, 0), ",", ifelse(variable_bias, 1, 0), ",", dim)

  trainloc = paste0(tempfile(), "libFMtrain.txt")
  testloc = paste0(tempfile(), "libFMtest.txt")
  outloc = paste0(tempfile(), "out.txt")

  write.table(train, file = trainloc, col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(test, file = testloc, col.names = FALSE, row.names = FALSE, quote = FALSE)

  command = paste0(libfm_exe,
                   " -task ", task,
                   " -train ", trainloc,
                   " -test ", testloc,
                   " -method ", method,
                   " -init_stdev ", init_stdev,
                   " -verbosity ", verbosity,
                   " -out ", outloc,
                   " -iter ", iter,
                   " -dim ", dim_txt)
  if (method %in% c("sgd", "als")) {
    command = paste0(command,
                     " -regular \'", regular_txt, "\'")
  }
  if (method %in% c("sgd", "sgda")) {
    command = paste0(command,
                     " -learn_rate ", learn_rate)
  }
  if (method == "sgda") {
    if (!missing(validation)) {
      validloc = paste0(tempfile(), "libFMvalid.txt")
      write.table(validation, file = validloc, col.names = FALSE, row.names = FALSE, quote = FALSE)

      command = paste0(command,
                       " -validation ", validloc)
    } else {
      stop("With method = \"sgda\", you must have provide validation data")
    }
  }
  if (!missing(grouping)) {
    groupingloc = paste0(tempfile(), "libFMgroups.txt")
    write.table(sprintf("%i", grouping), file = groupingloc,
                col.names = FALSE, row.names = FALSE, quote = FALSE)

    command = paste0(command,
                     " -meta ", groupingloc)
  }

  out = system(command, intern = verbosity <= 0)

  pred_libFM = read.table(outloc, header = FALSE)$V1

  return(pred_libFM)
}
