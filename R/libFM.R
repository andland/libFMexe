#' libFM factorization machines
#'
#' @param train training data.frame
#' @param test testing data.frame
#' @param formula formula of covariates included. They must all be factors
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
#' @param validation validation data.frame used for adaptive SGD
#' @param verbosity how much feedback to give
#' @param iter number of iterations
#' @param exe_loc location of libfm.exe executable (if not in the PATH)
#'
#' @details See the libFM manual, \url{http://www.libfm.org/libfm-1.42.manual.pdf},
#'  for details on the parameters.
#'
#'  If the function is not working, make sure that the directory is in the PATH
#'  by running \code{Sys.getenv('PATH')}. It is assumed that the executable is named
#'  \code{libFM} on. You can verify that the executable is being found and works by
#'  running either \code{system("libFM -help")}.
#'
#'  See the README on \url{https://github.com/andland/libFMexe} for some more
#'  information.
#'
#' @examples
#' \dontrun{
#' data(movie_lense)
#' train_rows = sample.int(nrow(movie_lense), nrow(movie_lense) * 2 / 3)
#' train = movie_lense[train_rows, ]
#' test  = movie_lense[-train_rows, ]
#'
#' predFM = libFM(train, test, Rating ~ User + Movie,
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
libFM <- function(train, test, formula, global_bias = TRUE, variable_bias = TRUE, dim = 8,
                task = c("c", "r"), method = c("mcmc", "sgd", "als", "sgda"),
                init_stdev = 0.1, regular = c(0, 0, 0), learn_rate = 0.1, validation,
                verbosity = 0, iter = 100, exe_loc) {
  method = match.arg(method)
  task = match.arg(task)
  if (missing(exe_loc)) {
#     if (.Platform$OS.type == "windows") {
#       libfm_exe = "libfm"
#     } else {
#       libfm_exe = "libFM"
#     }
    libfm_exe = "libFM"
  } else {
    libfm_exe = paste0("\"", file.path(exe_loc, "libfm"), "\"")
  }

  # the following will give an error if it cannot find libFM
  tmp = system(libfm_exe, intern = TRUE)

  if (method %in% c("sgd", "als")) {
    if (!(length(regular) %in% c(1, 3))) {
      stop("regular must be a scalar or vector of length 3")
    }
    if (length(regular) == 1) {
      regular = rep(regular, 3)
    }
    regular_txt = paste(regular, collapse = ",")
  } else {
    regular_txt = "0,0,0"
  }

  dim_txt = paste0(ifelse(global_bias, 1, 0), ",", ifelse(variable_bias, 1, 0), ",", dim)

  trainloc = paste0(tempfile(), "libFMtrain.txt")
  testloc = paste0(tempfile(), "libFMtest.txt")
  outloc = paste0(tempfile(), "out.txt")

  train_libFM = model.frame.libFM(formula, train)
  test_libFM = model.frame.libFM(formula, test)

  write.table(train_libFM, file = trainloc, col.names = FALSE, row.names = FALSE, quote = FALSE)
  write.table(test_libFM, file = testloc, col.names = FALSE, row.names = FALSE, quote = FALSE)

  command = paste0(libfm_exe,
                   " -task ", task,
                   " -train ", trainloc,
                   " -test ", testloc,
                   " -method ", method,
                   " -init_stdev ", init_stdev,
                   " -verbosity ", verbosity,
                   " -out ", outloc,
                   " -iter ", iter,
                   " -dim \'", dim_txt, "\'")
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
      valid_libFM = model.frame.libFM(formula, validation)
      write.table(valid_libFM, file = validloc, col.names = FALSE, row.names = FALSE, quote = FALSE)

      command = paste0(command,
                       " -validation ", validloc)
    } else {
      stop("with method = \"sgda\", you must have a validation data.frame")
    }
  }

  out = system(command, intern = verbosity <= 0)

  pred_libFM = read.table(outloc, header = FALSE)$V1

  return(pred_libFM)
}
