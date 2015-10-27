#' Converts a data frame into a format digestable for libFM
#'
#' This can only deal with factors as covariate.
#' It is important to make sure your covariates are all factors (not indicators),
#' or else it will not work correctly
#'
#' @param data data.frame
#' @param formula
#'
#' @return a character vector with one value per observation
#' @export
libFM.model.frame <- function(data, formula) {
  # the independent variables should all be factors
  is.response = attr(terms(formula), "response")
  if (is.response) {
    response_name = attr(attr(terms(formula), "factors"), "dimnames")[[1]][1]
    if (any(colnames(data) == response_name)) {
      out.string = paste0(data[, response_name])
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
    out.string = paste0(out.string, " ", col.num + as.numeric(data[, var]) - 1, ":", 1)
    col.num = col.num + nlevels(data[, var])
  }
  return(out.string)
}

#' libFM factorization machines
#'
#' @param train training data
#' @param test testing data
#' @param formula formula of covariates included. They must all be factors
#' @param global_bias whether to include an overall/global bias term
#' @param variable_bias whether to include variable main effects/biases
#' @param dim dimension of the two-way interaction
#' @param task classifcation or regression
#' @param method learning method
#' @param init_stdev standard deviation used for initialization
#'  of 2-way factors
#' @param verbosity how much feedback to give
#' @param iter number of iterations
#' @param exe_loc location of libfm.exe executable (if not in the PATH)
#'
#' @details See the libFM manual, \url{http://www.libfm.org/libfm-1.42.manual.pdf},
#'  for details on the parameters.
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
                init_stdev = 0.1, verbosity = 0, iter = 100, exe_loc) {
  method = match.arg(method)
  task = match.arg(task)
  if (missing(exe_loc)) {
    if (.Platform$OS.type == "windows") {
      libfm_exe = "libfm"
    } else {
      libfm_exe = "libFM"
    }
  } else {
    libfm_exe = paste0("\"", file.path(exe_loc, "libfm"), "\"")
  }

  # the following will give an error if it cannot find libFM
  tmp = system(libfm_exe, intern = TRUE)

  dim_txt = paste0(ifelse(global_bias, 1, 0), ",", ifelse(variable_bias, 1, 0), ",", dim)

  trainloc = paste0(tempfile(), "libFMtrain.txt")
  testloc = paste0(tempfile(), "libFMtest.txt")
  outloc = paste0(tempfile(), "out.txt")

  train_libFM = libFM.model.frame(train, formula)
  test_libFM = libFM.model.frame(test, formula)

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
  out = system(command, intern = TRUE)
  if (verbosity > 0) print(out)

  pred_libFM = read.table(outloc, header = FALSE)$V1

  return(pred_libFM)
}


# don't use the following functions ---------------------------------------

# libFM.groups <- function(formula) {
#   # the independent variables should all be factors
#   vars=attr(attr(terms(formula),"factors"),"dimnames")[[2]]
#   groups=rep(0:(length(vars)-1),as.numeric(apply(train.and.test[,vars],2,function(x) length(unique(x)))))
#   write.table(groups,file="U:/libFM/libfm-1.30.src/bin/groups.txt",col.names=F,row.names=F,quote=FALSE)
# }
#
# libFM.matrix <- function(matrix, y) {
#   if (!missing(y)) {
#     out.string = paste0(y)
#   } else {
#     warning("No response variable. Using constant 1 as response")
#     out.string = paste0(rep(1, nrow(matrix)))
#   }
#
#   col.num.mat = matrix(1:ncol(matrix), nrow(matrix), ncol(matrix), byrow = TRUE)
#   for (c in 1:ncol(matrix)) {
#     out.string = ifelse(matrix[, c] != 0, paste0(out.string, " ", c, ":", 1), out.string)
#   }
#   return(out.string)
# }
