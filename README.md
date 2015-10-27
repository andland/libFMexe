# R Wrapper for the libFM Executable

This package provides a rough interface to the [libFM](http://www.libfm.org/) library on Windows with [R](https://www.r-project.org/). It does this in two ways:

* The `libFM.model.frame()` function converts data into libFM (also LIBSVD) format
* It calls the libFM Windows executable with your data and returns the resulting prediction

## Installing

### Installing libFM executable

#### Windows

First, you will need to [download the libFM windows executable](http://www.libfm.org/#download) and save it on your computer, for example in `C:\libFM`. 

Then, it is recommended that you add the directory that you saved libFM in to you system path. [This webpage](https://msdn.microsoft.com/en-us/library/office/ee537574(v=office.14).aspx) provides one way to do that. If you do not or cannot do that, you can enter the directory as `exe_loc` argument into the `libFM()` function, for example `libFM(..., exe_loc = "C:\\libFM")`.

You can verify that the path contains the libFM directory by running `Sys.getenv('PATH')` and you can verify that the program works by running `system("libfm -help")`.

#### Mac / Linux



### Installing libFMwin R package

With the `devtools` package, you can install this package by running the following.

```r
# install.package("devtools")
devtools::install_github("andland/libFMwin")
```

## Using libFMwin

The support for `libFM` is still fairly limited, but this package allows you to quickly iterate on a number of models. 

libFM does well with categorical variables with lots of levels. The canonical example is for collaborative filtering, where there is one categorical variable for the users and one for the items. For now, I have only included support for covariates that factor variables.

The main advantage of this package is being able to try many different models. For example, trying different combinations of variables, trying different `dim` of the two-way interactions, or trying different `init_stdev`.

```r
library(libFMwin)

data(movie_lense)
train_rows = sample.int(nrow(movie_lense), nrow(movie_lense) * 2 / 3)

train = movie_lense[train_rows, ]
test  = movie_lense[-train_rows, ]

predFM = libFM(train, test, Rating ~ User + Movie,
               task = "r", dim = 10, iter = 500)

mean((predFM - test$Rating)^2)

# compare to ridge regression, which cannot model interactions
library(glmnet)

spmat = sparse.model.matrix(Rating ~ User + Movie, data = movie_lense)
trainsp = spmat[train_rows, ]
testsp = spmat[-train_rows, ]

mod = cv.glmnet(x = trainsp, y = movie_lense$Rating[train_rows], alpha = 0)
predRR = predict(mod, testsp, s = "lambda.min")

mean((predRR - test$Rating)^2)

# For comparison, dim = 0 is basically the same as ridge regression
predFM_RR = libFM(train, test, Rating ~ User + Movie,
                  task = "r", dim = 0, iter = 100)

mean((predFM_RR - test$Rating)^2)
```

## Improving this package

[c++ source code](https://github.com/srendle/libfm) is available for libFM. Let me know if you would like to help build an implementation that calls the source code.
