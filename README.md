# R Wrapper for the libFM Executable

This package provides a rough interface to the [libFM](http://www.libfm.org/) library on Windows with [R](https://www.r-project.org/). It does this in two ways:

* The `libFM.model.frame()` function converts data into libFM (also LIBSVD) format
* It calls the libFM Windows executable with your data and returns the resulting prediction

## Installing

### Installing libFM executable

First, you will need to [download the libFM windows executable](http://www.libfm.org/#download) and save it on your computer, for example in `C:\libFM`. 

Then, it is recommended that you add the directory that you saved libFM in to you system path. [This webpage](https://msdn.microsoft.com/en-us/library/office/ee537574(v=office.14).aspx) provides one way to do that. If you do not or cannot do that, you can enter the directory as `exe_loc` argument into the `libFM()` function, for example `libFM(..., exe_loc = "C:\\libFM")`.

### Installing libFMwin R package

With the `devtools` package, you can install this package by running the following.

```r
# install.package("devtools")
devtools::install_github("andland/libFMwin")
```

## Using libFMwin

The support for `libFM` is still fairly limited, but this package allows you to quickly iterate on a number of models. 

libFM does well with categorical variables with lots of levels. The canonical example is for collaborative filtering, where there is one categorical variable for the users and one for the items. For now, I have only included support for covariates that factor variables.

```r
data(car, package = "Rmixmod")
car$acceptable = ifelse(car$acceptability == "unacc", 0, 1)
train_rows = sample.int(nrow(car), nrow(car) * 2 / 3)

train = car[train_rows, ]
test = car[-train_rows, ]

library(libFMwin)
predFM = libFM(train, test, 
             acceptable ~ buying + maint + doors + persons + lug_boot + safety,
             task = "c", dim = 8)

mod = glm(acceptable ~ buying + maint + doors + persons + lug_boot + safety, data = train, family = binomial)
predGLM = predict(mod, test, type = "response")

logithistplot(data.frame(predFM, test$acceptable))
logithistplot(data.frame(predGLM, test$acceptable))
```
