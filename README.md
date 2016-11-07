# Motsai
Motsai Utilities Package


An R package for the analysis conducted by Sultan Wehaibi from Concordia University

## Instructions

#### For Mac and Linux:

* Install the release version of devtools from CRAN with

```{r}
install.packages("devtools")
```

* Install motsai package via the following command

```{r}
devtools::install_github("xsultan/activity_recognition")
```

#### For Windows:

Follow the same steps above, if you experience the following error: 
```{r}
Warning in install.packages :
 'lib = "C:/Program Files/R/R-X.X.X/library"' is not writable
```

Run the following code:

```{r}
setwd(dir.create("packages_to_install"))
pathOfPackages <- paste(getwd(), "packages_to_install", sep="\\")
download.package(c("nlme", "lme4"), destdir=pathOfPackages, type="win.binary")
setwd(pathOfPackages)
install.packages(list.files(), repos=NULL, type="win.binary", lib=Sys.getenv("R_LIBS_USER"))
install.packages(c("minqa", "abind", "coda", "nloptr")

```

Now try to install the motsai package using the following command:

```{r}
devtools::install_github("xsultan/activity_recognition")
```
