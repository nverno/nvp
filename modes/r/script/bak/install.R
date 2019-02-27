#!/usr/bin/env Rscript
                                              
## packages to install
packs <- c("devtools", "testthat", "roxygen2",  #dev
           "rio",                               #data import
           "data.table",                        #data manip
           "ggplot2", "gridExtra",              #graphics
           "MASS", "glmnet",                    #basic models
           "e1071",                             #svm, tune
           "leaps",                             #regsubsets
           "caret",                             #general ML tools
           "neuralnet",                         #deep learn
           "randomForest",                      #random forests
           "class"                              #KNN
           )

## install from github
ghub <- c("nverno/datadoc", "nverno/sync.afs", "nverno/rstuff", "rstudio/rmarkdown")

installedp <- function(pack) {
    pack %in% rownames(installed.packages())
}

for (pack in packs) {
    if (!installedp(pack))
        install.packages(pack, dependencies=TRUE)
}

if (require(devtools)) {
    for (pack in ghub) {
        p <- sub("[^/]+/", "", pack)       #trim username from github package
        if (!installedp(p))
            install_github(pack)
    }
}
