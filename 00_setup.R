# 00_setup.R - install & load packages (run once)
pkgs <- c("tidyverse", "randomForest", "pROC", "rpart", "rpart.plot",
          "arules", "shiny")
new <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))
dir.create("output", showWarnings = FALSE)
