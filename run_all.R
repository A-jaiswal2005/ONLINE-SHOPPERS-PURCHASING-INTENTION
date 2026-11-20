# Run the whole pipeline, then launch the dashboard
source("01_preprocessing_eda.R")
source("02_data_mining.R")
source("03_modelling_evaluation.R")
shiny::runApp("app.R")
