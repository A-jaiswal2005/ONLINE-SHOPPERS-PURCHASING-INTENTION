# app.R - Interactive Shiny dashboard
# Run 01, 02 and 03 scripts first, then: shiny::runApp()
library(shiny); library(tidyverse); library(randomForest)

df       <- readRDS("output/clean_data.rds")
metrics  <- read.csv("output/model_metrics.csv")
roc_df   <- readRDS("output/roc_data.rds")
imp      <- readRDS("output/rf_importance.rds")
rf_model <- readRDS("output/rf_model.rds")
template <- readRDS("output/template_row.rds")
meds     <- readRDS("output/num_medians.rds")

num_vars <- names(df)[sapply(df, is.numeric)]
cat_vars <- c("Month", "VisitorType", "Weekend", "TrafficType", "Region", "Browser")

ui <- navbarPage("Online Shoppers Purchase Intention",

  tabPanel("Overview",
    fluidRow(
      column(3, wellPanel(h4("Sessions"), h3(nrow(df)))),
      column(3, wellPanel(h4("Purchases"), h3(sum(df$Revenue == "Yes")))),
      column(3, wellPanel(h4("Conversion rate"), h3(textOutput("conv", inline = TRUE)))),
      column(3, wellPanel(h4("Best model AUC"), h3(max(metrics$ROC_AUC))))),
    plotOutput("monthPlot", height = 350)),

  tabPanel("Explore",
    sidebarLayout(
      sidebarPanel(
        selectInput("cat", "Conversion rate by:", cat_vars),
        selectInput("num", "Numeric variable by purchase:", num_vars, "PageValues")),
      mainPanel(plotOutput("catPlot", height = 300), plotOutput("numPlot", height = 300)))),

  tabPanel("Model performance",
    tableOutput("metricsTbl"),
    fluidRow(column(6, plotOutput("rocPlot")), column(6, plotOutput("impPlot")))),

  tabPanel("Predict a session",
    sidebarLayout(
      sidebarPanel(
        sliderInput("pv", "PageValues", 0, 100, 0, step = 1),
        sliderInput("pr", "ProductRelated pages", 0, 200, 20),
        sliderInput("br", "BounceRates", 0, 0.2, 0.02, step = 0.005),
        sliderInput("er", "ExitRates", 0, 0.2, 0.04, step = 0.005),
        selectInput("mo", "Month", levels(df$Month), "Nov"),
        selectInput("vt", "Visitor type", levels(df$VisitorType), "Returning_Visitor")),
      mainPanel(h3("Purchase probability"), h1(textOutput("prob")),
                p("Other fields are held at typical (median/first-row) values.")))))

server <- function(input, output) {
  output$conv <- renderText(paste0(round(100 * mean(df$Revenue == "Yes"), 1), "%"))

  rate_plot <- function(v) df |> group_by(.data[[v]]) |>
    summarise(rate = mean(Revenue == "Yes"), .groups = "drop") |>
    ggplot(aes(.data[[v]], rate)) + geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::percent) + theme_minimal() +
    labs(y = "Conversion rate", title = paste("Conversion rate by", v))

  output$monthPlot <- renderPlot(rate_plot("Month"))
  output$catPlot   <- renderPlot(rate_plot(input$cat))
  output$numPlot   <- renderPlot(
    ggplot(df, aes(Revenue, log1p(.data[[input$num]]), fill = Revenue)) +
      geom_boxplot() + theme_minimal() + labs(y = paste0("log1p(", input$num, ")")))

  output$metricsTbl <- renderTable(metrics, digits = 4)
  output$rocPlot <- renderPlot(
    ggplot(roc_df, aes(FPR, TPR, colour = Model)) + geom_line(linewidth = 1) +
      geom_abline(linetype = "dashed") + theme_minimal() + labs(title = "ROC curves"))
  output$impPlot <- renderPlot(
    ggplot(head(imp, 10), aes(reorder(Feature, Importance), Importance)) +
      geom_col(fill = "darkgreen") + coord_flip() + theme_minimal() +
      labs(title = "Top features (Random Forest)", x = NULL))

  output$prob <- renderText({
    row <- template
    for (n in names(meds)) if (n %in% names(row)) row[[n]] <- meds[[n]]
    row$PageValues <- input$pv; row$ProductRelated <- input$pr
    row$BounceRates <- input$br; row$ExitRates <- input$er
    row$Month <- factor(input$mo, levels = levels(df$Month))
    row$VisitorType <- factor(input$vt, levels = levels(df$VisitorType))
    p <- predict(rf_model, row, type = "prob")[, "Yes"]
    paste0(round(100 * p, 1), "%")
  })
}

shinyApp(ui, server)
