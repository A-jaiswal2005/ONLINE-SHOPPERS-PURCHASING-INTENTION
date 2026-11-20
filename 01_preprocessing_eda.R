# 01_preprocessing_eda.R
# Steps 1-4: acquisition, understanding, preprocessing, EDA
source("00_setup.R")

# ---- 1. Dataset acquisition ----
# UCI: https://archive.ics.uci.edu/dataset/468/online+shoppers+purchasing+intention+dataset
# (already placed in data/)
raw <- read.csv("data/online_shoppers_intention.csv", stringsAsFactors = FALSE)

# ---- 2. Data understanding ----
dim(raw)
str(raw)
summary(raw)
cat("Missing values:", sum(is.na(raw)), "\n")
cat("Duplicate rows:", sum(duplicated(raw)), "\n")
print(prop.table(table(raw$Revenue)))   # class imbalance (~15% buy)

# ---- 3. Preprocessing ----
df <- raw |>
  distinct() |>                                   # remove duplicate rows
  drop_na() |>                                    # (none expected)
  mutate(
    Month = factor(Month, levels = c("Feb","Mar","May","June","Jul",
                                     "Aug","Sep","Oct","Nov","Dec")),
    VisitorType = factor(VisitorType),
    Weekend = factor(ifelse(Weekend, "Yes", "No"), levels = c("No","Yes")),
    # numeric codes that are really categories -> factors,
    # rare levels (<2%) lumped into "Other" so train/test levels match
    OperatingSystems = fct_lump_prop(factor(OperatingSystems), 0.02),
    Browser          = fct_lump_prop(factor(Browser), 0.02),
    Region           = fct_lump_prop(factor(Region), 0.02),
    TrafficType      = fct_lump_prop(factor(TrafficType), 0.02),
    Revenue = factor(ifelse(Revenue, "Yes", "No"), levels = c("No","Yes"))
  )
saveRDS(df, "output/clean_data.rds")
cat("Clean data:", nrow(df), "rows\n")

# ---- 4. EDA ----
theme_set(theme_minimal())
save_plot <- function(p, name) ggsave(paste0("output/", name, ".png"), p, width = 7, height = 4.5)

# Purchase vs non-purchase
save_plot(ggplot(df, aes(Revenue, fill = Revenue)) + geom_bar() +
            labs(title = "Purchase vs No Purchase"), "eda_target")

# Conversion rate by categorical variables
conv_plot <- function(var) {
  df |> group_by(.data[[var]]) |>
    summarise(rate = mean(Revenue == "Yes"), .groups = "drop") |>
    ggplot(aes(.data[[var]], rate)) + geom_col(fill = "steelblue") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Conversion rate by", var), y = "Conversion rate")
}
for (v in c("Month", "VisitorType", "Weekend", "TrafficType"))
  save_plot(conv_plot(v), paste0("eda_conv_", v))

# Numeric variables vs purchase
for (v in c("PageValues", "ExitRates", "BounceRates", "ProductRelated_Duration")) {
  save_plot(ggplot(df, aes(Revenue, log1p(.data[[v]]), fill = Revenue)) +
              geom_boxplot() + labs(title = paste(v, "(log1p) by Revenue")),
            paste0("eda_box_", v))
}

# Correlation heatmap of numeric variables
num <- df |> select(where(is.numeric))
cm <- round(cor(num), 2)
print(cm)
png("output/eda_correlation.png", 800, 700)
heatmap(cm, symm = TRUE, margins = c(12, 12), main = "Correlation")
dev.off()

# Statistical tests
print(chisq.test(table(df$VisitorType, df$Revenue)))
print(t.test(PageValues ~ Revenue, data = df))
