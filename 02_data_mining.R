
# 02_data_mining.R
# Step 5: Clustering, Association Rules and Decision Tree

# ---- Package installation ----
packages <- c("dplyr", "arules", "rpart", "rpart.plot")

new_packages <- packages[
  !packages %in% installed.packages()[, "Package"]
]

if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE)
}

library(dplyr)
library(arules)
library(rpart)
library(rpart.plot)

# ---- Load data ----
source("00_setup.R")

df <- readRDS("output/clean_data.rds")

set.seed(42)

# Ensure Revenue is a factor
df$Revenue <- factor(df$Revenue, levels = c("No", "Yes"))


# =====================================================
# A. K-MEANS CLUSTERING OF VISITOR BEHAVIOUR
# =====================================================

cl_vars <- c(
  "Administrative_Duration",
  "Informational_Duration",
  "ProductRelated_Duration",
  "BounceRates",
  "ExitRates",
  "PageValues"
)

# Log transformation and scaling
X <- scale(log1p(df[, cl_vars]))

# Elbow method
wss <- sapply(1:8, function(k) {
  kmeans(X, centers = k, nstart = 10)$tot.withinss
})

png("output/cluster_elbow.png", 700, 450)

plot(
  1:8, wss,
  type = "b",
  xlab = "Number of clusters (K)",
  ylab = "Total within-cluster sum of squares",
  main = "Elbow Method for Optimal K"
)

dev.off()

# Apply K-means with 4 clusters
km <- kmeans(X, centers = 4, nstart = 25)

df$Cluster <- factor(km$cluster)

# Cluster profiling
cluster_profile <- df |>
  group_by(Cluster) |>
  summarise(
    Size = n(),
    PurchaseRate = round(mean(Revenue == "Yes"), 3),
    across(all_of(cl_vars), ~ round(mean(.x), 2)),
    .groups = "drop"
  )

print(cluster_profile)

write.csv(
  cluster_profile,
  "output/cluster_profile.csv",
  row.names = FALSE
)


# =====================================================
# B. ASSOCIATION RULES USING APRIORI
# =====================================================

# Discretization function
disc <- function(x, nm) {
  
  breaks <- unique(
    quantile(
      x,
      probs = c(0, 0.33, 0.66, 1),
      na.rm = TRUE
    )
  )
  
  if (length(breaks) < 2) {
    return(factor(rep("Low", length(x))))
  }
  
  labels <- c("Low", "Med", "High")[seq_len(length(breaks) - 1)]
  
  result <- cut(
    x,
    breaks = breaks,
    labels = labels,
    include.lowest = TRUE
  )
  
  factor(result)
}

# Create transaction data
# IMPORTANT: Do not manually prefix values with column names.
trans_df <- data.frame(
  
  PageValues = factor(
    ifelse(df$PageValues > 0, "Positive", "Zero")
  ),
  
  ProductPages = disc(
    df$ProductRelated,
    "ProductPages"
  ),
  
  Bounce = disc(
    df$BounceRates,
    "Bounce"
  ),
  
  Exit = disc(
    df$ExitRates,
    "Exit"
  ),
  
  Visitor = factor(
    as.character(df$VisitorType)
  ),
  
  Weekend = factor(
    as.character(df$Weekend)
  ),
  
  Purchase = factor(
    as.character(df$Revenue),
    levels = c("No", "Yes")
  )
)

# Convert dataframe into transactions
trans <- as(trans_df, "transactions")

# Check available item labels
itemLabels(trans)

# Generate association rules
rules <- apriori(
  trans,
  parameter = list(
    supp = 0.01,
    conf = 0.30,
    minlen = 2,
    maxlen = 4
  ),
  appearance = list(
    rhs = "Purchase=Yes",
    default = "lhs"
  )
)

# Sort by lift
rules <- sort(rules, by = "lift", decreasing = TRUE)

# Display top 10 rules
inspect(head(rules, 10))

# Save top 20 rules
top_rules <- head(rules, 20)

write.csv(
  as(top_rules, "data.frame"),
  "output/top_rules.csv",
  row.names = FALSE
)


# =====================================================
# C. DECISION TREE CLASSIFICATION
# =====================================================

tree <- rpart(
  Revenue ~ . - Cluster,
  data = df,
  method = "class",
  control = rpart.control(
    cp = 0.005,
    maxdepth = 5
  )
)

# Visualize decision tree
png("output/decision_tree.png", 1000, 650)

rpart.plot(
  tree,
  extra = 104,
  main = "Decision Tree: Purchase Intention"
)

dev.off()

# Variable importance
importance <- sort(
  tree$variable.importance,
  decreasing = TRUE
)

print(importance)

# Save variable importance
importance_df <- data.frame(
  Variable = names(importance),
  Importance = as.numeric(importance)
)

write.csv(
  importance_df,
  "output/variable_importance.csv",
  row.names = FALSE
)

cat("\nData mining analysis completed successfully!\n")
