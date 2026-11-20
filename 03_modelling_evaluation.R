# 03_modelling_evaluation.R
# Steps 6-7: Logistic Regression & Random Forest, evaluation
source("00_setup.R")
df <- readRDS("output/clean_data.rds")
set.seed(42)

# ---- Stratified 80/20 train-test split ----
idx <- unlist(lapply(split(seq_len(nrow(df)), df$Revenue),
                     function(i) sample(i, floor(0.8 * length(i)))))
train <- df[idx, ]
test  <- df[-idx, ]

# ---- Models ----
log_model <- glm(Revenue ~ ., data = train, family = binomial)
rf_model  <- randomForest(Revenue ~ ., data = train, ntree = 300, importance = TRUE)

# ---- Predictions ----
p_log <- predict(log_model, test, type = "response")
p_rf  <- predict(rf_model, test, type = "prob")[, "Yes"]

# ---- Metrics ----
evaluate <- function(prob, actual, name, cutoff = 0.5) {
  pred <- factor(ifelse(prob >= cutoff, "Yes", "No"), levels = c("No", "Yes"))
  tp <- sum(pred == "Yes" & actual == "Yes"); tn <- sum(pred == "No" & actual == "No")
  fp <- sum(pred == "Yes" & actual == "No");  fn <- sum(pred == "No" & actual == "Yes")
  precision <- tp / (tp + fp); recall <- tp / (tp + fn)
  data.frame(Model = name,
             Accuracy  = (tp + tn) / length(actual),
             Precision = precision,
             Recall    = recall,
             F1        = 2 * precision * recall / (precision + recall),
             ROC_AUC   = as.numeric(auc(roc(actual, prob, levels = c("No", "Yes"), quiet = TRUE))))
}
metrics <- rbind(evaluate(p_log, test$Revenue, "Logistic Regression"),
                 evaluate(p_rf,  test$Revenue, "Random Forest"))
metrics[, -1] <- round(metrics[, -1], 4)
print(metrics)
write.csv(metrics, "output/model_metrics.csv", row.names = FALSE)

# ---- ROC curves ----
roc_log <- roc(test$Revenue, p_log, levels = c("No", "Yes"), quiet = TRUE)
roc_rf  <- roc(test$Revenue, p_rf,  levels = c("No", "Yes"), quiet = TRUE)
roc_df <- rbind(
  data.frame(FPR = 1 - roc_log$specificities, TPR = roc_log$sensitivities, Model = "Logistic Regression"),
  data.frame(FPR = 1 - roc_rf$specificities,  TPR = roc_rf$sensitivities,  Model = "Random Forest"))
saveRDS(roc_df, "output/roc_data.rds")
ggsave("output/roc_curves.png",
       ggplot(roc_df, aes(FPR, TPR, colour = Model)) + geom_line(linewidth = 1) +
         geom_abline(linetype = "dashed") + theme_minimal() + labs(title = "ROC curves"),
       width = 6, height = 5)

# ---- Confusion matrices ----
cat("\nLogistic confusion matrix:\n"); print(table(Predicted = ifelse(p_log >= .5, "Yes", "No"), Actual = test$Revenue))
cat("\nRandom Forest confusion matrix:\n"); print(table(Predicted = ifelse(p_rf >= .5, "Yes", "No"), Actual = test$Revenue))

# ---- Feature importance (RF) ----
imp <- data.frame(Feature = rownames(importance(rf_model)),
                  Importance = importance(rf_model)[, "MeanDecreaseGini"]) |>
  arrange(desc(Importance))
print(head(imp, 10))
saveRDS(imp, "output/rf_importance.rds")
ggsave("output/rf_importance.png",
       ggplot(head(imp, 10), aes(reorder(Feature, Importance), Importance)) +
         geom_col(fill = "darkgreen") + coord_flip() + theme_minimal() +
         labs(title = "Random Forest - top 10 features", x = NULL),
       width = 6, height = 4.5)

# ---- Save for the dashboard ----
saveRDS(rf_model, "output/rf_model.rds")
saveRDS(train[1, setdiff(names(train), "Revenue")], "output/template_row.rds")
saveRDS(sapply(train[sapply(train, is.numeric)], median), "output/num_medians.rds")
