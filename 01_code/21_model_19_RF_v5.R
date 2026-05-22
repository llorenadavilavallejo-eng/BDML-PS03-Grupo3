###
#RF
grid_rf <- expand.grid(mtry = c(2, 4, 6))

set.seed(2026)
rf_model <- train(
  log_price ~ .,
  data      = train_model_clean,
  method    = "rf",
  metric    = "MAE",
  trControl = ctrl_reg,
  tuneGrid  = grid_rf,
  ntree     = 150
)

pred_rf <- predict(rf_model, newdata = test_model_clean)
write.csv(data.frame(property_id = test_model_clean$property_id, price = exp(pred_rf)), 
          here("03_output", "submissions", "Model4_RandomForest.csv"), row.names = FALSE)

cat("✅ Modelo 19\n")