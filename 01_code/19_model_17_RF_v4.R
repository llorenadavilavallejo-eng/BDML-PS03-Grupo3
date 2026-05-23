####################################
# MODELO 4: RANDOM FOREST FAST
####################################

if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# =========================
# BASE RF FAST
# =========================

train_model_rf_fast <- train_model_lm3
test_model_rf_fast  <- test_model_lm3

# =========================
# CONTROL MÁS LIVIANO
# =========================

ctrl_rf_fast <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  savePredictions = "final"
)

# =========================
# GRID PEQUEÑO
# =========================

grid_rf_fast <- expand.grid(
  mtry = c(6, 10)
)

# =========================
# MODELO RANDOM FOREST FAST
# =========================

set.seed(2026)

rf_fast <- train(
  log_price ~ .,
  data       = train_model_rf_fast,
  method     = "rf",
  metric     = "MAE",
  trControl  = ctrl_rf_fast,
  tuneGrid   = grid_rf_fast,
  ntree      = 40,
  importance = FALSE
)

rf_fast
rf_fast$bestTune
getTrainPerf(rf_fast)

# =========================
# TABLA DE PUNTAJES
# =========================

tabla_rf_fast <- rf_fast$results |>
  arrange(MAE) |>
  mutate(rank = row_number()) |>
  select(rank, mtry, MAE, RMSE, Rsquared)

View(tabla_rf_fast)

write.csv(
  tabla_rf_fast,
  "Tabla_Model4_RF_fast.csv",
  row.names = FALSE
)

# =========================
# PREDICCIONES
# =========================

pred_log_rf_fast <- predict(
  rf_fast,
  newdata = test_model_rf_fast |> select(-property_id)
)

# =========================
# RECORTE DE PREDICCIONES
# =========================

log_low_rf_fast <- quantile(
  train_model_rf_fast$log_price,
  0.01,
  na.rm = TRUE
)

log_high_rf_fast <- quantile(
  train_model_rf_fast$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_rf_fast_c <- pmin(
  pmax(pred_log_rf_fast, log_low_rf_fast),
  log_high_rf_fast
)

# =========================
# SUBMISSION KAGGLE
# =========================

submission_rf_fast <- data.frame(
  property_id = test_model_rf_fast$property_id,
  price       = exp(pred_log_rf_fast_c)
)

View(submission_rf_fast)

mtry_str_rf_fast <- as.character(
  rf_fast$bestTune$mtry
)

file_rf_fast <- paste0(
  "Model4_RF_fast_mtry_",
  mtry_str_rf_fast,
  ".csv"
)

write.csv(
  submission_rf_fast,
  here("03_output", "submissions", file_rf_fast),
  row.names = FALSE
)

cat("✅ Modelo 17\n")