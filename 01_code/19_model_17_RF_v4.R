####################################
# MODELO 4: RANDOM FOREST MEJORADO
####################################


if (!require(randomForest)) {
  install.packages("randomForest")
  library(randomForest)
}

# =========================
# BASE RF MEJORADA
# =========================

train_model_rf1 <- train_model_lm3 |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    amenity_density =
      log_rest_250m +
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m,
    
    dist_services =
      log_dist_bus +
      log_dist_commerce +
      log_dist_school,
    
    dist_security_health =
      log_dist_health +
      log_dist_police,
    
    bath_bed = bathrooms * bedrooms,
    surface_bath = log_surface_total * bathrooms,
    surface_bed  = log_surface_total * bedrooms
  )

test_model_rf1 <- test_model_lm3 |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    amenity_density =
      log_rest_250m +
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m,
    
    dist_services =
      log_dist_bus +
      log_dist_commerce +
      log_dist_school,
    
    dist_security_health =
      log_dist_health +
      log_dist_police,
    
    bath_bed = bathrooms * bedrooms,
    surface_bath = log_surface_total * bathrooms,
    surface_bed  = log_surface_total * bedrooms
  )

# =========================
# GRID RF
# =========================

grid_rf1 <- expand.grid(
  mtry = c(4, 6, 8, 10, 12, 15)
)

ctrl_rf1 <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final"
)

# =========================
# MODELO RANDOM FOREST
# =========================

set.seed(2026)

rf_mejorado <- train(
  log_price ~ .,
  data       = train_model_rf1,
  method     = "rf",
  metric     = "MAE",
  trControl  = ctrl_rf1,
  tuneGrid   = grid_rf1,
  ntree      = 200,
  importance = TRUE
)

rf_mejorado
rf_mejorado$bestTune
getTrainPerf(rf_mejorado)

# =========================
# TABLA DE RESULTADOS
# =========================

tabla_rf1 <- rf_mejorado$results |>
  arrange(MAE) |>
  mutate(rank = row_number()) |>
  select(rank, mtry, MAE, RMSE, Rsquared)

View(tabla_rf1)

write.csv(
  tabla_rf1,
  "Tabla_Model4_RF_mejorado.csv",
  row.names = FALSE
)

# =========================
# IMPORTANCIA VARIABLES
# =========================

imp_rf1 <- varImp(rf_mejorado)

imp_rf1

# =========================
# PREDICCIONES
# =========================

pred_log_rf1 <- predict(
  rf_mejorado,
  newdata = test_model_rf1 |> select(-property_id)
)

# =========================
# RECORTE PRUDENTE
# =========================

log_low_rf1 <- quantile(
  train_model_rf1$log_price,
  0.01,
  na.rm = TRUE
)

log_high_rf1 <- quantile(
  train_model_rf1$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_rf1_c <- pmin(
  pmax(pred_log_rf1, log_low_rf1),
  log_high_rf1
)

# =========================
# SUBMISSION
# =========================

submission_rf_mejorado <- data.frame(
  property_id = test_model_rf1$property_id,
  price       = exp(pred_log_rf1_c)
)

View(submission_rf_mejorado)

mtry_str_rf1 <- as.character(
  rf_mejorado$bestTune$mtry
)

file_rf1 <- paste0(
  "Model4_RF_mejorado_mtry_",
  mtry_str_rf1,
  ".csv"
)

write.csv(
  submission_rf_mejorado,
  here("03_output", "submissions", "file_rf1"),
  row.names = FALSE
)

cat("✅ Modelo 17\n")