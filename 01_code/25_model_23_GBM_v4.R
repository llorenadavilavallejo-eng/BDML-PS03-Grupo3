####################################
# MODELO 5: GBM 2 MEJORADO 
####################################

if (!require(gbm)) {
  install.packages("gbm")
  library(gbm)
}

# =========================
# BASE GBM 2
# =========================

train_model_gbm2 <- train_model_lm3 |>
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
    
    surface_bath = log_surface_total * bathrooms,
    surface_bed  = log_surface_total * bedrooms,
    bath_bed     = bathrooms * bedrooms
  )

test_model_gbm2 <- test_model_lm3 |>
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
    
    surface_bath = log_surface_total * bathrooms,
    surface_bed  = log_surface_total * bedrooms,
    bath_bed     = bathrooms * bedrooms
  )

# =========================
# CONTROL LIVIANO
# =========================

ctrl_gbm2 <- trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE,
  savePredictions = "final"
)

# =========================
# GRID MÁS FINO PERO LIVIANO
# =========================

grid_gbm2 <- expand.grid(
  n.trees = c(700, 900, 1100),
  interaction.depth = c(4, 5),
  shrinkage = c(0.02, 0.03),
  n.minobsinnode = c(8, 12)
)

# =========================
# MODELO GBM 2
# =========================

set.seed(2026)

gbm_2_mejorado <- train(
  log_price ~ .,
  data      = train_model_gbm2,
  method    = "gbm",
  metric    = "MAE",
  trControl = ctrl_gbm2,
  tuneGrid  = grid_gbm2,
  verbose   = FALSE
)

gbm_2_mejorado
gbm_2_mejorado$bestTune
getTrainPerf(gbm_2_mejorado)

# =========================
# TABLA DE RESULTADOS
# =========================

tabla_gbm2 <- gbm_2_mejorado$results |>
  arrange(MAE) |>
  mutate(rank = row_number()) |>
  select(
    rank,
    n.trees,
    interaction.depth,
    shrinkage,
    n.minobsinnode,
    MAE,
    RMSE,
    Rsquared
  )

View(tabla_gbm2)

write.csv(
  tabla_gbm2,
  "Tabla_Model5_GBM_2_mejorado.csv",
  row.names = FALSE
)

# =========================
# PREDICCIONES
# =========================

pred_log_gbm2 <- predict(
  gbm_2_mejorado,
  newdata = test_model_gbm2 |> select(-property_id)
)

# Recorte un poco más agresivo para evitar sobrepredicción
log_low_gbm2 <- quantile(train_model_gbm2$log_price, 0.015, na.rm = TRUE)
log_high_gbm2 <- quantile(train_model_gbm2$log_price, 0.985, na.rm = TRUE)

pred_log_gbm2_c <- pmin(
  pmax(pred_log_gbm2, log_low_gbm2),
  log_high_gbm2
)

# =========================
# SUBMISSION
# =========================

submission_gbm2 <- data.frame(
  property_id = test_model_gbm2$property_id,
  price       = exp(pred_log_gbm2_c)
)

View(submission_gbm2)

best_gbm2 <- gbm_2_mejorado$bestTune

file_gbm2 <- paste0(
  "Model5_GBM_2_mejorado_arboles_",
  best_gbm2$n.trees,
  "_prof_",
  best_gbm2$interaction.depth,
  "_shrink_",
  gsub("\\.", "_", best_gbm2$shrinkage),
  "_minobs_",
  best_gbm2$n.minobsinnode,
  ".csv"
)

write.csv(
  submission_gbm2,
  here("03_output", "submissions", "file_gbm2"),
  row.names = FALSE
)

cat("✅ Modelo 23\n")