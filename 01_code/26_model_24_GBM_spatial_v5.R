####################################
# BASE GBM MEJORADA
####################################

train_model_gbm1 <- train_model_lm3 |>
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
    
    surface_bath =
      log_surface_total * bathrooms,
    
    surface_bed =
      log_surface_total * bedrooms
  )

test_model_gbm1 <- test_model_lm3 |>
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
    
    surface_bath =
      log_surface_total * bathrooms,
    
    surface_bed =
      log_surface_total * bedrooms
  )

####################################
# SPATIAL CV
####################################

train_sf <- st_as_sf(
  train_model_gbm1,
  coords = c("lon", "lat"),
  crs = 4326
)

set.seed(2026)

spatial_folds <- spatial_block_cv(
  train_sf,
  v = 5
)

index <- lapply(
  spatial_folds$splits,
  function(x) x$in_id
)

indexOut <- lapply(
  spatial_folds$splits,
  function(x) {
    setdiff(
      seq_len(nrow(train_model_gbm1)),
      x$in_id
    )
  }
)

####################################
# CONTROL
####################################

ctrl_gbm1 <- trainControl(
  method = "cv",
  index = index,
  indexOut = indexOut,
  verboseIter = TRUE,
  savePredictions = "final"
)

####################################
# GRID GBM
####################################

grid_gbm1 <- expand.grid(
  n.trees = c(300, 500, 700),
  interaction.depth = c(3, 5),
  shrinkage = c(0.03, 0.05),
  n.minobsinnode = c(10)
)

####################################
# MODELO GBM
####################################

set.seed(2026)

gbm_mejorado_spatial <- train(
  log_price ~ .,
  data = train_model_gbm1,
  method = "gbm",
  metric = "MAE",
  trControl = ctrl_gbm1,
  tuneGrid = grid_gbm1,
  verbose = FALSE
)

####################################
# RESULTADOS
####################################

gbm_mejorado_spatial

gbm_mejorado_spatial$bestTune

getTrainPerf(gbm_mejorado_spatial)

tabla_gbm1 <- gbm_mejorado_spatial$results |>
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

####################################
# IMPORTANCIA VARIABLES
####################################

varImp(gbm_mejorado_spatial)

####################################
# PREDICCIONES
####################################

pred_log_gbm1 <- predict(
  gbm_mejorado_spatial,
  newdata = test_model_gbm1 |>
    select(-property_id)
)

####################################
# RECORTE PRUDENTE
####################################

log_low_gbm1 <- quantile(
  train_model_gbm1$log_price,
  0.01,
  na.rm = TRUE
)

log_high_gbm1 <- quantile(
  train_model_gbm1$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_gbm1_c <- pmin(
  pmax(pred_log_gbm1, log_low_gbm1),
  log_high_gbm1
)

####################################
# SUBMISSION
####################################

submission_gbm1 <- data.frame(
  property_id = test_model_gbm1$property_id,
  price = exp(pred_log_gbm1_c)
)

best_gbm1 <- gbm_mejorado_spatial$bestTune

file_gbm1 <- paste0(
  "Model_GBM_SpatialCV_",
  "trees_", best_gbm1$n.trees,
  "_depth_", best_gbm1$interaction.depth,
  "_shrink_", gsub("\\.","_",best_gbm1$shrinkage),
  ".csv"
)

write.csv(
  submission_gbm1,
  here("03_output", "submissions", file_gbm1),
  row.names = FALSE
)

cat("✅ Modelo 24\n")
