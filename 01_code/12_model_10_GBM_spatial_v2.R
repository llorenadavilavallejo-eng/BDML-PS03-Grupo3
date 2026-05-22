# Seleccionamos las variables finales
train_model <- train_final |>
  select(
    log_price, bedrooms, property_type,  lat, lon,
    dist_parque_m, dist_bus_stop, dist_commerce, dist_school, n_rest_250m,
    dist_gym, dist_bank, dist_health, dist_police, tiene_ascensor, tiene_gimnasio, 
    tiene_bbq, tiene_parqueadero, tiene_balcon, tiene_deposito
  )

test_model <- test_final |>
  select(
    property_id, bedrooms, property_type, lat, lon,
    dist_parque_m, dist_bus_stop, dist_commerce, dist_school, n_rest_250m,
    dist_gym, dist_bank, dist_health, dist_police, tiene_ascensor, tiene_gimnasio, 
    tiene_bbq, tiene_parqueadero, tiene_balcon, tiene_deposito
  )

train_sf <- st_as_sf(
  train_model,
  coords = c("lon", "lat"),
  crs = 4326
)

x_vars <- setdiff(names(train_model), c("log_price","price"))
num_vars <- x_vars[sapply(train_model[, x_vars], is.numeric)]
fac_vars <- x_vars[sapply(train_model[, x_vars], is.factor)]

colSums(is.na(train_model))
colSums(is.na(test_model))

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
  function(x) setdiff(seq_len(nrow(train_model)), x$in_id)
)

ctrl_spatial <- trainControl(
  method = "cv",
  index = index,
  indexOut = indexOut,
  verboseIter = FALSE,
  savePredictions = "final"
)

grid_gbm <- expand.grid(
  n.trees           = c(200, 500, 800),
  interaction.depth = c(2, 4, 6),
  shrinkage         = c(0.01, 0.05),
  n.minobsinnode    = c(10, 20)
)

set.seed(2026)
gbm_reg <- train(
  log_price ~ .,
  data = train_model,
  method = "gbm",
  trControl = ctrl_spatial,
  metric = "MAE",
  tuneGrid = grid_gbm,
  verbose = FALSE
)

gbm_reg
gbm_reg$bestTune

pred_gbm <- predict(gbm_reg, newdata = test_model)

submission_gbm <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_gbm)
)

file_gbm <- paste0(
  "Model_GBM_SpatialCV_trees_",
  gbm_reg$bestTune$n.trees,
  "_depth_",
  gbm_reg$bestTune$interaction.depth,
  "_shrink_",
  gbm_reg$bestTune$shrinkage,
  ".csv"
)

write.csv(
  submission_gbm,
  here("03_output","submissions",file_gbm),
  row.names = FALSE
)

cat("✅ Modelo 10\n")