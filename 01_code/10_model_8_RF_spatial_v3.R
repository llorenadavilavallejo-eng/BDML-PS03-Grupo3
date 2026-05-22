# Seleccionamos las variables finales
train_model <- train_final |>
  select(
    log_price, bedrooms, property_type,lat, lon, 
    dist_parque_m, dist_bus_stop, dist_commerce, dist_school, n_rest_250m,
    dist_gym, dist_bank, dist_health, dist_police, tiene_ascensor, tiene_gimnasio, 
    tiene_bbq, tiene_parqueadero, tiene_balcon, tiene_deposito
  )

train_sf <- st_as_sf(
  train_model,
  coords = c("lon", "lat"),
  crs = 4326
)

test_model <- test_final |>
  select(
    property_id, bedrooms, property_type,lat, lon, 
    dist_parque_m, dist_bus_stop, dist_commerce, dist_school, n_rest_250m,
    dist_gym, dist_bank, dist_health, dist_police, tiene_ascensor, tiene_gimnasio, 
    tiene_bbq, tiene_parqueadero, tiene_balcon, tiene_deposito
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

grid_rf1 <- expand.grid(
  mtry          = c(8, 11, 15),
  splitrule     = "variance",
  min.node.size = c(5, 10, 20)
)

set.seed(2026)
rf_spatial  <- train(
  log_price ~ .,
  data       = train_model,
  method     = "ranger",
  trControl  = ctrl_spatial,
  metric     = "MAE",
  tuneGrid   = grid_rf1,
  num.trees  = 300,
  importance = "impurity"
)

rf_spatial
rf_spatial$bestTune

pred_rf_spatial  <- predict(rf_spatial, newdata = test_model)

submission_rf_spatial <- data.frame(
  property_id = test_model$property_id,
  price = exp(pred_rf_spatial)
)

file_rf_spatial <- paste0(
  "Model_RF_SpatialCV_mtry_",
  rf_spatial$bestTune$mtry,
  "_minNode_",
  rf_spatial$bestTune$min.node.size,
  "_ntree_300.csv"
)

write.csv(
  submission_rf_spatial,
  here("03_output","submissions",file_rf_spatial),
  row.names = FALSE
)

cat("✅ Modelo 8\n")