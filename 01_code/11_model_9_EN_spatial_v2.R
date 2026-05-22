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
    property_id, bedrooms, property_type,  lat, lon,
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

grid_EN1 <- expand.grid(
  alpha  = seq(0, 1, by = 0.5),
  lambda = 10^seq(-3, -1, length = 10)
)

set.seed(2026)
EN1 <- train(
  log_price ~ .,
  data = train_model,
  method = "glmnet",
  family = "gaussian",
  metric = "MAE",
  trControl = ctrl_spatial,
  preProcess = c("center", "scale"),
  tuneGrid = grid_EN1
)

EN1
EN1$bestTune

pred_EN1 <- predict(EN1, newdata = test_model)

submission_EN1 <- data.frame(
  property_id = test_model$property_id,
  price = exp(pred_EN1)
)

lambda_str1 <- gsub("\\.", "_", as.character(round(EN1$bestTune$lambda, 6)))
alpha_str1  <- gsub("\\.", "_", as.character(EN1$bestTune$alpha))

name_EN1 <- paste0(
  "Model_EN1_SpatialCV_lambda_",
  lambda_str1,
  "_alpha_",
  alpha_str1,
  ".csv"
)

write.csv(
  submission_EN1,
  here("03_output","submissions",name_EN1),
  row.names = FALSE
)
