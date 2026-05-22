# Mejora variable área
# Extraer áreas
patron_area <- "(\\d+[\\.,]?\\d*)\\s?(m2|mt2|mts2|mts|m²|metros|metro)"

full_base <- full_base |>
  mutate(
    area_texto = str_extract(text_all, patron_area),
    area_texto = str_extract(area_texto, "\\d+[\\.,]?\\d*"),
    area_texto = str_replace(area_texto, ",", "."),
    area_texto = as.numeric(area_texto)
  )

# Completar faltantes
full_base <- full_base |>
  mutate(
    surface_covered = if_else(
      is.na(surface_covered),
      area_texto,
      surface_covered
    )
  )

# Volver a separar train y test 
train_final <- full_base |> filter(dataset == "train") |> select(-dataset)
test_final  <- full_base |> filter(dataset == "test")  |> select(-dataset, -price, -log_price)

# Convertimos variables a formatos correctos
train_final <- train_final |>
  mutate(
    property_type = as.factor(property_type),
    across(starts_with("tiene_"), as.numeric)
  )

test_final <- test_final |>
  mutate(
    property_type = as.factor(property_type),
    across(starts_with("tiene_"), as.numeric)
  )

train_model <- train_final |>
  select(
    log_price,surface_covered,bedrooms,bathrooms, property_type,lat,lon,
    dist_parque_m,dist_bus_stop,dist_commerce,dist_school,n_rest_250m,
    dist_gym,dist_bank,dist_health,dist_police,tiene_ascensor,tiene_gimnasio,
    tiene_bbq,tiene_parqueadero,tiene_balcon,tiene_deposito
  )

test_model <- test_final |>
  select(
    property_id,surface_covered,bedrooms,bathrooms,property_type,lat,lon,
    dist_parque_m,dist_bus_stop,dist_commerce,dist_school,n_rest_250m,
    dist_gym,dist_bank,dist_health,dist_police,tiene_ascensor,tiene_gimnasio,
    tiene_bbq,tiene_parqueadero,tiene_balcon,tiene_deposito
  )

x_vars <- setdiff(names(train_model), c("log_price","price"))
num_vars <- x_vars[sapply(train_model[, x_vars], is.numeric)]
fac_vars <- x_vars[sapply(train_model[, x_vars], is.factor)]

colSums(is.na(train_model))
colSums(is.na(test_model))

train_sf <- st_as_sf(
  train_model,
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
  function(x) as.integer(x$in_id)
)

indexOut <- lapply(
  spatial_folds$splits,
  function(x) as.integer(x$out_id)
)

ctrl_spatial <- trainControl(
  method = "cv",
  index = index,
  indexOut = indexOut,
  verboseIter = TRUE,
  savePredictions = "final"
)

grid_xgb <- expand.grid(
  nrounds = c(300, 500, 800),
  max_depth = c(4, 6, 8),
  eta = c(0.03, 0.05, 0.1),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 0.9),
  min_child_weight = c(1, 5),
  subsample = c(0.7, 0.9)
)

set.seed(2026)

xgb_spatial <- train(
  log_price ~ .,
  data = train_model,
  method = "xgbTree",
  metric = "MAE",
  trControl = ctrl_spatial,
  tuneGrid = grid_xgb,
  na.action = na.pass,
  verbosity = 0
)

xgb_spatial
xgb_spatial$bestTune

varImp(xgb_spatial)

pred_xgb <- predict(
  xgb_spatial,
  newdata = test_model
)

submission_xgb <- data.frame(
  property_id = test_model$property_id,
  price = exp(pred_xgb)
)

file_xgb <- paste0(
  "Model_XGB_SpatialCV_depth_",
  xgb_spatial$bestTune$max_depth,
  "_eta_",
  gsub("\\.","_",xgb_spatial$bestTune$eta),
  "_rounds_",
  xgb_spatial$bestTune$nrounds,
  ".csv"
)

write.csv(
  submission_xgb,
  here("03_output","submissions",file_xgb),
  row.names = FALSE
)