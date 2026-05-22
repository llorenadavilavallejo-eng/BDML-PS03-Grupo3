####################################
# MODELO 3: CART 3 MEJORADO
####################################

library(rpart)

# =========================
# BASE CART MEJORADA
# =========================

train_model_cart3 <- train_model_lm3 |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    amenity_density =
      log_rest_250m +
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m,
    
    bath_bed = bathrooms * bedrooms
  )

test_model_cart3 <- test_model_lm3 |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    
    amenity_density =
      log_rest_250m +
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m,
    
    bath_bed = bathrooms * bedrooms
  )

# =========================
# GRID MÁS FINO
# =========================

grid_cart3 <- expand.grid(
  cp = seq(0.0001, 0.01, length = 30)
)

# =========================
# CONTROL CV
# =========================

ctrl_cart3 <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  savePredictions = "final"
)

# =========================
# MODELO CART
# =========================

set.seed(2026)

cart_3_mejorado <- train(
  log_price ~ .,
  data      = train_model_cart3,
  method    = "rpart",
  metric    = "MAE",
  trControl = ctrl_cart3,
  tuneGrid  = grid_cart3,
  control   = rpart.control(
    maxdepth  = 12,
    minsplit  = 30,
    minbucket = 10
  )
)

cart_3_mejorado
cart_3_mejorado$bestTune
getTrainPerf(cart_3_mejorado)

# =========================
# TABLA DE RESULTADOS
# =========================

tabla_cart3 <- cart_3_mejorado$results |>
  arrange(MAE) |>
  mutate(rank = row_number()) |>
  select(rank, cp, MAE, RMSE, Rsquared)

tabla_cart3

View(tabla_cart3)

write.csv(
  tabla_cart3,
  "Tabla_Model3_CART_3_mejorado.csv",
  row.names = FALSE
)

# =========================
# IMPORTANCIA VARIABLES
# =========================

varImp(cart_3_mejorado)

# =========================
# PREDICCIONES
# =========================

pred_log_cart3 <- predict(
  cart_3_mejorado,
  newdata = test_model_cart3 |>
    select(-property_id)
)

# =========================
# RECORTE MÁS AGRESIVO
# =========================

log_low_cart3 <- quantile(
  train_model_cart3$log_price,
  0.02,
  na.rm = TRUE
)

log_high_cart3 <- quantile(
  train_model_cart3$log_price,
  0.98,
  na.rm = TRUE
)

pred_log_cart3_c <- pmin(
  pmax(pred_log_cart3, log_low_cart3),
  log_high_cart3
)

# =========================
# SUBMISSION
# =========================

submission_cart3 <- data.frame(
  property_id = test_model_cart3$property_id,
  price       = exp(pred_log_cart3_c)
)

View(submission_cart3)

# =========================
# NOMBRE DINÁMICO
# =========================

cp_str_cart3 <- gsub(
  "\\.",
  "_",
  as.character(round(cart_3_mejorado$bestTune$cp, 6))
)

file_cart3 <- paste0(
  "Model3_CART_3_mejorado_cp_",
  cp_str_cart3,
  ".csv"
)

write.csv(
  submission_cart3,
  here("03_output", "submissions", "file_cart3"),
  row.names = FALSE
)

cat("✅ Modelo 16\n")