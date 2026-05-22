####################################
# MODELO 2: ELASTIC NET 4 MEJORADO
####################################

#Para el ultimo modelo elactic net educir el sobreajuste y mejorar la capacidad predictiva del modelo. Para ello,
#se seleccionan únicamente las variables más relevantes, se incorporan nuevas interacciones espaciales y económicas

# =========================
# BASE EN4 
# =========================

train_model_en4 <- train_model_lm3 |>
  select(
    log_price,
    property_type,
    lat, lon,
    rooms, bedrooms, bathrooms,
    log_surface_total,
    log_surface_covered,
    ratio_covered_total,
    surface_per_room,
    bathrooms_per_room,
    bedrooms_per_room,
    log_dist_parque,
    log_dist_bus,
    log_dist_commerce,
    log_dist_school,
    log_rest_250m,
    log_parks_500m,
    log_bus_300m,
    log_commerce_500m,
    tiene_ascensor,
    tiene_gimnasio,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito,
    tiene_terraza,
    tiene_remodelado,
    tiene_iluminado,
    tiene_duplex,
    tiene_moderno
  ) |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    superficie_x_banos = log_surface_total * bathrooms,
    superficie_x_tipo = log_surface_total
  )

test_model_en4 <- test_model_lm3 |>
  select(
    property_id,
    property_type,
    lat, lon,
    rooms, bedrooms, bathrooms,
    log_surface_total,
    log_surface_covered,
    ratio_covered_total,
    surface_per_room,
    bathrooms_per_room,
    bedrooms_per_room,
    log_dist_parque,
    log_dist_bus,
    log_dist_commerce,
    log_dist_school,
    log_rest_250m,
    log_parks_500m,
    log_bus_300m,
    log_commerce_500m,
    tiene_ascensor,
    tiene_gimnasio,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito,
    tiene_terraza,
    tiene_remodelado,
    tiene_iluminado,
    tiene_duplex,
    tiene_moderno
  ) |>
  mutate(
    lat_lon = lat * lon,
    lat2 = lat^2,
    lon2 = lon^2,
    superficie_x_banos = log_surface_total * bathrooms,
    superficie_x_tipo = log_surface_total
  )

# =========================
# IMPUTACIÓN
# =========================

train_model_en4 <- train_model_en4 |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

test_model_en4 <- test_model_en4 |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

# =========================
# OUTLIERS SOLO EN TRAIN
# =========================

q_low_en4 <- quantile(train_model_en4$log_price, 0.01, na.rm = TRUE)
q_high_en4 <- quantile(train_model_en4$log_price, 0.99, na.rm = TRUE)

train_model_en4 <- train_model_en4 |>
  filter(
    log_price >= q_low_en4,
    log_price <= q_high_en4
  )

# =========================
# GRID
# =========================

grid_EN4 <- expand.grid(
  alpha  = seq(0.05, 0.95, by = 0.05),
  lambda = 10^seq(-5, 0, length = 100)
)

ctrl_en4 <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final"
)

# =========================
# MODELO ELASTIC NET 4
# =========================

set.seed(2026)

EN4_mejorado <- train(
  log_price ~ .,
  data       = train_model_en4,
  method     = "glmnet",
  family     = "gaussian",
  metric     = "MAE",
  trControl  = ctrl_en4,
  preProcess = c("center", "scale"),
  tuneGrid   = grid_EN4
)

EN4_mejorado
EN4_mejorado$bestTune
getTrainPerf(EN4_mejorado)

# =========================
# PREDICCIÓN
# =========================

pred_log_EN4 <- predict(
  EN4_mejorado,
  newdata = test_model_en4 |> select(-property_id)
)

# Recorte más agresivo para evitar sobrepredicción
log_low_EN4 <- quantile(train_model_en4$log_price, 0.02, na.rm = TRUE)
log_high_EN4 <- quantile(train_model_en4$log_price, 0.98, na.rm = TRUE)

pred_log_EN4_c <- pmin(
  pmax(pred_log_EN4, log_low_EN4),
  log_high_EN4
)

# =========================
# SUBMISSION SIN SMEARING
# =========================

submission_EN4_mejorado <- data.frame(
  property_id = test_model_en4$property_id,
  price = exp(pred_log_EN4_c)
)

View(submission_EN4_mejorado)

lambda_str_EN4 <- gsub(
  "\\.",
  "_",
  as.character(round(EN4_mejorado$bestTune$lambda, 8))
)

alpha_str_EN4 <- gsub(
  "\\.",
  "_",
  as.character(EN4_mejorado$bestTune$alpha)
)

name_EN4 <- paste0(
  "Model2_EN_4_mejorado_lambda_",
  lambda_str_EN4,
  "_alpha_",
  alpha_str_EN4,
  ".csv"
)

write.csv(
  submission_EN4_mejorado,
  here("03_output", "submissions", "name_EN4"),
  row.names = FALSE
)

cat("✅ Modelo 15\n")
