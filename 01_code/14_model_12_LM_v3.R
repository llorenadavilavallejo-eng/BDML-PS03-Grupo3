####################################
# MODELO 2: LM 3 MEJORADO
####################################

###Finalmente realziaremos una mejora de nuestro modelo lineal en donde incorporaremos
#coordenadas espaciales, densidades de amenidades cercanas
#nuevas variables derivadas del texto de los anuncios, con el fin de saber si podemos mejorar nuestro modelo

# ##########################
# DENSIDADES ESPACIALES
# ##########################

parks_500m <- st_is_within_distance(
  full_sf_m,
  parks_centroids_m,
  dist = 500
)

bus_300m <- st_is_within_distance(
  full_sf_m,
  bus_points_m,
  dist = 300
)

commerce_500m <- st_is_within_distance(
  full_sf_m,
  commerce_points_m,
  dist = 500
)

full_db <- full_db |>
  mutate(
    n_parks_500m    = lengths(parks_500m),
    n_bus_300m      = lengths(bus_300m),
    n_commerce_500m = lengths(commerce_500m)
  )

# ########################
# NUEVAS VARIABLES DE TEXTO
# ########################

full_base <- full_base |>
  left_join(
    full_db |>
      select(property_id, n_parks_500m, n_bus_300m, n_commerce_500m),
    by = "property_id"
  ) |>
  mutate(
    tiene_terraza   = if_else(str_detect(text_all, "terraza"), 1, 0),
    tiene_remodelado = if_else(str_detect(text_all, "remodelado|remodelada|remodelar"), 1, 0),
    tiene_iluminado = if_else(str_detect(text_all, "iluminado|iluminada|luz natural"), 1, 0),
    tiene_duplex    = if_else(str_detect(text_all, "duplex|d[uú]plex"), 1, 0),
    tiene_moderno   = if_else(str_detect(text_all, "moderno|moderna"), 1, 0),
    tiene_chimenea  = if_else(str_detect(text_all, "chimenea"), 1, 0)
  )

# =========================
# VOLVER A SEPARAR TRAIN Y TEST
# =========================

train_final_lm3 <- full_base |>
  filter(dataset == "train") |>
  select(-dataset)

test_final_lm3 <- full_base |>
  filter(dataset == "test") |>
  select(-dataset, -price, -log_price)

# #########################
# BASE TRAIN LM3
# #########################

train_model_lm3 <- train_final_lm3 |>
  mutate(
    property_type = as.factor(property_type),
    
    log_surface_total   = log1p(surface_total),
    log_surface_covered = log1p(surface_covered),
    
    log_dist_parque   = log1p(dist_parque_m),
    log_dist_bus      = log1p(dist_bus_stop),
    log_dist_commerce = log1p(dist_commerce),
    log_dist_school   = log1p(dist_school),
    log_dist_gym      = log1p(dist_gym),
    log_dist_bank     = log1p(dist_bank),
    log_dist_health   = log1p(dist_health),
    log_dist_police   = log1p(dist_police),
    
    log_rest_250m       = log1p(n_rest_250m),
    log_parks_500m      = log1p(n_parks_500m),
    log_bus_300m        = log1p(n_bus_300m),
    log_commerce_500m   = log1p(n_commerce_500m),
    
    ratio_covered_total = surface_covered / surface_total,
    surface_per_room    = surface_total / rooms,
    bathrooms_per_room  = bathrooms / rooms,
    bedrooms_per_room   = bedrooms / rooms,
    
    ratio_covered_total = if_else(is.finite(ratio_covered_total), ratio_covered_total, NA_real_),
    surface_per_room    = if_else(is.finite(surface_per_room), surface_per_room, NA_real_),
    bathrooms_per_room  = if_else(is.finite(bathrooms_per_room), bathrooms_per_room, NA_real_),
    bedrooms_per_room   = if_else(is.finite(bedrooms_per_room), bedrooms_per_room, NA_real_)
  ) |>
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
    log_dist_gym,
    log_dist_bank,
    log_dist_health,
    log_dist_police,
    
    log_rest_250m,
    log_parks_500m,
    log_bus_300m,
    log_commerce_500m,
    
    tiene_ascensor,
    tiene_gimnasio,
    tiene_bbq,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito,
    tiene_terraza,
    tiene_remodelado,
    tiene_iluminado,
    tiene_duplex,
    tiene_moderno,
    tiene_chimenea
  )

# #########################
# BASE TEST LM3
# #########################

test_model_lm3 <- test_final_lm3 |>
  mutate(
    property_type = as.factor(property_type),
    
    log_surface_total   = log1p(surface_total),
    log_surface_covered = log1p(surface_covered),
    
    log_dist_parque   = log1p(dist_parque_m),
    log_dist_bus      = log1p(dist_bus_stop),
    log_dist_commerce = log1p(dist_commerce),
    log_dist_school   = log1p(dist_school),
    log_dist_gym      = log1p(dist_gym),
    log_dist_bank     = log1p(dist_bank),
    log_dist_health   = log1p(dist_health),
    log_dist_police   = log1p(dist_police),
    
    log_rest_250m       = log1p(n_rest_250m),
    log_parks_500m      = log1p(n_parks_500m),
    log_bus_300m        = log1p(n_bus_300m),
    log_commerce_500m   = log1p(n_commerce_500m),
    
    ratio_covered_total = surface_covered / surface_total,
    surface_per_room    = surface_total / rooms,
    bathrooms_per_room  = bathrooms / rooms,
    bedrooms_per_room   = bedrooms / rooms,
    
    ratio_covered_total = if_else(is.finite(ratio_covered_total), ratio_covered_total, NA_real_),
    surface_per_room    = if_else(is.finite(surface_per_room), surface_per_room, NA_real_),
    bathrooms_per_room  = if_else(is.finite(bathrooms_per_room), bathrooms_per_room, NA_real_),
    bedrooms_per_room   = if_else(is.finite(bedrooms_per_room), bedrooms_per_room, NA_real_)
  ) |>
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
    log_dist_gym,
    log_dist_bank,
    log_dist_health,
    log_dist_police,
    
    log_rest_250m,
    log_parks_500m,
    log_bus_300m,
    log_commerce_500m,
    
    tiene_ascensor,
    tiene_gimnasio,
    tiene_bbq,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito,
    tiene_terraza,
    tiene_remodelado,
    tiene_iluminado,
    tiene_duplex,
    tiene_moderno,
    tiene_chimenea
  )

# #########################
# IMPUTACIÓN DE MISSINGS
# #########################

train_model_lm3 <- train_model_lm3 |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

test_model_lm3 <- test_model_lm3 |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
    )
  )

# ##########################
# WINSORIZACIÓN
# ##########################

winsor <- function(x) {
  q1  <- quantile(x, 0.01, na.rm = TRUE)
  q99 <- quantile(x, 0.99, na.rm = TRUE)
  pmin(pmax(x, q1), q99)
}

train_model_lm3 <- train_model_lm3 |>
  mutate(across(where(is.numeric), winsor))

test_model_lm3 <- test_model_lm3 |>
  mutate(across(where(is.numeric), winsor))

# #########################
# OUTLIERS DEL TARGET
# #########################

q_low <- quantile(
  train_model_lm3$log_price,
  0.01,
  na.rm = TRUE
)

q_high <- quantile(
  train_model_lm3$log_price,
  0.99,
  na.rm = TRUE
)

train_model_lm3 <- train_model_lm3 |>
  filter(
    log_price >= q_low,
    log_price <= q_high
  )

# #########################
# MODELO LM 3 MEJORADO
# #########################

set.seed(2026)

lm_3_mejorado <- train(
  log_price ~
    
    property_type +
    
    lat + lon +
    I(lat^2) + I(lon^2) +
    lat:lon +
    
    rooms + bedrooms + bathrooms +
    
    log_surface_total +
    I(log_surface_total^2) +
    log_surface_covered +
    
    ratio_covered_total +
    surface_per_room +
    bathrooms_per_room +
    bedrooms_per_room +
    
    log_dist_parque +
    log_dist_bus +
    log_dist_commerce +
    log_dist_school +
    log_dist_gym +
    log_dist_bank +
    log_dist_health +
    log_dist_police +
    
    log_rest_250m +
    log_parks_500m +
    log_bus_300m +
    log_commerce_500m +
    
    tiene_ascensor +
    tiene_gimnasio +
    tiene_bbq +
    tiene_parqueadero +
    tiene_balcon +
    tiene_deposito +
    tiene_terraza +
    tiene_remodelado +
    tiene_iluminado +
    tiene_duplex +
    tiene_moderno +
    tiene_chimenea +
    
    log_surface_total:property_type +
    bathrooms:log_surface_total,
  
  data      = train_model_lm3,
  method    = "lm",
  trControl = ctrl_reg,
  metric    = "MAE"
)

lm_3_mejorado
getTrainPerf(lm_3_mejorado)

# #########################
# PREDICCIONES
# #########################

pred_log_lm3 <- predict(
  lm_3_mejorado,
  newdata = test_model_lm3 |>
    select(-property_id)
)

log_low <- quantile(
  train_model_lm3$log_price,
  0.01,
  na.rm = TRUE
)

log_high <- quantile(
  train_model_lm3$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_lm3_c <- pmin(
  pmax(pred_log_lm3, log_low),
  log_high
)

# #########################
# SMEARING FACTOR
# #########################

resid_lm3 <- residuals(
  lm_3_mejorado$finalModel
)

smearing_factor_lm3 <- mean(
  exp(resid_lm3),
  na.rm = TRUE
)

# #########################
# SUBMISSION
# #########################

submission_lm_3_mejorado <- data.frame(
  property_id = test_model_lm3$property_id,
  price       = exp(pred_log_lm3_c) * smearing_factor_lm3
)

View(submission_lm_3_mejorado)

write.csv(
  submission_lm_3_mejorado,
  here("03_output", "submissions", "Model1_LM_3_mejorado.csv"),
  row.names = FALSE
)

cat("✅ Modelo 12\n")