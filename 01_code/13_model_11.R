####################################
# MODELO 1 MEJORADO: LM 
####################################

#Se realiza un modelo mejorado de regresion lineal,
#con el fin de aumentar la capacidad predictiva y reducir el error MAE en las predicciones de precios inmobiliarios. 
# se incorporó variables estructurales, espaciales y de texto, además de transformaciones logarítmicas e interacciones entre variables.
#adicional se realizo un tratamiento de valores extremos y una corrección de retransfomación para obtener predicciones más estables y precisas.


train_model_lm <- train_final |>
  mutate(
    property_type = as.factor(property_type),
    
    # Transformaciones
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
    
    log_rest_250m = log1p(n_rest_250m),
    
    # Ratio construido / total
    ratio_covered_total = surface_covered / surface_total,
    
    ratio_covered_total = if_else(
      is.finite(ratio_covered_total),
      ratio_covered_total,
      NA_real_
    )
  ) |>
  
  select(
    log_price,
    property_type,
    rooms,
    bedrooms,
    bathrooms,
    
    log_surface_total,
    log_surface_covered,
    ratio_covered_total,
    
    log_dist_parque,
    log_dist_bus,
    log_dist_commerce,
    log_dist_school,
    log_dist_gym,
    log_dist_bank,
    log_dist_health,
    log_dist_police,
    
    log_rest_250m,
    
    tiene_ascensor,
    tiene_gimnasio,
    tiene_bbq,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito
  )

# #########################
# BASE TEST
# #########################

test_model_lm <- test_final |>
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
    
    log_rest_250m = log1p(n_rest_250m),
    
    ratio_covered_total = surface_covered / surface_total,
    
    ratio_covered_total = if_else(
      is.finite(ratio_covered_total),
      ratio_covered_total,
      NA_real_
    )
  ) |>
  
  select(
    property_id,
    property_type,
    rooms,
    bedrooms,
    bathrooms,
    
    log_surface_total,
    log_surface_covered,
    ratio_covered_total,
    
    log_dist_parque,
    log_dist_bus,
    log_dist_commerce,
    log_dist_school,
    log_dist_gym,
    log_dist_bank,
    log_dist_health,
    log_dist_police,
    
    log_rest_250m,
    
    tiene_ascensor,
    tiene_gimnasio,
    tiene_bbq,
    tiene_parqueadero,
    tiene_balcon,
    tiene_deposito
  )

# #########################
# IMPUTACIÓN DE MISSINGS
# #########################

train_model_lm <- train_model_lm |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.),
               median(., na.rm = TRUE),
               .)
    )
  )

test_model_lm <- test_model_lm |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.na(.),
               median(., na.rm = TRUE),
               .)
    )
  )

# #########################
# ELIMINAR OUTLIERS
# #########################

q_low  <- quantile(
  train_model_lm$log_price,
  0.01,
  na.rm = TRUE
)

q_high <- quantile(
  train_model_lm$log_price,
  0.99,
  na.rm = TRUE
)

train_model_lm <- train_model_lm |>
  filter(
    log_price >= q_low,
    log_price <= q_high
  )

# #########################
# MODELO LM
# #########################

set.seed(2026)

lm_mejorado <- train(
  
  log_price ~
    
    property_type +
    
    rooms +
    bedrooms +
    bathrooms +
    
    log_surface_total +
    log_surface_covered +
    
    ratio_covered_total +
    
    log_dist_parque +
    log_dist_bus +
    log_dist_commerce +
    log_dist_school +
    log_dist_gym +
    log_dist_bank +
    log_dist_health +
    log_dist_police +
    
    log_rest_250m +
    
    tiene_ascensor +
    tiene_gimnasio +
    tiene_bbq +
    tiene_parqueadero +
    tiene_balcon +
    tiene_deposito +
    
    log_surface_total:property_type +
    bathrooms:log_surface_total,
  
  data      = train_model_lm,
  method    = "lm",
  trControl = ctrl_reg,
  metric    = "MAE"
)

# Resultados
lm_mejorado
getTrainPerf(lm_mejorado)

# #########################
# PREDICCIONES
# #########################

pred_log_lm <- predict(
  lm_mejorado,
  newdata = test_model_lm |>
    select(-property_id)
)

# Recorte de predicciones
log_low <- quantile(
  train_model_lm$log_price,
  0.01,
  na.rm = TRUE
)

log_high <- quantile(
  train_model_lm$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_lm_c <- pmin(
  pmax(pred_log_lm, log_low),
  log_high
)

# #########################
# SMEARING FACTOR#
###########################

resid_lm <- residuals(
  lm_mejorado$finalModel
)

smearing_factor <- mean(
  exp(resid_lm),
  na.rm = TRUE
)

# #########################
# SUBMISSION
# #########################

submission_lm_mejorado <- data.frame(
  property_id = test_model_lm$property_id,
  price       = exp(pred_log_lm_c) * smearing_factor
)

View(submission_lm_mejorado)

write.csv(
  submission_lm_mejorado,
  "Model1_LM_mejorado.csv",
  row.names = FALSE
)

#para la ejecucion anterior del codigo se pudo mejorar la estructura del precio.con el fin de reducir el error
