####################################
# MODELO 2: ELASTIC NET 3 MEJORADO
####################################

#incorporamos nuevas interacciones espaciales y económicas, variables de densidad de amenidades asociados
#se implementa un proceso de regularización más robusto mediante una búsqueda más amplia de hiperparámetros alpha y lambda con el fin de validar
#si se mejoro el modelo 

# ##########################
# BASE FINAL EN3
# ##########################

train_model_en3 <- train_model_lm3 |>
  
  mutate(
    
    # Interacciones espaciales adicionales
    lat_lon = lat * lon,
    
    # Curvaturas espaciales
    lat2 = lat^2,
    lon2 = lon^2,
    
    # Interacciones económicas
    bath_bed = bathrooms * bedrooms,
    
    # Densidad total de amenidades
    amenity_density =
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m +
      
      log_rest_250m
  )

test_model_en3 <- test_model_lm3 |>
  
  mutate(
    
    lat_lon = lat * lon,
    
    lat2 = lat^2,
    lon2 = lon^2,
    
    bath_bed = bathrooms * bedrooms,
    
    amenity_density =
      log_parks_500m +
      log_bus_300m +
      log_commerce_500m +
      
      log_rest_250m
  )

# ############################
# ELIMINAR VARIABLES MUY RUIDOSAS
# ############################

remove_vars <- c()

train_model_en3 <- train_model_en3 |>
  select(-all_of(remove_vars))

test_model_en3 <- test_model_en3 |>
  select(-all_of(remove_vars))

# ###########################
# GRID MUCHO MÁS FINO
# ###########################

grid_EN3 <- expand.grid(
  
  alpha = seq(0, 1, by = 0.05),
  
  lambda = 10^seq(
    -6,
    1,
    length = 120
  )
)

##############################
# CONTROL CV
##############################

ctrl_en3 <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final"
)

# ########################
# MODELO ELASTIC NET 3
# ########################

set.seed(2026)

EN3_mejorado <- train(
  
  log_price ~ .,
  
  data = train_model_en3,
  
  method = "glmnet",
  
  family = "gaussian",
  
  metric = "MAE",
  
  tuneGrid = grid_EN3,
  
  trControl = ctrl_en3,
  
  preProcess = c(
    "center",
    "scale"
  )
)

# #########################
# RESULTADOS
# #########################
EN3_mejorado
EN3_mejorado$bestTune
getTrainPerf(EN3_mejorado)

# ########################
# PREDICCIONES
# ########################

pred_log_EN3 <- predict(
  EN3_mejorado,
  newdata = test_model_en3 |>
    select(-property_id)
)

# #########################
# RECORTE PRUDENTE
# #########################

log_low_EN3 <- quantile(
  train_model_en3$log_price,
  0.01,
  na.rm = TRUE
)

log_high_EN3 <- quantile(
  train_model_en3$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_EN3_c <- pmin(
  pmax(
    pred_log_EN3,
    log_low_EN3
  ),
  log_high_EN3
)

# ########################
# SMEARING FACTOR
# ########################

pred_train_EN3 <- predict(
  EN3_mejorado,
  newdata = train_model_en3
)

resid_EN3 <-
  train_model_en3$log_price -
  pred_train_EN3

smearing_factor_EN3 <- mean(
  exp(resid_EN3),
  na.rm = TRUE
)

# ###########################
# SUBMISSION
# ###########################

submission_EN3_mejorado <- data.frame(
  
  property_id =
    test_model_en3$property_id,
  
  price =
    exp(pred_log_EN3_c) *
    smearing_factor_EN3
)

View(submission_EN3_mejorado)

# ########################
# NOMBRE DINÁMICO
# ########################

lambda_str_EN3 <- gsub(
  "\\.",
  "_",
  as.character(
    round(
      EN3_mejorado$bestTune$lambda,
      8
    )
  )
)

alpha_str_EN3 <- gsub(
  "\\.",
  "_",
  as.character(
    EN3_mejorado$bestTune$alpha
  )
)

name_EN3 <- paste0(
  
  "Model2_EN_3_mejorado_lambda_",
  
  lambda_str_EN3,
  
  "_alpha_",
  
  alpha_str_EN3,
  
  ".csv"
)

write.csv(
  
  submission_EN3_mejorado,
  
  here("03_output", "submissions", "name_EN3"),
  
  row.names = FALSE
)

cat("✅ Modelo 14\n")