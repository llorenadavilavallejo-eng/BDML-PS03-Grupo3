####################################
# MODELO 2: ELASTIC NET 2 MEJORADO
####################################

# Usamos la misma base mejorada del LM 3
# Utilizamos como´apoyo  a train_model_lm3 y test_model_lm3 que es base de datos final mejorada que realizamos 
#En los modelos lineales 

train_model_en2 <- train_model_lm3
test_model_en2  <- test_model_lm3

# ########################
# GRID MEJORADO
# ########################

grid_EN2 <- expand.grid(
  alpha  = seq(0, 1, by = 0.1),
  lambda = 10^seq(-5, 1, length = 80)
)

# ########################
# MODELO ELASTIC NET
# ########################

set.seed(2026)

EN2_mejorado <- train(
  log_price ~ .,
  data       = train_model_en2,
  method     = "glmnet",
  family     = "gaussian",
  metric     = "MAE",
  trControl  = ctrl_reg,
  preProcess = c("center", "scale"),
  tuneGrid   = grid_EN2
)

EN2_mejorado
EN2_mejorado$bestTune
getTrainPerf(EN2_mejorado)

# ##########################
# PREDICCIONES
# ##########################

pred_log_EN2 <- predict(
  EN2_mejorado,
  newdata = test_model_en2 |> select(-property_id)
)

# Recorte prudente por percentiles
log_low_EN2 <- quantile(
  train_model_en2$log_price,
  0.01,
  na.rm = TRUE
)

log_high_EN2 <- quantile(
  train_model_en2$log_price,
  0.99,
  na.rm = TRUE
)

pred_log_EN2_c <- pmin(
  pmax(pred_log_EN2, log_low_EN2),
  log_high_EN2
)

# ########################
# SMEARING FACTOR
# #######################

pred_train_EN2 <- predict(
  EN2_mejorado,
  newdata = train_model_en2
)

resid_EN2 <- train_model_en2$log_price - pred_train_EN2

smearing_factor_EN2 <- mean(
  exp(resid_EN2),
  na.rm = TRUE
)

# #########################
# SUBMISSION
# #########################

submission_EN2_mejorado <- data.frame(
  property_id = test_model_en2$property_id,
  price       = exp(pred_log_EN2_c) * smearing_factor_EN2
)

View(submission_EN2_mejorado)

lambda_str_EN2 <- gsub(
  "\\.",
  "_",
  as.character(round(EN2_mejorado$bestTune$lambda, 8))
)

alpha_str_EN2 <- gsub(
  "\\.",
  "_",
  as.character(EN2_mejorado$bestTune$alpha)
)

name_EN2 <- paste0(
  "Model2_EN_2_mejorado_lambda_",
  lambda_str_EN2,
  "_alpha_",
  alpha_str_EN2,
  ".csv"
)

write.csv(
  submission_EN2_mejorado,
  here("03_output", "submissions", name_EN2),
  row.names = FALSE
)

#El resultado del la 2 version del modelo elastic net mejor a diferencia de la primera prediccion del modelo
#sin sembargo el modelo ML sigue siendo el mejor

cat("✅ Modelo 13\n")