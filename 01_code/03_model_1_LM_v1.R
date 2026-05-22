# Seleccionamos las variables finales
train_model <- train_final |>
  select(
    log_price, bedrooms, property_type,
    dist_parque_m, dist_bus_stop, dist_commerce, dist_school, n_rest_250m,
    dist_gym, dist_bank, dist_health, dist_police, tiene_ascensor, tiene_gimnasio, 
    tiene_bbq, tiene_parqueadero, tiene_balcon, tiene_deposito
  )

test_model <- test_final |>
  select(
    property_id, bedrooms, property_type,
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

ctrl_reg <- trainControl(
  method          = "cv",
  number          = 5,
  verboseIter     = FALSE,
  savePredictions = "final"
)

lm_simple <- train(
  log_price ~ .,
  data      = train_model,
  method    = "lm",
  trControl = ctrl_reg,
  metric = "MAE"
)

lm_simple
getTrainPerf(lm_simple)

pred_lm_simple <- predict(lm_simple, newdata = test_model)
log_min <- min(train_model$log_price, na.rm = TRUE)
log_max <- max(train_model$log_price, na.rm = TRUE)
pred_lm_c <- pmin(pmax(pred_lm_simple, log_min), log_max)

submission_lm_simple <- data.frame(
  property_id = test_model$property_id,
  price       = exp(pred_lm_c)
)

write.csv(
  submission_lm_simple,
  here("03_output", "submissions", "Model1_LM.csv"),
  row.names = FALSE
)

cat("✅ Modelo 1\n")