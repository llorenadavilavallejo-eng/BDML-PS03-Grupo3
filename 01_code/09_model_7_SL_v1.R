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
ctrl_sl <- trainControl(
  method          = "cv",
  number          = 5,
  savePredictions = "final",
  verboseIter     = FALSE
)

model_list <- caretList(
  log_price ~ .,
  data       = train_model,
  trControl  = ctrl_sl,
  metric     = "MAE",
  methodList = c("glmnet", "ranger", "gbm")
)

sl_model <- caretEnsemble(
  model_list,
  metric = "MAE"
)

summary(sl_model)

pred_sl <- predict(sl_model, newdata = test_final)

submission_sl <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_sl)
)

write.csv(submission_sl,
          "Model10_SuperLearner_EN_RF_GBM.csv",
          row.names = FALSE)