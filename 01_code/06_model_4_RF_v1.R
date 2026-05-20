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

grid_rf1 <- expand.grid(
  mtry          = c(8, 11, 15),
  splitrule     = "variance",
  min.node.size = c(5, 10, 20)
)

set.seed(2026)
rf1 <- train(
  log_price ~ .,
  data       = train_model,
  method     = "ranger",
  trControl  = ctrl_reg,
  metric     = "MAE",
  tuneGrid   = grid_rf1,
  num.trees  = 300,
  importance = "impurity"
)

rf1
rf1$bestTune

pred_rf1 <- predict(rf1, newdata = test_final)

submission_rf1 <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_rf1)
)

file_rf1 <- paste0(
  "Model6_RF1_mtry_", rf1$bestTune$mtry,
  "_minNode_", rf1$bestTune$min.node.size,
  "_ntree_300.csv"
)

write.csv(
  submission_rf1,
  here("03_output","submissions", file_rf1),
  row.names = FALSE
)
