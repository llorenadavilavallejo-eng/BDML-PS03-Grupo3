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

grid_rf2 <- expand.grid(
  mtry          = c(6, 9),
  splitrule     = "variance",
  min.node.size = c(3, 8, 15)
)

set.seed(2026)
rf2 <- train(
  log_price ~ .,
  data       = train_model,
  method     = "ranger",
  trControl  = ctrl_reg,
  metric     = "MAE",
  tuneGrid   = grid_rf2,
  num.trees  = 500,
  importance = "impurity"
)

rf2
rf2$bestTune

pred_rf2 <- predict(rf2, newdata = test_final)

submission_rf2 <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_rf2)
)

file_rf2 <- paste0(
  "Model7_RF2_mtry_", rf2$bestTune$mtry,
  "_minNode_", rf2$bestTune$min.node.size,
  "_ntree_500.csv"
)

write.csv(
  submission_rf2,
  here("03_output","submissions", file_rf2),
  row.names = FALSE
)

cat("✅ Modelo 5\n")