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

grid_cart2 <- expand.grid(
  cp = c(0.002, 0.004, 0.0075, 0.01, 0.02)
)

set.seed(2026)
cart_reg2 <- train(
  log_price ~ .,
  data      = train_model,
  method    = "rpart",
  trControl = ctrl_reg,
  tuneGrid  = grid_cart2,
  metric    = "MAE"
)

cart_reg2
cart_reg2$bestTune

pred_cart2 <- predict(cart_reg2, newdata = test_final)

submission_cart2 <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_cart2)
)

cp_str2  <- gsub("\\.", "_", as.character(round(cart_reg2$bestTune$cp, 4)))
file_cart2 <- paste0("Model4_CART2_cp_", cp_str2, ".csv")

write.csv(
  submission_cart2,
  here("03_output","submissions", file_cart2),
  row.names = FALSE
)
