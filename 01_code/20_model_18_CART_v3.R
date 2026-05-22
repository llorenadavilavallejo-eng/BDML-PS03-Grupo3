
# Imputación simple de NAs (en caso de que bedrooms o distancias tengan datos faltantes)
train_model_clean <- train_model
test_model_clean <- test_model

# Imputamos la mediana para variables numéricas con NAs
for(col in num_vars) {
  if(any(is.na(train_model_clean[[col]]))) {
    med_val <- median(train_model_clean[[col]], na.rm = TRUE)
    train_model_clean[[col]][is.na(train_model_clean[[col]])] <- med_val
    test_model_clean[[col]][is.na(test_model_clean[[col]])] <- med_val
  }
}

###
#CART
grid_cart <- expand.grid(cp = seq(0.001, 0.05, by = 0.005))

set.seed(2026)
cart_model <- train(
  log_price ~ .,
  data      = train_model_clean,
  method    = "rpart",
  metric    = "MAE",
  trControl = ctrl_reg,
  tuneGrid  = grid_cart
)

pred_cart <- predict(cart_model, newdata = test_model_clean)
write.csv(data.frame(property_id = test_model_clean$property_id, price = exp(pred_cart)), 
          here("03_output", "submissions", "Model3_CART.csv"), row.names = FALSE)

cat("✅ Modelo 18\n")