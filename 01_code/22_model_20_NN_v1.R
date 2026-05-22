###
#NN
grid_nn <- expand.grid(
  size = c(3, 5, 7),  # Número de nodos en la capa oculta
  decay = c(0.1, 0.01) # Regularización para evitar sobreajuste
)

set.seed(2026)
nn_model <- train(
  log_price ~ .,
  data       = train_model_clean,
  method     = "nnet",
  metric     = "MAE",
  trControl  = ctrl_reg,
  tuneGrid   = grid_nn,
  preProcess = c("center", "scale"),
  linout     = TRUE,                
  trace      = FALSE,
  maxit      = 200
)

pred_nn <- predict(nn_model, newdata = test_model_clean)
write.csv(data.frame(property_id = test_model_clean$property_id, price = exp(pred_nn)), 
          here("03_output", "submissions", "Model6_NeuralNetwork.csv"), row.names = FALSE)

cat("✅ Modelo 20\n")