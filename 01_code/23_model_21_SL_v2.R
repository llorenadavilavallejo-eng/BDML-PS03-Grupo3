###
#SL
# 1. Preparar las matrices para SuperLearner (convertir factores a dummies)
X_train <- model.matrix(log_price ~ . -1, data = train_model_clean)
Y_train <- train_model_clean$log_price

# Para el test, creamos una fórmula dummy sin log_price
test_model_clean$log_price <- 0 
X_test <- model.matrix(log_price ~ . -1, data = test_model_clean)

# =====================================================================
# SOLUCON AL ERROR: Alinear X_test con X_train
# =====================================================================
# 1. Encontrar qué columnas de la matriz de entrenamiento faltan en la de prueba
columnas_faltantes <- setdiff(colnames(X_train), colnames(X_test))

# 2. Si faltan columnas, las creamos llenas de ceros (0)
if(length(columnas_faltantes) > 0) {
  matriz_ceros <- matrix(0, nrow = nrow(X_test), ncol = length(columnas_faltantes))
  colnames(matriz_ceros) <- columnas_faltantes
  X_test <- cbind(X_test, matriz_ceros)
}

# 3. Asegurar que X_test tenga exactamente las mismas columnas y en el mismo orden que X_train
# (Esto también descarta columnas que estén en test pero no en train)
X_test <- X_test[, colnames(X_train), drop = FALSE]
# =====================================================================

# 2. Definir los algoritmos de la librería del SuperLearner
sl_library <- c("SL.lm", "SL.glmnet", "SL.rpart")

# 3. Entrenar el SuperLearner
set.seed(2026)
sl_model <- SuperLearner(
  Y = Y_train,
  X = as.data.frame(X_train),
  SL.library = sl_library,
  family = gaussian(),
  cvControl = list(V = 5)
)

print(sl_model)

# 4. Predicción del SuperLearner (Ahora funcionará sin problemas)
pred_sl_link <- predict(sl_model, newdata = as.data.frame(X_test), onlySL = TRUE)
pred_sl <- pred_sl_link$pred

# Guardar los resultados del ensamble
write.csv(data.frame(property_id = test_model_clean$property_id, price = exp(pred_sl)), 
          here("03_output", "submissions", "Model7_SuperLearner.csv"), row.names = FALSE)

###
#Comparación de rendimiento - No incluye Superlearner
resultados <- resamples(list(
  LM = lm_simple,
  ElasticNet = EN1,
  CART = cart_model,
  RandomForest = rf_model,
  NeuralNet = nn_model
  #SuperL = sl_model
))

# Ver tabla comparativa de MAE y RMSE
summary(resultados)

# Gráfico comparativo
bwplot(resultados, metric = "MAE")

cat("✅ Modelo 21\n")