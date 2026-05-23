##########################################################
# Master script
#
# Running this file reproduces all results in the repository.
#
# To reproduce all results, run:
# from an interactive R session: source("01_code/00_rundirectory.R")   
# or from the command line: R CMD BATCH 01_code/00_rundirectory.R
#
# Authors:
#
# - Leidy Lorena Dávila Vallejo
# - Juan Guillermo Sánchez
# - Héctor Steben Barrios Carranza
##########################################################

# Paso 0: Limpieza espacio de trabajo.
cat("\014")
rm(list = ls())

# Paso 1: Carga e instala los paquetes necesarios.
source("01_code/01_setup_packages.R")

# Paso 2: Llama, transforma y realiza la limpieza necesaria de la base de datos
source("01_code/02_load_and_prepare_data.R")

# Paso 3: Estimación del modelo lineal.
source("01_code/03_model_1_LM_v1.R")

# Paso 4: Estimación del modelo elastic net.
source("01_code/04_model_2_EN_v1.R")

# Paso 5: Estimación del modelo de árbol o CART.
source("01_code/05_model_3_CART_v1.R")

# Paso 6: Estimación del modelo random forest.
source("01_code/06_model_4_RF_v1.R")

# Paso 7: Estimación del modelo random forest alternativo.
source("01_code/07_model_5_RF_v2.R")

# Paso 8: Estimación del modelo gradient boosting.
source("01_code/08_model_6_GBM_v1.R")

# Paso 9: Estimación del modelo super learner.
source("01_code/09_model_7_SL_v1.R")

# Paso 10: Estimación del modelo random forest con CV spatial.
source("01_code/10_model_8_RF_spatial_v3.R")

# Paso 11: Estimación del modelo elastic net con CV spatial.
source("01_code/11_model_9_EN_spatial_v2.R")

# Paso 12: Estimación del modelo gradient boosting con CV spatial.
source("01_code/12_model_10_GBM_spatial_v2.R")

# Paso 13: Estimación del modelo lineal mejorado.
source("01_code/13_model_11_LM_v2.R")

# Paso 14: Estimación del modelo lineal mejorado alternativo.
source("01_code/14_model_12_LM_v3.R")

# Paso 15: Estimación del modelo elastic net mejorado.
source("01_code/15_model_13_EN_v3.R")

# Paso 16: Estimación del modelo elastic net mejorado alternativo.
source("01_code/16_model_14_EN_v4.R")

# Paso 17: Estimación del modelo elastic net mejorado alternativo.
source("01_code/17_model_15_EN_v5.R")

# Paso 18: Estimación del modelo de árbol o CART mejorado.
source("01_code/18_model_16_CART_v2.R")

# Paso 19: Estimación del modelo random forest alternativo.
source("01_code/19_model_17_RF_v4.R")

# Paso 20: Estimación del modelo de árbol o CART mejorado alternativo.
source("01_code/20_model_18_CART_v3.R")

# Paso 21: Estimación del modelo random forest alternativo.
source("01_code/21_model_19_RF_v5.R")

# Paso 22: Estimación del modelo red neuronal.
source("01_code/22_model_20_NN_v1.R")

# Paso 23: Estimación del modelo super learner alternativo.
source("01_code/23_model_21_SL_v2.R")

# Paso 24: Estimación del modelo gradient boosting 
source("01_code/24_model_22_GBM_v3.R")

# Paso 25: Estimación del modelo gradient boosting 
source("01_code/25_model_23_GBM_v4.R")

# Paso 26: Estimación del modelo gradient boosting con CV spatial
source("01_code/26_model_24_GBM_spatial_v5.R")

# Paso 27: Crea tablas, gráficas y mapas sobre los resultados y los cálculos de los modelos.
source("01_code/27_statistics_and_maps.R")
