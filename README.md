# BDML-PS03-Grupo3
Big Data and Machine Learning – Universidad de los Andes – Problem Set 3: Spatial and Machine Learning Models for Housing Price Prediction in Bogotá

En este repositorio encontrará la solución del Problem set 3 correspondiente al mejor modelo para predecir los precios de la vivienda en Chapinero, Bogotá.

## Autores

-   Leidy Lorena Dávila Vallejo - COD: 202522776
-   Juan Guillermo Sánchez - COD: 202323123
-   Héctor Steben Barrios Carranza - COD: 202116184

## Comentarios

Los datos están incluidos en este repositorio.

## Replicación

Para reproducir todos los resultados, una vez descargados y ubicados los datos, correr:

`source("01_code/00_rundirectory.R")`

## Estructura de código

-   `01_code/00_rundirectory.R`: Master script. Reproduce todos los códigos y resultados.
-   `01_code/01_setup_packages.R`: Carga e instala los paquetes necesarios.
-   `01_code/02_load_and_preprare_data.R`: Llama, transforma y realiza la limpieza necesaria a los datos.
-   `01_code/03_model_1_LM_v1.R`: Estimación del modelo lineal
-   `01_code/04_model_2_EN_v1.R`: Estimación del modelo elastic net.
-   `01_code/05_model_3_CART_v1.R`: Estimación del modelo de árbol o CART.
-   `01_code/06_model_4_RF_v1.R`: Estimación del modelo random forest.
-   `01_code/07_model_5_RF_v2.R`: Estimación del modelo random forest alternativo.
-   `01_code/08_model_6_GBM_v1.R`: Estimación del modelo gradient boosting.
-   `01_code/09_model_7_SL_v1.R`: Estimación del modelo super learner.
-   `01_code/10_model_8_RF_spatial_v3.R`: Estimación del modelo random forest con CV spatial.
-   `01_code/11_model_9_EN_spatial_v2.R`: Estimación del modelo elastic net con CV spatial.
-   `01_code/12_model_10_GBM_spatial_v2.R`: Estimación del modelo gradient boosting con CV spatial.
-   `01_code/13_model_11_LM_v2.R`: Estimación del modelo lineal mejorado.
-   `01_code/14_model_12_LM_v3.R`: Estimación del modelo lineal mejorado alternativo.
-   `01_code/15_model_13_EN_v3.R`: Estimación del modelo elastic net mejorado.
-   `01_code/16_model_14_EN_v4.R`: Estimación del modelo elastic net mejorado alternativo.
-   `01_code/17_model_15_EN_v5.R`: Estimación del modelo elastic net mejorado alternativo.
-   `01_code/18_model_16_CART_v2.R`: Estimación del modelo de árbol o CART mejorado.
-   `01_code/19_model_17_RF_v4.R`: Estimación del modelo random forest alternativo.
-   `01_code/20_model_18_CART_v3.R`: Estimación del modelo de árbol o CART mejorado alternativo.
-   `01_code/21_model_19_RF_v5.R`: Estimación del modelo random forest alternativo.
-   `01_code/22_model_20_NN_v1.R`: Estimación del modelo red neuronal.
-   `01_code/23_model_21_SL_v2.R`: Estimación del modelo super learner alternativo.
-   `01_code/24_model_22_GBM_v3.R`: Estimación del modelo gradient boosting.
-   `01_code/25_model_23_GBM_v4.R`: Estimación del modelo gradient boosting.
-   `01_code/26_model_24_GBM_spatial_v5.R`: Estimación del modelo gradient boosting con CV spatial.
-   `01_code/27_statistics_of_maps.R`: Crea tablas, gráficas y mapas sobre los resultados y los cálculos de los modelos.

## Salidas

Todos los outputs se generan en `02_outputs/`.

-   Figures (`03_outputs/figures/`): visualizaciones generadas por el código
-   Submissions (`03_outputs/submissions/`): archivos con los resultados de los modelos para subir a Kaggle
-   Tables (`03_outputs/tables/`): resultados de estimaciones en formato `.tex` y `.html`
-   Openstreetmap (`03_outputs/openstreetmap/`): archivos descargados de openstreetmap para obtener datos de georreferenciación

## Software / entorno

-   R version 4.5.1
-   Required packages: Pacman
