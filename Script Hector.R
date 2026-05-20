rm(list = ls())
gc()

library(pacman) 

p_load(rio,        # Importar datos de internet.
       tidyverse,  # Manipulación de datos tabulares.
       sf,         # Leer/escribir/manipular d  atos espaciales.
       tidymodels, # Implementación de modelos de machine learning.
       gt,         # Estadística descriptiva.
       gtsummary,  
       osmdata,    # Obtener datos de OpenStreetMaps. 
       leaflet,    # Visualización de mapas interactivos.
       spatialsample, # Validación cruzada espacial
       stringr,     # Manipulación de texto tidyverse
       stringi,    # Manipulación de texto base R
       dplyr,
       caret
) 
####################################
# CONFIGURACIÓN GENERAL
####################################

# Fijamos semilla para reproducibilidad.
set.seed(2026)

# Importamos las bases
setwd("C:/Users/HECTOR BARRIOS/Documents/Taller 3/Bases")
train<-read.csv("train.csv")
test<-read.csv("test.csv")

# Revisamos estructura de los datos
glimpse(train)
glimpse(test)

dim(train)
dim(test)

names(train)

####################################
# LIMPIEZA Y PROCESAMIENTO
####################################

# Unimos train y test para adjuntar las nuevas variables en conjunto
train <- train |> mutate(dataset = "train")
test  <- test  |> mutate(dataset = "test", price = NA_real_)
full_db <- bind_rows(train, test)

# Tabla descriptiva inicial
train %>%
  select(price, surface_total,surface_covered,rooms,bedrooms,bathrooms) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({min}, {max})"
    ),
    digits = all_continuous() ~ 0,
    label = list(
      price            = "Precio",
      surface_total    = "Área total",
      surface_covered  = "Área construida",
      rooms            = "Número de habitaciones",
      bedrooms         = "Número de alcobas",
      bathrooms        = "Número de baños"
    ),
    missing_text = "(Faltante)"
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  bold_labels()

# Creamos logaritmo del precio
full_db <- full_db |>
  mutate(
    log_price = if_else(dataset == "train",
                        log(price),
                        NA_real_))

# Revisamos tipos de inmueble y faltantes de coordenadas
full_db %>%
  count(property_type)

full_db |>
  summarise(
    nas_lat = sum(is.na(lat)),
    nas_lon = sum(is.na(lon)))

####################################
# GRÁFICAS INICIALES
####################################
#  Distribución del precio
ggplot(
  full_db %>% filter(dataset == "train"),
  aes(x = price)
) +
  geom_histogram(
    bins = 60,
    fill = "steelblue",
    color = "white"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Distribución del precio",
    x = "Precio",
    y = "Frecuencia"
  )

# Distribución log-precio
ggplot(
  full_db %>% filter(dataset == "train"),
  aes(x = log_price)
) +
  geom_histogram(
    bins = 50,
    fill = "darkblue",
    color = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Distribución log-precio",
    x = "Log precio",
    y = "Frecuencia"
  )

####################################
# MAPA
####################################
# Convertimos base a objeto espacial
full_sf <- st_as_sf(full_db,coords = c("lon", "lat"),crs = 4326)

# Visualización simple
ggplot() +
  geom_sf(
    data = full_sf,
    color = "steelblue",
    alpha = 0.5,
    size = 0.3
  ) +
  theme_minimal() +
  labs(
    title = "Ubicación de inmuebles"
  )

# Centro del mapa
latitud_central  <- mean(full_db$lat)
longitud_central <- mean(full_db$lon)

# Mapa e inmuebles
leaflet() %>%
  addTiles() %>%
  setView(
    lng = longitud_central,
    lat = latitud_central,
    zoom = 11
  ) %>%
  addCircleMarkers(
    lng = full_db$lon,
    lat = full_db$lat,
    radius = 2,
    color = "steelblue",
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = paste0(
      "<b>Precio:</b> ",
      scales::comma(full_db$price),
      "<br><b>Tipo:</b> ",
      full_db$property_type
    )
  )

####################################
# VARIABLES OPENSTREETMAP
####################################
# Bounding box de Bogotá
bbox_bogota <- getbb("Bogota Colombia")

# Transformamos inmuebles a metros
# Bogotá se puede trabajar, por ejemplo, con EPSG: 3116 (MAGNA-SIRGAS / Colombia Bogota)
full_sf_m <- st_transform(full_sf, 3116)

# PARQUES
# Descargamos desde OSM
if (!file.exists("osm_parks_raw.rds")) {
  q_parks <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_parks <- add_osm_feature(
    q_parks,
    key = "leisure",
    value = "park"
  )
  Sys.sleep(10)
  osm_parks_raw <- osmdata_sf(q_parks)
  saveRDS(osm_parks_raw, "osm_parks_raw.rds")
} else {
  osm_parks_raw <- readRDS("osm_parks_raw.rds")
}

# Extraemos polígonos
parks_poly <- osm_parks_raw$osm_polygons
parks_poly <- st_as_sf(parks_poly)

# Visualización
leaflet() %>%
  addTiles() %>%
  addPolygons(
    data = parks_poly,
    color = "darkgreen",
    weight = 1,
    opacity = 0.7,
    fillOpacity = 0.3,
    popup = parks_poly$name
  ) %>%
  addCircleMarkers(
    lng = full_db$lon,
    lat = full_db$lat,
    radius = 1,
    color = "steelblue",
    stroke = FALSE,
    fillOpacity = 0.4
  )

# Calculamos centroides
parks_centroids <- st_centroid(
  parks_poly,
  byid = TRUE
)

# Transformamos a metros
parks_centroids_m <- st_transform(
  parks_centroids,
  3116
)

# Distancias
nearest_park <- st_nearest_feature(
  full_sf_m,
  parks_centroids_m
)

dist_park <- st_distance(
  full_sf_m,
  parks_centroids_m[nearest_park, ],
  by_element = TRUE
)

# Variable final
full_db <- mutate(
  full_db,
  dist_parque_m = as.numeric(dist_park)
)

# Gráfica distancia al parque más cercano
ggplot(
  full_db,
  aes(x = dist_parque_m)
) +
  geom_histogram(
    bins = 60,
    fill = "darkgreen",
    color = "white"
  ) +
  theme_minimal() +
  labs(
    title = "Distancia al parque más cercano",
    x = "Distancia (metros)",
    y = "Frecuencia"
  )

# Gráfica distancia al parque más cercano vs precio
ggplot(
  full_db %>%
    filter(dataset == "train") %>%
    sample_n(4000),
  aes(
    x = dist_parque_m,
    y = price
  )
) +
  geom_point(
    alpha = 0.3,
    color = "steelblue"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Distancia al parque vs precio",
    x = "Distancia al parque (m)",
    y = "Precio"
  )

# BUS STOPS
if (!file.exists("osm_bus_raw.rds")) {
  q_bus <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_bus <- add_osm_feature(
    q_bus,
    key = "highway",
    value = "bus_stop"
  )
  Sys.sleep(10)
  osm_bus_raw <- osmdata_sf(q_bus)
  saveRDS(osm_bus_raw, "osm_bus_raw.rds")
} else {
  osm_bus_raw <- readRDS("osm_bus_raw.rds")
}

bus_points <- osm_bus_raw$osm_points
bus_points <- st_as_sf(bus_points)
bus_points_m <- st_transform(bus_points, 3116)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = bus_points,
    radius = 2,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5
  )

# Función para calcular distancia mínima
calc_min_dist <- function(origins, destinations) {
  nearest_index <- st_nearest_feature(
    origins,
    destinations
  )
  nearest_geom <- destinations[nearest_index, ]
  dist_vec <- st_distance(
    origins,
    nearest_geom,
    by_element = TRUE
  )
  as.numeric(dist_vec)
}

full_db <- mutate(
  full_db,
  dist_bus_stop = calc_min_dist(
    full_sf_m,
    bus_points_m
  )
)

# MALLS Y SUPERMERCADOS
if (!file.exists("osm_commerce_raw.rds")) {
  q_commerce <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_commerce <- add_osm_feature(
    q_commerce,
    key = "shop",
    value = c(
      "mall",
      "supermarket"
    )
  )
  Sys.sleep(10)
  osm_commerce_raw <- osmdata_sf(q_commerce)
  saveRDS(
    osm_commerce_raw,
    "osm_commerce_raw.rds"
  )
} else {
  osm_commerce_raw <- readRDS(
    "osm_commerce_raw.rds"
  )
}

commerce_points <- osm_commerce_raw$osm_points
commerce_points <- st_as_sf(commerce_points)
commerce_points_m <- st_transform(
  commerce_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = commerce_points,
    radius = 2,
    color = "darkred",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_commerce = calc_min_dist(
    full_sf_m,
    commerce_points_m
  )
)

# COLEGIOS Y UNIVERSIDADES
if (!file.exists("osm_schools_raw.rds")) {
  q_schools <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_schools <- add_osm_feature(
    q_schools,
    key = "amenity",
    value = c(
      "school",
      "university"
    )
  )
  Sys.sleep(10)
  osm_schools_raw <- osmdata_sf(
    q_schools
  )
  saveRDS(
    osm_schools_raw,
    "osm_schools_raw.rds"
  )
} else {
  osm_schools_raw <- readRDS(
    "osm_schools_raw.rds"
  )
}

schools_points <- osm_schools_raw$osm_points
schools_points <- st_as_sf(
  schools_points
)
schools_points_m <- st_transform(
  schools_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = schools_points,
    radius = 2,
    color = "darkred",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_school = calc_min_dist(
    full_sf_m,
    schools_points_m
  )
)

# RESTAURANTES
# Cantidad de restaurantes dentro de 250 metros.
if (!file.exists("osm_rest_raw.rds")) {
  q_rest <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_rest <- add_osm_feature(
    q_rest,
    key = "amenity",
    value = "restaurant"
  )
  Sys.sleep(10)
  osm_rest_raw <- osmdata_sf(
    q_rest
  )
  saveRDS(
    osm_rest_raw,
    "osm_rest_raw.rds"
  )
} else {
  osm_rest_raw <- readRDS(
    "osm_rest_raw.rds"
  )
}

rest_points <- osm_rest_raw$osm_points
rest_points <- st_as_sf(
  rest_points
)
rest_points_m <- st_transform(
  rest_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = rest_points,
    radius = 2,
    color = "darkgreen",
    stroke = FALSE,
    fillOpacity = 0.5
  )

rest_250m <- st_is_within_distance(
  full_sf_m,
  rest_points_m,
  dist = 250
)

full_db <- mutate(
  full_db,
  n_rest_250m = lengths(
    rest_250m
  )
)

# Histograma restaurantes
ggplot(
  full_db,
  aes(x = n_rest_250m)
) +
  geom_histogram(
    bins = 40,
    fill = "darkgreen",
    color = "white"
  ) +
  theme_minimal()

# GIMNASIOS
if (!file.exists("osm_gyms_raw.rds")) {
  q_gyms <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_gyms <- add_osm_feature(
    q_gyms,
    key = "leisure",
    value = "fitness_centre"
  )
  Sys.sleep(10)
  osm_gyms_raw <- osmdata_sf(
    q_gyms
  )
  saveRDS(
    osm_gyms_raw,
    "osm_gyms_raw.rds"
  )
} else {
  osm_gyms_raw <- readRDS(
    "osm_gyms_raw.rds"
  )
}

gyms_points <- osm_gyms_raw$osm_points
gyms_points <- st_as_sf(
  gyms_points
)
gyms_points_m <- st_transform(
  gyms_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = gyms_points,
    radius = 2,
    color = "darkred",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_gym = calc_min_dist(
    full_sf_m,
    gyms_points_m
  )
)

# BANCOS Y ATM
if (!file.exists("osm_bank_raw.rds")) {
  q_bank <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_bank <- add_osm_feature(
    q_bank,
    key = "amenity",
    value = c(
      "atm",
      "bank"
    )
  )
  Sys.sleep(10)
  osm_bank_raw <- osmdata_sf(
    q_bank
  )
  saveRDS(
    osm_bank_raw,
    "osm_bank_raw.rds"
  )
} else {
  osm_bank_raw <- readRDS(
    "osm_bank_raw.rds"
  )
}

bank_points <- osm_bank_raw$osm_points

bank_points <- st_as_sf(
  bank_points
)

bank_points_m <- st_transform(
  bank_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = bank_points,
    radius = 2,
    color = "darkred",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_bank = calc_min_dist(
    full_sf_m,
    bank_points_m
  )
)

# HOSPITALES Y CLÍNICAS
if (!file.exists("osm_health_raw.rds")) {
  q_health <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_health <- add_osm_feature(
    q_health,
    key = "amenity",
    value = c(
      "clinic",
      "hospital"
    )
  )
  Sys.sleep(10)
  osm_health_raw <- osmdata_sf(
    q_health
  )
  saveRDS(
    osm_health_raw,
    "osm_health_raw.rds"
  )
} else {
  osm_health_raw <- readRDS(
    "osm_health_raw.rds"
  )
}

health_points <- osm_health_raw$osm_points

health_points <- st_as_sf(
  health_points
)

health_points_m <- st_transform(
  health_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = health_points,
    radius = 2,
    color = "darkblue",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_health = calc_min_dist(
    full_sf_m,
    health_points_m
  )
)

# POLICÍA
if (!file.exists("osm_police_raw.rds")) {
  q_police <- opq(
    bbox = bbox_bogota,
    timeout = 120
  )
  q_police <- add_osm_feature(
    q_police,
    key = "amenity",
    value = "police"
  )
  Sys.sleep(10)
  osm_police_raw <- osmdata_sf(
    q_police
  )
  saveRDS(
    osm_police_raw,
    "osm_police_raw.rds"
  )
} else {
  osm_police_raw <- readRDS(
    "osm_police_raw.rds"
  )
}

police_points <- osm_police_raw$osm_points

police_points <- st_as_sf(
  police_points
)

police_points_m <- st_transform(
  police_points,
  3116
)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(
    data = police_points,
    radius = 2,
    color = "gray",
    stroke = FALSE,
    fillOpacity = 0.5
  )

full_db <- mutate(
  full_db,
  dist_police = calc_min_dist(
    full_sf_m,
    police_points_m
  )
)

full_db <- mutate(
  full_db,
  across(
    starts_with("dist_"),
    as.numeric
  )
)

summary(
  select(
    full_db,
    dist_parque_m,
    dist_bus_stop,
    dist_school,
    n_rest_250m,
    dist_gym,
    dist_bank,
    dist_health,
    dist_police,
    dist_commerce
  )
)

####################################
# VARIABLES DE TEXTO
####################################

# Pasamos todo a minúsculas
full_base <- full_db |>
  mutate(
    title_lower = str_to_lower(title),
    desc_lower  = str_to_lower(description),
    text_all    = paste(title_lower, desc_lower, sep = " ")
  )

# Amenities
full_base <- full_base |>
  mutate(
    tiene_ascensor = if_else(str_detect(text_all, "ascensor"), 1, 0),
    tiene_gimnasio = if_else(str_detect(text_all, "gimnasio|gym"), 1, 0),
    tiene_bbq      = if_else(str_detect(text_all, "bbq|parrilla"), 1, 0),
    tiene_parqueadero   = if_else(str_detect(text_all, "parqueadero|garaje|parking"), 1, 0),
    tiene_balcon   = if_else(str_detect(text_all, "balc[oó]n"), 1, 0),
    tiene_deposito = if_else(str_detect(text_all, "deposito|dep[oó]sito|bodega"), 1, 0)
  )

####################################
# PREPARACIÓN FINAL
####################################

# Volver a separar train y test 
train_final <- full_base |> filter(dataset == "train") |> select(-dataset)
test_final  <- full_base |> filter(dataset == "test")  |> select(-dataset, -price, -log_price)

# Convertimos variables a formatos correctos
train_final <- train_final |>
  mutate(
    property_type = as.factor(property_type),
    across(starts_with("tiene_"), as.numeric)
  )

test_final <- test_final |>
  mutate(
    property_type = as.factor(property_type),
    across(starts_with("tiene_"), as.numeric)
  )

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

####################################
# MODELOS
####################################
# Modelo 1: LM

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

View(submission_lm_simple)

write.csv(submission_lm_simple,
          "Model1_LM.csv",
          row.names = FALSE)

# Modelo 2: ELASTIC NET

grid_EN1 <- expand.grid(
  alpha  = seq(0, 1, by = 0.5),
  lambda = 10^seq(-3, -1, length = 10)
)

set.seed(2026)
EN1 <- train(
  log_price ~ .,
  data       = train_model,
  method     = "glmnet",
  family     = "gaussian",
  metric     = "MAE",
  trControl  = ctrl_reg,
  preProcess = c("center", "scale"),
  tuneGrid   = grid_EN1
)

EN1
EN1$bestTune

pred_EN1 <- predict(EN1, newdata = test_final)

submission_EN1 <- data.frame(
  property_id = test_final$property_id,
  price       = exp(pred_EN1)
)

lambda_str1 <- gsub("\\.", "_", as.character(round(EN1$bestTune$lambda, 6)))
alpha_str1  <- gsub("\\.", "_", as.character(EN1$bestTune$alpha))

name_EN1 <- paste0("Model2_EN1_lambda_", lambda_str1,
                   "_alpha_", alpha_str1, ".csv")

write.csv(submission_EN1, name_EN1, row.names = FALSE)
