#############################
# Tabla descriptiva datos
#############################
tabla_descriptiva <- train %>%
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

tabla_gt <- tabla_descriptiva %>%
  as_gt() %>%
  tab_header(
    title = md("**Tabla descriptiva de variables**")
  ) %>%
  opt_table_font(
    font = list(
      gt::google_font("Roboto")
    )
  )

gtsave(
  data = tabla_gt,
  filename = "tabla_descriptiva.png",
  path = "03_output/tables"
)

####################################
# GRÁFICAS INICIALES
####################################
#  Distribución del precio
dist_precio <- ggplot(
  full_db %>% 
    filter(dataset == "train"),
  aes(x = price)
) +
  geom_histogram(
    bins = 60,
    fill = "#2C7FB8",
    color = "white",
    linewidth = 0.2,
    alpha = 0.95
  ) +
  scale_x_continuous(
    labels = label_number(
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  labs(
    title = "Distribución del precio de los inmuebles",
    subtitle = "Base de entrenamiento",
    x = "Precio",
    y = "Número de inmuebles",
    caption = "Fuente: elaboración propia"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#1A1A1A",
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 14,
      color = "#4D4D4D",
      margin = margin(b = 15)
    ),
    axis.title = element_text(
      size = 15,
      face = "bold"
    ),
    axis.text = element_text(
      size = 12,
      color = "#333333"
    ),
    panel.grid.minor = element_blank(),
    
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.4
    ),
    plot.caption = element_text(
      size = 10,
      color = "gray40",
      hjust = 1
    ),
    plot.margin = margin(
      t = 15,
      r = 20,
      b = 15,
      l = 20
    )
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "dist_precio.png"
  ),
  plot = dist_precio,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

# Distribución log-precio
dist_log_precio <- ggplot(
  full_db %>% filter(dataset == "train"),
  aes(x = log_price)
) +
  geom_histogram(
    bins = 50,
    fill = "#08306B",
    color = "white",
    linewidth = 0.2,
    alpha = 0.95
  ) +
  labs(
    title = "Distribución del logaritmo del precio",
    x = "Log precio",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#1A1A1A",
      hjust = 0
    ),
    axis.title = element_text(
      size = 15,
      face = "bold"
    ),
    axis.text = element_text(
      size = 12,
      color = "#333333"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "#E5E5E5",
      linewidth = 0.4
    ),
    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 15
    )
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "dist_log_precio.png"
  ),
  plot = dist_log_precio,
  width = 12,
  height = 8,
  dpi = 400,
  bg = "white"
)

# Ubicaciones inmuebles
mapa_inmuebles <- ggplot() +
  geom_sf(
    data = full_sf,
    color = "#2C7FB8",
    alpha = 0.45,
    size = 0.25
  ) +
  labs(
    title = "Ubicación de los inmuebles"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    panel.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.title = element_text(
      size = 22,
      face = "bold",
      color = "#1A1A1A",
      hjust = 0
    ),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 10,
      b = 10,
      l = 10
    )
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "mapa_inmuebles.png"
  ),
  plot = mapa_inmuebles,
  width = 10,
  height = 10,
  dpi = 400,
  bg = "white"
)

####################################
# MAPAS
####################################
# Mapa inmuebles
mapa_leaflet <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(
    lng = longitud_central,
    lat = latitud_central,
    zoom = 12
  ) %>%
  addCircleMarkers(
    lng = full_db$lon,
    lat = full_db$lat,
    radius = 2,
    color = "#2C7FB8",
    stroke = FALSE,
    fillOpacity = 0.45
  )

htmlwidgets::saveWidget(
  mapa_leaflet,
  file = here("03_output", "figures", "mapa_inmuebles.html"),
  selfcontained = TRUE
)

webshot2::webshot(
  url = here("03_output", "figures", "mapa_inmuebles.html"),
  file = here("03_output", "figures", "mapa_inmuebles.png"),
  vwidth = 1600,
  vheight = 1000,
  zoom = 2
)

####################################
# GRÁFICAS COMPLEMENTARIAS
####################################
# Histograma distancia al parque
graf_dist_parque <- ggplot(
  full_db,
  aes(x = dist_parque_m)
) +
  geom_histogram(
    bins = 60,
    fill = "#1B7837",
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distancia al parque más cercano",
    x = "Distancia (metros)",
    y = "Número de inmuebles"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(
      size = 13
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "distancia_parque_histograma.png"
  ),
  plot = graf_dist_parque,
  width = 10,
  height = 7,
  dpi = 400,
  bg = "white"
)

# Scatter distancia parque vs precio
graf_parque_precio <- ggplot(
  full_db %>%
    filter(dataset == "train") %>%
    sample_n(4000),
  aes(
    x = dist_parque_m,
    y = price
  )
) +
  geom_point(
    alpha = 0.28,
    color = "#2C7FB8",
    size = 1.7
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#D95F0E",
    linewidth = 1.3
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Distancia al parque y precio del inmueble",
    x = "Distancia al parque (metros)",
    y = "Precio"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(
      size = 13
    ),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "distancia_parque_vs_precio.png"
  ),
  plot = graf_parque_precio,
  width = 10,
  height = 7,
  dpi = 400,
  bg = "white"
)


# Histograma restaurantes
graf_restaurantes <- ggplot(
  full_db,
  aes(x = n_rest_250m)
) +
  geom_histogram(
    bins = 40,
    fill = "#1B7837",
    color = "white",
    linewidth = 0.2
  ) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Número de restaurantes en 250 metros",
    x = "Cantidad de restaurantes",
    y = "Número de inmuebles"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 22,
      hjust = 0.5
    ),
    axis.title = element_text(
      face = "bold",
      size = 16
    ),
    axis.text = element_text(
      size = 13
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

ggsave(
  filename = here(
    "03_output",
    "figures",
    "hist_restaurantes_250m.png"
  ),
  plot = graf_restaurantes,
  width = 10,
  height = 7,
  dpi = 400,
  bg = "white"
)


####Cuadro comparativo#############################################
mae_regular  <- min(gbm_mejorado_liviano$results$MAE)
rmse_regular <- min(gbm_mejorado_liviano$results$RMSE)

mae_spatial  <- min(gbm_mejorado_spatial$results$MAE)
rmse_spatial <- min(gbm_mejorado_spatial$results$RMSE)

comparacion_cv <- data.frame(
  Validacion = c("Random CV","Spatial CV"),
  MAE = c(mae_regular,mae_spatial),
  RMSE = c(rmse_regular,rmse_spatial)
)

comparacion_cv <- comparacion_cv |>
  mutate(
    MAE = round(MAE,3),
    RMSE = round(RMSE,3)
  )

comparacion_cv

gap_pct <- round(
  ((mae_spatial - mae_regular) / mae_regular) * 100,
  1
)

gap_pct

graf_mae <- ggplot(
  comparacion_cv,
  aes(
    x = Validacion,
    y = MAE,
    fill = Validacion
  )
) +
  
  geom_col(width = 0.6) +
  
  geom_text(
    aes(label = round(MAE,3)),
    vjust = -0.5,
    size = 5
  ) +
  
  ylim(0,0.26) +
  
  labs(
    title = "Regular CV vs Spatial CV",
    subtitle = paste0(
      "Spatial validation increases MAE by ",
      gap_pct,
      "%"
    ),
    x = "",
    y = "MAE"
  ) +
  
  theme_minimal(base_size = 14)

graf_mae

imp <- varImp(gbm_mejorado_spatial)$importance |>
  rownames_to_column("variable") |>
  arrange(desc(Overall)) |>
  slice(1:15)

imp

graf_imp <- ggplot(
  imp,
  aes(
    x = reorder(variable, Overall),
    y = Overall
  )
) +
  
  geom_col() +
  
  coord_flip() +
  
  labs(
    title = "Top 15 Most Important Variables",
    subtitle = "GBM + Spatial CV",
    x = "",
    y = "Importance"
  ) +
  
  theme_minimal(base_size = 13)

graf_imp


fold_map <- train_sf |>
  mutate(
    fold = NA
  )

for(i in 1:length(spatial_folds$splits)) {
  
  test_ids <- setdiff(
    seq_len(nrow(train_sf)),
    spatial_folds$splits[[i]]$in_id
  )
  
  fold_map$fold[test_ids] <- paste0("Fold ", i)
}

####################################################
# GRÁFICA
####################################################

ggplot(fold_map) +
  
  geom_sf(aes(color = fold), size = 1) +
  
  labs(
    title = "Spatial Cross-Validation Folds",
    subtitle = "Validation sets are geographically separated",
    color = "Fold"
  ) +
  
  theme_minimal(base_size = 14)

######################################################
# TABLA DESCRIPTIVA


tabla_desc_gbm <- train_model_gbm1 |>
  select(
    log_price,
    log_surface_total,
    bedrooms,
    bathrooms,
    lat,
    lon,
    log_dist_parque,
    log_dist_bus,
    log_dist_commerce,
    log_rest_250m,
    amenity_density,
    surface_bath,
    surface_bed
  ) |>
  summarise(
    across(
      everything(),
      list(
        Media = ~ mean(.x, na.rm = TRUE),
        Desv_Est = ~ sd(.x, na.rm = TRUE),
        Minimo = ~ min(.x, na.rm = TRUE),
        Maximo = ~ max(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_pattern = "(.+)_(Media|Desv_Est|Minimo|Maximo)"
  ) |>
  mutate(
    across(
      c(Media, Desv_Est, Minimo, Maximo),
      ~ round(.x, 4)
    )
  )


# IMAGEN


tabla_desc_gbm_gt <- tabla_desc_gbm |>
  gt() |>
  tab_header(
    title = "Estadísticas Descriptivas - Modelo GBM con Validación Espacial"
  ) |>
  cols_label(
    Variable = "Variable",
    Media = "Media",
    Desv_Est = "Desv. Est.",
    Minimo = "Mínimo",
    Maximo = "Máximo"
  ) |>
  fmt_number(
    columns = c(Media, Desv_Est, Minimo, Maximo),
    decimals = 4
  )

# 
# EXPORTAR IMAGEN PNG
#

gtsave(
  data = tabla_desc_gbm_gt,
  filename = "Tabla_Estadisticas_Descriptivas_GBM_SpatialCV.png",
  path = "03_output/tables"
)

#######
plot(
  pred_train,
  train_model_gbm1$log_price,
  pch = 16,
  cex = 0.5,
  xlab = "Predicción",
  ylab = "Valor real",
  main = "Predicción vs Valor real"
)

abline(0,1,col="red",lwd=2)
