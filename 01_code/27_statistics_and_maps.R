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
