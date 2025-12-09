library(tidyverse)
library(arrow)

url <- "https://datos.gob.cl/dataset/606ef5bb-11d1-475b-b69f-b980da5757f4/resource/ae6c9887-106d-4e98-8875-40bf2b836041/download/at_urg_respiratorio_semanal.parquet"

download.file(url, "raw-data/at_urg_respiratorio_semanal.parquet", mode = "wb")

data <- arrow::read_parquet(
  "raw-data/at_urg_respiratorio_semanal.parquet"
)


# Parte 1 ----

flu_hospitals <- data |>
  filter(
    TipoUrgencia == "Urgencia Hospitalaria (UEH)",
    Causa == "Influenza (J09-J11)"
  ) |>
  group_by(Anio, SemanaEstadistica) |>
  summarise(casos = sum(NumTotal)) |>
  ungroup() |>
  janitor::clean_names()

flu_hospitals <- flu_hospitals |>
  filter(!(anio == 2025 & semana_estadistica == last(semana_estadistica)))

flu_hosp2 <- flu_hospitals |>
  mutate(anio = factor(anio))

ggplot(
  flu_hospitals,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line()

ggplot(
  data = flu_hospitals,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals$semana_estadistica),
      max(flu_hospitals$semana_estadistica),
      by = 5
    )
  ) +
  scale_color_manual(
    values = c(
      rep("#ced4da", times = length(unique(flu_hospitals$anio)) - 1),
      "#ff006e"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Año 2025 resaltado",
    x = "Semana epidemiológica",
    y = "Casos",
    color = "Año",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold")
  )


# Parte 2 ----

flu_hospitals_per_region <- data |>
  filter(
    TipoUrgencia == "Urgencia Hospitalaria (UEH)",
    Causa == "Influenza (J09-J11)"
  ) |>
  group_by(RegionGlosa, Anio, SemanaEstadistica) |>
  summarise(casos = sum(NumTotal)) |>
  ungroup() |>
  janitor::clean_names()

flu_hospitals_per_region <- flu_hospitals_per_region |>
  filter(!(anio == 2025 & semana_estadistica == last(semana_estadistica)))


ggplot(
  data = flu_hospitals_per_region,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 0.6) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals_per_region$semana_estadistica),
      max(flu_hospitals_per_region$semana_estadistica),
      by = 5
    )
  ) +
  scale_color_manual(
    values = c(
      rep("#ced4da", times = length(unique(flu_hospitals_per_region$anio)) - 1),
      "#ff006e"
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza, por regiones de Chile",
    subtitle = "Año 2025 resaltado",
    x = "Semana epidemiológica",
    y = "Casos",
    color = "Año",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold")
  ) +
  facet_wrap(~region_glosa, scales = "free_y")


# Parte 3 ----

library(ggtext)

unique_years <- unique(flu_hospitals$anio)

# Definir los colores para los años 2024 y 2025
colors <- rep("#ced4da", times = length(unique_years))
colors[unique_years == 2024] <- "#8338ec"
colors[unique_years == 2025] <- "#ff006e"

# Obtener el último punto de datos para el año 2025
last_2025 <- flu_hospitals |>
  filter(anio == 2025) |>
  slice(which.max(semana_estadistica))

# Obtener el valor máximo para el 2023
max_2024 <- flu_hospitals |>
  filter(anio == 2024) |>
  slice(which.max(casos))


ggplot(
  data = flu_hospitals,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 0.8) +
  geom_text(
    data = last_2025,
    aes(label = casos),
    vjust = 0,
    hjust = -0.2,
    size = 3.5
  ) +
  geom_text(
    data = max_2024,
    aes(label = casos),
    vjust = 0,
    hjust = -0.2,
    size = 3.5
  ) +
  geom_point(
    data = last_2025,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#ff006e"
  ) +
  geom_point(
    data = max_2024,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#8338ec"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(flu_hospitals$semana_estadistica),
      max(flu_hospitals$semana_estadistica),
      by = 5
    )
  ) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Atenciones en urgencias hospitalarias por Influenza",
    subtitle = "Se han resaltado los años <span style='color:#8338ec;'>2024</span> y <span style='color:#ff006e;'>2025</span>",
    x = "Semana epidemiológica",
    y = "Casos",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 16, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10, face = "bold")
  )


# Hospitalizaciones de urgencia RN y lactantes

vrs_hospitals <- data |>
  filter(
    Causa == "HOSPITALIZACIONES POR CAUSAS SISTEMA RESPIRATORIO"
  ) |>
  group_by(Anio, SemanaEstadistica) |>
  summarise(casos = sum(NumMenor1Anio)) |>
  ungroup() |>
  janitor::clean_names()

vrs_hospitals <- vrs_hospitals |>
  filter(!(anio == 2025 & semana_estadistica == last(semana_estadistica)))


library(ggtext)

unique_years <- unique(vrs_hospitals$anio)

# Definir los colores para los años 2024 y 2025
colors <- rep("#ced4da", times = length(unique_years))
colors[unique_years == 2024] <- "#8338ec"
colors[unique_years == 2025] <- "#ff006e"

# Obtener el último punto de datos para el año 2025
last_2025 <- vrs_hospitals |>
  filter(anio == 2025) |>
  slice(which.max(semana_estadistica))

# Obtener el valor máximo para el 2023
max_2024 <- vrs_hospitals |>
  filter(anio == 2024) |>
  slice(which.max(casos))


ggplot(
  data = vrs_hospitals,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 1) +
  geom_text(
    data = last_2025,
    aes(label = casos),
    vjust = -3,
    hjust = -0.2,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_text(
    data = max_2024,
    aes(label = casos),
    vjust = -3,
    hjust = -0.2,
    size = 3.5,
    fontface = "bold"
  ) +
  geom_point(
    data = last_2025,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#ff006e"
  ) +
  geom_point(
    data = max_2024,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#8338ec"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(vrs_hospitals$semana_estadistica),
      max(vrs_hospitals$semana_estadistica),
      by = 5
    )
  ) +
  scale_color_manual(values = colors) +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(
    xintercept = 14,
    linetype = "dashed",
    color = "black",
    alpha = 0.5
  ) +
  geom_text(
    aes(
      x = 6,
      y = 1100,
      label = "Inicio de inmunización\ncon Nirsevimab (2024)"
    ),
    vjust = 1,
    hjust = 0,
    size = 4
  ) +
  labs(
    title = "Hospitalizaciones por causas respiratorias en menores de un año de edad en Chile (2014 - 2025)",
    subtitle = "Se han resaltado los años <span style='color:#8338ec;'>2024</span> y <span style='color:#ff006e;'>2025</span>",
    x = "Semana epidemiológica",
    y = "N° Hospitalizaciones",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 16, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8, face = "bold")
  )


# Versión simplificada con gghighlight ----

library(gghighlight)

ggplot(
  data = vrs_hospitals,
  aes(x = semana_estadistica, y = casos, color = factor(anio))
) +
  geom_line(linewidth = 1) +
  gghighlight(
    anio %in% c(2024, 2025),
    use_direct_label = FALSE,
    unhighlighted_params = list(color = "#ced4da", linewidth = 0.8)
  ) +
  scale_color_manual(values = c("2024" = "#8338ec", "2025" = "#ff006e")) +
  geom_text(
    data = last_2025,
    aes(label = casos),
    vjust = -3,
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    color = "#ff006e"
  ) +
  geom_text(
    data = max_2024,
    aes(label = casos),
    vjust = -3,
    hjust = -0.2,
    size = 3.5,
    fontface = "bold",
    color = "#8338ec"
  ) +
  geom_point(
    data = last_2025,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#ff006e"
  ) +
  geom_point(
    data = max_2024,
    aes(x = semana_estadistica, y = casos),
    size = 3,
    color = "#8338ec"
  ) +
  scale_x_continuous(
    breaks = seq(
      min(vrs_hospitals$semana_estadistica),
      max(vrs_hospitals$semana_estadistica),
      by = 5
    )
  ) +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(
    xintercept = 14,
    linetype = "dashed",
    color = "black",
    alpha = 0.5
  ) +
  geom_text(
    aes(
      x = 6,
      y = 1100,
      label = "Inicio de inmunización\ncon Nirsevimab (2024)"
    ),
    vjust = 1,
    hjust = 0,
    size = 4,
    color = "black"
  ) +
  labs(
    title = "Hospitalizaciones por causas respiratorias en menores de un año de edad en Chile (2014 - 2025)",
    subtitle = "Se han resaltado los años <span style='color:#8338ec;'>2024</span> y <span style='color:#ff006e;'>2025</span>",
    x = "Semana epidemiológica",
    y = "N° Hospitalizaciones",
    caption = "datos.gob.cl | MINSAL"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = ggtext::element_markdown(size = 16, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text = element_text(size = 8, face = "bold")
  )
