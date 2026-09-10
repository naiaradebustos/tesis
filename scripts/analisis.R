# --------------#
#   ANÁLISIS    #
# --------------#

# librerías
library(tidyverse)
library(dplyr)
library(here)
library(broom)       # Para convertir outputs de regresiones a dataframes limpios
library(modelsummary)# Para generar tablas de regresión tipo publicación
library(ggplot2)
# install.packages("sjPlot")
library(sjPlot)
# install.packages("marginaleffects")
library(marginaleffects)

# Configurar tema visual estandarizado para los gráficos de la tesis
theme_set(theme_minimal(base_size = 12))

# Definir ruta e importar datos
output_dir <- here("output")
datos <- readRDS(file.path(output_dir, "datos_analisis_tesis.rds"))

# --------------------
# ANALISIS DESCRIPTIVO
# --------------------

### Tratamiento por party id

# Tabla party id y tratamiento
table_tratamiento_party <- table(datos$tratamiento, datos$party_id, useNA = "ifany")
print(table_tratamiento_party)

# Gráfico de barras: Distribución de la muestra por Tratamiento y Partido
g_distribucion <- ggplot(datos, aes(x = tratamiento, fill = party_id)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribución de participantes por grupo de tratamiento e identificación política",
    x = "Tratamiento asignado",
    y = "Cantidad de encuestados",
    fill = "Identidad Partidaria"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Guardar gráfico en carpeta output
ggsave(file.path(output_dir, "g1_distribucion_tratamiento.png"), 
       g_distribucion, width = 8, height = 5)


### Intención e importancia de tener hijos según tratamiento e identidad

# INTENCIÓN
# Promedios por tratamiento e identidad partidaria
resumen_intencion <- datos %>%
  filter(!is.na(party_id), !is.na(tratamiento), !is.na(intencion_hijos)) %>%
  group_by(tratamiento, party_id) %>%
  summarise(
    # Multiplicamos por 100 para tener el porcentaje directo (ej: 45.2 en lugar de 0.452)
    prop_si = mean(intencion_hijos, na.rm = TRUE) * 100,
    n = n(),
    # Ajustamos el Error Estándar también a la escala 0-100
    se = sqrt(((prop_si / 100) * (1 - (prop_si / 100))) / n) * 100, 
    .groups = "drop"
  )

# Gráfico de puntos con barras de error
# Porcentaje que responde que si quiere tener hijos
g_intencion <- ggplot(resumen_intencion, aes(x = tratamiento, y = prop_si, color = party_id, group = party_id)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(
    aes(ymin = prop_si - 1.96 * se, ymax = prop_si + 1.96 * se), 
    position = position_dodge(width = 0.4), 
    width = 0.2
  ) +
  labs(
    title = "Intención de tener hijos según Tratamiento e Identidad Partidaria",
    x = "Tratamiento",
    y = "Porcentaje que responde 'Sí' (%)", # Cambiamos el eje Y
    color = "Partido"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(output_dir, "g2a_intencion_por_tratamiento.png"), 
       g_intencion, width = 9, height = 5)

# Los libertarios dicen más que sí cuando el mensaje es de su partido
# Los peronistas dicen más que no cuando el mensaje es de su partido
# ambas respuestas suelen tener esta tendencia en las demás opciones, pero 
# en ellas se potencia. 


# IMPORTANCIA
# Promedios por tratamiento e identidad partidaria
resumen_importancia <- datos %>%
  filter(!is.na(party_id), !is.na(tratamiento), !is.na(importancia_hijos)) %>%
  group_by(tratamiento, party_id) %>%
  summarise(
    media_importancia = mean(importancia_hijos, na.rm = TRUE),
    n = n(),
    sd = sd(importancia_hijos, na.rm = TRUE),
    # Error estándar de la media (no de proporción, porque acá la variable no es binaria)
    se = sd / sqrt(n),
    .groups = "drop"
  )

# Gráfico de puntos con barras de error
# Promedio de importancia asignada a tener hijos (escala 1-4)
g_importancia <- ggplot(resumen_importancia, aes(x = tratamiento, y = media_importancia, color = party_id, group = party_id)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(
    aes(ymin = media_importancia - 1.96 * se, ymax = media_importancia + 1.96 * se), 
    position = position_dodge(width = 0.4), 
    width = 0.2
  ) +
  labs(
    title = "Importancia de tener hijos según Tratamiento e Identidad Partidaria",
    x = "Tratamiento",
    y = "Importancia promedio (escala 1-4)", # Cambiamos el eje Y
    color = "Partido"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(output_dir, "g2b_importancia_por_tratamiento.png"), 
       g_importancia, width = 9, height = 5)

# ---------------------
# MODELOS DE REGRESIÓN
# ---------------------

### INTENCIÓN

# Modelo 1: Modelo sin tratamiento y con controles
m1_intencion <- glm(intencion_hijos ~ party_id  + edad_c + genero , 
                    data = datos, family = binomial)

# Modelo 2: Interacción con tratamiento y partimos los modelos por mensaje
m2A_intencion_choice <- glm(intencion_hijos ~ party_id + edad_c + genero + tratamiento +
                              party_id * tratamiento, 
                            data = datos %>% 
                              filter(tratamiento %in% c("Prochoice_Peronistas", "Prochoice_Control")) %>%
                              mutate(tratamiento = droplevels(tratamiento),
                                     party_id = droplevels(party_id)),
                            family = binomial)

m2B_intencion_nat <- glm(intencion_hijos ~ party_id + edad_c + genero + tratamiento +
                           party_id * tratamiento,
                         data = datos %>% 
                           filter(tratamiento %in% c("Pronatalista_Libertarios", "Pronatalista_Control")) %>%
                           mutate(tratamiento = droplevels(tratamiento),
                                  party_id = droplevels(party_id)),
                         family = binomial)

### IMPORTANCIA

# Modelo 1: Modelo sin tratamiento y con controles
m1_importancia <- lm(importancia_hijos ~ party_id + edad_c + genero, data = datos)

# Modelo 2: Interacción con tratamiento y partimos los modelos por mensaje
m2A_importancia_choice <- lm(importancia_hijos ~ party_id + tratamiento + edad_c + genero + 
                               party_id * tratamiento, 
                             data = datos%>% 
                               filter(tratamiento %in% c("Prochoice_Peronistas", "Prochoice_Control")) %>%
                               mutate(tratamiento = droplevels(tratamiento),
                                      party_id = droplevels(party_id)))

m2B_importancia_nat <- lm(importancia_hijos ~ party_id + tratamiento + edad_c + genero + 
                            party_id * tratamiento,
                      data = datos %>% 
                        filter(tratamiento %in% c("Pronatalista_Libertarios", "Pronatalista_Control"))%>%
                        mutate(tratamiento = droplevels(tratamiento),
                               party_id = droplevels(party_id)))

# --------------------
# TABLAS DE REGRESIÓN
# --------------------

### INTENCIóN

# Tabla 1: Intención de tener hijos
models_intencion <- list(
  "M1: Sin tratamiento"     = m1_intencion,
  "M2: Prochoice"           = m2A_intencion_choice,
  "M2: Pronatalista"        = m2B_intencion_nat
)

modelsummary(
  models_intencion,
  exponentiate = TRUE,
  stars = TRUE,
  title = "Regresión logística: Intención de tener hijos",
#  output = file.path(output_dir, "tabla_intencion.png")
)

### IMPORTANCIA

# Tabla 2: Actitud/importancia de tener hijos (todos modelos lineales → comparables entre sí)
models_importancia <- list(
  "M1: Sin tratamiento"     = m1_importancia,
  "M2: Prochoice"           = m2A_importancia_choice,
  "M2: Pronatalista"        = m2B_importancia_nat
)

modelsummary(
  models_importancia,
  exponentiate = FALSE,
  stars = TRUE,
  title = "Tabla Y: Regresión lineal (OLS) — Actitud hacia tener hijos",
#  output = file.path(output_dir, "tabla_importancia.png")
)

# ------------------
# EFECTOS MARGINALES 
# ------------------

# Efecto marginal del tratamiento, separado por party_id

### INTENCIóN

# choice
em_intencion_choice <- avg_comparisons(
  m2A_intencion_choice,
  variables = "tratamiento",
  by = "party_id"
)
print(em_intencion_choice)

# natalidad
em_intencion_nat <- avg_comparisons(
  m2B_intencion_nat,
  variables = "tratamiento",
  by = "party_id"
)
print(em_intencion_nat)

### IMPORTANCIA

# choice
em_importancia_choice <- avg_comparisons(
  m2A_importancia_choice,
  variables = "tratamiento",
  by = "party_id"
)
print(em_importancia_choice)

# natalidad
em_importancia_nat <- avg_comparisons(
  m2B_importancia_nat,
  variables = "tratamiento",
  by = "party_id"
)
print(em_importancia_nat)

# --------
# GRÁFICOS
# --------

# --- Intención, mensaje Prochoice ---
g_efecto_intencion_choice <- plot_comparisons(
  m2A_intencion_choice, 
  variables = "tratamiento", 
  by = "party_id"
) +
  labs(
    title = "Efecto marginal del tratamiento sobre la intención de tener hijos",
    subtitle = "Comparación: Prochoice_Peronistas vs. Prochoice_Control",
    x = "Identidad Partidaria",
    y = "Diferencia en probabilidad predicha"
  )
ggsave(file.path(output_dir, "g3a_efecto_intencion_choice.png"), 
       g_efecto_intencion_choice, width = 7, height = 5)

# --- Intención, mensaje Pronatalista ---
g_efecto_intencion_nat <- plot_comparisons(
  m2B_intencion_nat, 
  variables = "tratamiento", 
  by = "party_id"
) +
  labs(
    title = "Efecto marginal del tratamiento sobre la intención de tener hijos",
    subtitle = "Comparación: Pronatalista_Libertarios vs. Pronatalista_Control",
    x = "Identidad Partidaria",
    y = "Diferencia en probabilidad predicha"
  )
ggsave(file.path(output_dir, "g3b_efecto_intencion_nat.png"), 
       g_efecto_intencion_nat, width = 7, height = 5)


# --- Importancia, mensaje Prochoice ---
g_efecto_importancia_choice <- plot_comparisons(
  m2A_importancia_choice, 
  variables = "tratamiento", 
  by = "party_id"
) +
  labs(
    title = "Efecto marginal del tratamiento sobre la importancia de tener hijos",
    subtitle = "Comparación: Prochoice_Peronistas vs. Prochoice_Control",
    x = "Identidad Partidaria",
    y = "Diferencia en escala de importancia (1-4)"
  )
ggsave(file.path(output_dir, "g3c_efecto_importancia_choice.png"), 
       g_efecto_importancia_choice, width = 7, height = 5)

# --- Importancia, mensaje Pronatalista ---
g_efecto_importancia_nat <- plot_comparisons(
  m2B_importancia_nat, 
  variables = "tratamiento", 
  by = "party_id"
) +
  labs(
    title = "Efecto marginal del tratamiento sobre la importancia de tener hijos",
    subtitle = "Comparación: Pronatalista_Libertarios vs. Pronatalista_Control",
    x = "Identidad Partidaria",
    y = "Diferencia en escala de importancia (1-4)"
  )
ggsave(file.path(output_dir, "g3d_efecto_importancia_nat.png"), 
       g_efecto_importancia_nat, width = 7, height = 5)





