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
install.packages("sjPlot")
library(sjPlot)

# Configurar tema visual estandarizado para los gráficos de la tesis
theme_set(theme_minimal(base_size = 12))

# Definir ruta e importar datos
output_dir <- here("output")
datos <- readRDS(file.path(output_dir, "datos_analisis_tesis.rds"))

# --------------------
# ANALISIS DESCRIPTIVO
# --------------------

### Tratamiento por party id

# Tabla cruzada de recuentos
table_tratamiento_party <- table(datos$tratamiento, datos$party_id, useNA = "ifany")
print(table_tratamiento_party)

# Gráfico de barras: Distribución de la muestra por Tratamiento y Partido
g_distribucion <- ggplot(datos, aes(x = tratamiento, fill = party_id)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribución de participantes por grupo de tratamiento y filiación política",
    x = "Tratamiento asignado",
    y = "Cantidad de encuestados",
    fill = "Identidad Partidaria"
  ) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Guardar gráfico en carpeta output
ggsave(file.path(output_dir, "g1_distribucion_tratamiento.png"), g_distribucion, width = 8, height = 5)


### Intención e importancia de tener hijos según tratamiento e identidad

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

ggsave(file.path(output_dir, "g2_intencion_por_tratamiento.png"), g_intencion, width = 9, height = 5)

# ---------------------
# MODELOS DE REGRESIÓN
# ---------------------

### Intención de tener hijos

# Modelo 1: Efecto principal del tratamiento
m1_intencion <- glm(intencion_hijos ~ tratamiento, 
                    data = datos, family = binomial)

# Modelo 2: Agregando Identidad Partidaria y Controles (Demográficos + Índice Sociocultural)
m2_intencion <- glm(intencion_hijos ~ tratamiento + party_id + edad + genero + 
                      indice_progresismo, data = datos, family = binomial)

# Modelo 3: Modelo de Interacción (Tratamiento * Identidad Partidaria)
m3_intencion <- glm(intencion_hijos ~ tratamiento * party_id + edad + genero + 
                      indice_progresismo, data = datos, family = binomial)

# Ver resumen estadístico del modelo con interacción
summary(m3_intencion)


### Importancia ed tener hijos

# Modelo 1: Efecto principal
m1_importancia <- lm(importancia_hijos ~ tratamiento, data = datos)

# Modelo 2: Con controles
m2_importancia <- lm(importancia_hijos ~ tratamiento + party_id + edad + 
                       genero + indice_progresismo, data = datos)

# Modelo 3: Interacción entre Tratamiento y Partidismo
m3_importancia <- lm(importancia_hijos ~ tratamiento * party_id + edad + 
                       genero + indice_progresismo, data = datos)

summary(m3_importancia)

# --------------------
# TABLAS DE REGRESIÓN
# --------------------

### INTENCIóN

# Tabla comparativa de los modelos de Intención de Tener Hijos
models_intencion <- list(
  "M1: Solo Tratamiento" = m1_intencion,
  "M2: Con Controles"    = m2_intencion,
  "M3: Interacción"      = m3_intencion
)

modelsummary(
  models_intencion,
  output = file.path(output_dir, "tabla_regresion_intencion.html"),  
  exponentiate = TRUE, # Muestra Odds Ratios en vez de coeficientes logit
  stars = TRUE,
  title = "Tabla X: Regresión Logística para Intención de Tener Hijos (Odds Ratios)"
)

### IMPORTANCIA

# Tabla comparativa de los modelos de Importancia de Tener Hijos
models_importancia <- list(
  "M1: Solo Tratamiento" = m1_importancia,
  "M2: Con Controles"    = m2_importancia,
  "M3: Interacción"      = m3_importancia
)

modelsummary(
  models_importancia,
  output = file.path(output_dir, "tabla_regresion_importancia.html"),
  exponentiate = FALSE, # FALSO para OLS / lm (muestra coeficientes directos, no Odds Ratios)
  stars = TRUE,
  title = "Tabla Y: Regresión Lineal (OLS) para Importancia de Tener Hijos"
)

# ----------------------------------
# EFECTOS MARGINALES E INTERACCIONES
# ----------------------------------

### INTENCIóN

# Visualizar probabilidades predichas de la interacción (Modelo 3 Logístico)
g_interaccion_intencion <- plot_model(
  m3_intencion, 
  type = "pred", 
  terms = c("tratamiento", "party_id"),
  title = "Efecto interactivo entre Tratamiento y Partidismo en la Intención de Tener Hijos",
  axis.title = c("Tratamiento", "Probabilidad Predicha de Tener Hijos")
) + theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(file.path(output_dir, "g3_efectos_interaccion_intencion.png"), g_interaccion_intencion, width = 9, height = 6)

# Visualizar el rol moderador del Índice de Progresismo (Efecto Marginal)
g_efecto_progresismo <- plot_model(
  m2_intencion,
  type = "pred",
  terms = "indice_progresismo",
  title = "Probabilidad Predicha de Intención de Tener Hijos según Índice de Progresismo",
  axis.title = c("Índice de Progresismo (1 = Tradicional, 4 = Progresista)", "Probabilidad Predicha")
)

ggsave(file.path(output_dir, "g4_efecto_progresismo.png"), g_efecto_progresismo, width = 7, height = 5)









