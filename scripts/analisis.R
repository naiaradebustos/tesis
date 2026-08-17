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

# Configurar tema visual estandarizado para los gráficos de la tesis
theme_set(theme_minimal(base_size = 12))

# Definir ruta e importar datos
output_dir <- here("output")
datos <- readRDS(file.path(output_dir, "datos_analisis_tesis.rds"))

# --------------------
# ANALISIS DESCRIPTIVO
# --------------------
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


