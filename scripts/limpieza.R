# ----------------------#
#   LIMPIEZA DE LA BASE #
# ----------------------#

#librerías
library(tidyverse)
library(dplyr)
library(here)

# traemos la base
encuesta <- "./data/encuesta_tesis.csv"
datos_encuesta = read.csv(encuesta) %>% 
  slice(-c(1, 2)) # Omite las filas 1 y 2
head(datos_encuesta)

# Filtramos: Vamos a filtrar los NAs y la edad (va a quedar entre 18 y 29) 
datos_filtrados <- datos_encuesta %>% 
  # Convertimos Q1 a número y creamos la variable edad
  mutate(edad = as.numeric(Q1)) %>% 
  filter(
    edad >= 18 & edad <= 29,
    Finished == "Verdadero" # Evalúa ambos formatos de Qualtrics
  )

# Verificar cuántas observaciones válidas quedaron
nrow(datos_filtrados)

# ---------------------------------------
# TRATAMIENTO Y VARIABLE DEPENDIENTE (VD)
# ---------------------------------------
datos_limpios <- datos_filtrados %>%
mutate(
  tratamiento = case_when(
    !is.na(Q8)  & Q8  != "" ~ "Pronatalista_Libertarios",
    !is.na(Q10) & Q10 != "" ~ "Pronatalista_Control",
    !is.na(Q12) & Q12 != "" ~ "Prochoice_Peronistas",
    !is.na(Q14) & Q14 != "" ~ "Prochoice_Control",
    TRUE ~ NA_character_
  ),
  
  # Consolidar Importancia de tener hijos según el tratamiento
  importancia_hijos_raw = case_when(
    tratamiento == "Pronatalista_Libertarios" ~ Q9,
    tratamiento == "Pronatalista_Control"     ~ Q11,
    tratamiento == "Prochoice_Peronistas"      ~ Q13,
    tratamiento == "Prochoice_Control"         ~ Q15,
    TRUE ~ NA_character_
  ),
  
  # Consolidar Intención a los 30 años según el tratamiento
  intencion_hijos_raw = case_when(
    tratamiento == "Pronatalista_Libertarios" ~ Q16,
    tratamiento == "Pronatalista_Control"     ~ Q18,
    tratamiento == "Prochoice_Peronistas"      ~ Q19,
    tratamiento == "Prochoice_Control"         ~ Q20.1,
    TRUE ~ NA_character_
  )
)

# -------------------------------------
# CODIFICAR VARIABLES DEPENDIENTES (VD)
# -------------------------------------
datos_limpios <- datos_limpios %>%
  mutate(
    # Intención de tener hijos (Binaria: 1 = Sí, 0 = No)
    intencion_hijos = case_when(
      str_detect(intencion_hijos_raw, "(?i)s[íi]") ~ 1,
      str_detect(intencion_hijos_raw, "(?i)no")   ~ 0,
      TRUE ~ NA_real_ # Ignora "Prefiero no responder" o vacíos
    ),
    
    # Importancia de tener hijos (Escala Likert de 1 a 4)
    importancia_hijos = case_when(
      importancia_hijos_raw == "Nada importante" ~ 1,
      importancia_hijos_raw == "Poco importante" ~ 2,
      importancia_hijos_raw == "Algo importante" ~ 3,
      importancia_hijos_raw == "Muy importante" ~ 4,
      TRUE ~ NA_real_
    )
  ) %>% 

# ---------------------------------
# CODIFICAR VARIABLE INDEPENDIENTE 
# ---------------------------------  
  mutate(
    party_id = case_when(
      # Libertarios: identifica LLA en Q5 O votó a Milei en Q6
      Q5 == "La Libertad Avanza" | Q6 == "Javier Milei" ~ "Libertarios",
      # Peronistas: identifica Unión por la Patria en Q5 O votó a Massa en Q6
      str_detect(Q5, "Unión por la Patria") | Q6 == "Sergio Massa" ~ "Peronistas",
      # No identificados (Específicamente quienes respondieron no identificarse)
      Q5 == "No me identifico con ninguno" ~ "No Identificado",
      # Todo lo demás (PRO, UCR, Izquierda, Voto en blanco, NAs, etc.)
      TRUE ~ "Resto"
    ),
  
    # Convertimos a Factor definiendo "No Identificado" como grupo de comparación base
    party_id = factor(party_id, levels = c("No Identificado", "Libertarios", "Peronistas", "Resto"))
  )

datos_limpios %>% count(party_id) 
  
# --------------------------------------  
# CODIFICAR DEMOGRÁFICAS Y RELIGIOSIDAD 
# --------------------------------------
datos_analisis <- datos_limpios %>%
  mutate(
    edad = as.numeric(Q1),
    
    genero = case_when(
      Q2 == "Mujer/Femenino" ~ "Mujer",
      Q2 == "Hombre/Masculino" ~ "Hombre",
      TRUE ~ "Otro"
    )
  )

# --------------------------------------
# H0: ÍNDICE DE VALORES SOCIOCULTURALES
# --------------------------------------
datos_analisis <- datos_analisis %>%
mutate(
  # Oportunidades iguales (Acuerdo = postura tradicional/conservadora)
  p4_1_num = case_when(
    Q24_1 == "Totalmente de acuerdo" ~ 1,
    Q24_1 == "De acuerdo" ~ 2,
    Q24_1 == "En desacuerdo" ~ 3,
    Q24_1 == "Totalmente en desacuerdo" ~ 4
  ),
  # Preocupación exagerada (Acuerdo = tradicional)
  p4_2_num = case_when(
    Q24_2 == "Totalmente de acuerdo" ~ 1,
    Q24_2 == "De acuerdo" ~ 2,
    Q24_2 == "En desacuerdo" ~ 3,
    Q24_2 == "Totalmente en desacuerdo" ~ 4
  ),
  # No se necesitan campañas (Acuerdo = tradicional)
  p4_3_num = case_when(
    Q24_3 == "Totalmente de acuerdo" ~ 1,
    Q24_3 == "De acuerdo" ~ 2,
    Q24_3 == "En desacuerdo" ~ 3,
    Q24_3 == "Totalmente en desacuerdo" ~ 4
  ),
  # Aborto justificado (Acuerdo = progresista)
  p4_4_num = case_when(
    Q24_4 == "Totalmente de acuerdo" ~ 4,
    Q24_4 == "De acuerdo" ~ 3,
    Q24_4 == "En desacuerdo" ~ 2,
    Q24_4 == "Totalmente en desacuerdo" ~ 1
  ),
  # Derechos homosexuales (Acuerdo = progresista)
  p4_5_num = case_when(
    Q24_5 == "Totalmente de acuerdo" ~ 4,
    Q24_5 == "De acuerdo" ~ 3,
    Q24_5 == "En desacuerdo" ~ 2,
    Q24_5 == "Totalmente en desacuerdo" ~ 1
  ),
  
  # Religiosidad invertida (4 = Más secular/progresista, 1 = Más tradicional)
  # (Ajusta 'Q5' o el nombre exacto de tu columna de religiosidad)
  p_religiosidad_num = case_when(
    str_detect(Q5, "(?i)regularmente") ~ 1,
    str_detect(Q5, "(?i)ocasionalmente") ~ 2,
    str_detect(Q5, "(?i)raramente") ~ 3,
    str_detect(Q5, "(?i)nunca") ~ 4,
    TRUE ~ NA_real_
  )
) %>% 
  
  # 4. PROMEDIAR LOS 6 ÍTEMS PARA EL ÍNDICE FINAL
  rowwise() %>% 
  mutate(
    indice_progresismo = mean(c(p4_1_num, p4_2_num, p4_3_num, p4_4_num, p4_5_num, p_religiosidad_num), na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  
  # 5. REORDENAR Y MOVER LAS VARIABLES PRINCIPALES AL PRINCIPIO
  relocate(intencion_hijos, importancia_hijos, indice_progresismo, .after = tratamiento)

# --------
# GUARDAR
# --------
# Definimos la carpeta output con here()
output_dir <- here("output")

# Creamos la carpeta si no existe
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
} else {
  cat("'output' ya existe. Se sobrescribirá el archivo.\n")
}

# Guardamos la base procesada directamente dentro de la carpeta output
saveRDS(datos_analisis, file.path(output_dir, "datos_analisis_tesis.rds"))


