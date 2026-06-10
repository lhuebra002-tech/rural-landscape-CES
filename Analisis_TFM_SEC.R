# ==============================================================================
# BLOQUE 0: CARGA DE LIBRERÍAS Y PAQUETES 
# ==============================================================================
# install.packages(c("dplyr", "tidyverse", "psych", "corrplot", "ggrepel"))

library(dplyr)
library(tidyverse)
library(psych)
library(corrplot)
library(ggplot2)
library(ggrepel)

# ==============================================================================
# BLOQUE 1: SUBIR DATOS
# ==============================================================================
depuracion_de_datos <- as.data.frame(depuracion_de_datos)

vars_factor <- c("unidad", "edad", "genero", "nivel_estudios", "ocupacion", 
                 "relacion_territorio", "residencia", "tiempo_conocimiento", 
                 "frecuencia_visita", "vinculo_emocional")

depuracion_de_datos[vars_factor] <- lapply(depuracion_de_datos[vars_factor], as.factor)

cat("\n--- Resumen de la muestra (N=95) ---\n")
summary(depuracion_de_datos[vars_factor])


# ==============================================================================
# BLOQUE 2: CREACIÓN DE ÍNDICES SEC
# ==============================================================================
sec_items <- list(
  Estetica = c("aes_1", "aes_2"),
  Sentido_Lugar = c("sop_1", "sop_2"),
  Patrimonio = c("her_1", "her_2"),
  Educacion = c("edu_1", "edu_2"),
  Recreacion = c("rec_1", "rec_2"),
  Bienestar = c("wel_1", "wel_2"),
  Espiritual = c("spi_1", "spi_2", "spi_3")
)


depuracion_de_datos <- depuracion_de_datos %>%
  rowwise() %>%
  mutate(
    idx_aes = mean(c_across(all_of(sec_items$Estetica)), na.rm = TRUE),
    idx_sop = mean(c_across(all_of(sec_items$Sentido_Lugar)), na.rm = TRUE),
    idx_her = mean(c_across(all_of(sec_items$Patrimonio)), na.rm = TRUE),
    idx_edu = mean(c_across(all_of(sec_items$Educacion)), na.rm = TRUE),
    idx_rec = mean(c_across(all_of(sec_items$Recreacion)), na.rm = TRUE),
    idx_wel = mean(c_across(all_of(sec_items$Bienestar)), na.rm = TRUE),
    idx_spi = mean(c_across(all_of(sec_items$Espiritual)), na.rm = TRUE)
  ) %>%
  ungroup()

print("Bloque 2 ejecutado con éxito! Índices SEC calculados y guardados.")


# ==============================================================================
# BLOQUE 3: NORMALIDAD Y CORRELACIONES ENTRE SERVICIOS ECOSISTÉMICOS
# ==============================================================================
indices_sec <- c("idx_aes", "idx_sop", "idx_her", "idx_edu", "idx_rec", "idx_wel", "idx_spi")

cat("\n--- Resultados de Normalidad (Shapiro-Wilk) ---\n")
shapiro_resultados <- depuracion_de_datos %>%
  select(all_of(indices_sec)) %>%
  map_df(~ shapiro.test(.x)$p.value) %>%
  pivot_longer(cols = everything(), names_to = "Dimension_SEC", values_to = "p_valor_Shapiro")

print(shapiro_resultados)

matriz_cor_sec <- cor(depuracion_de_datos[, indices_sec], method = "spearman", use = "complete.obs")

corrplot(matriz_cor_sec, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         diag = FALSE, title = "Correlaciones de Spearman entre dimensiones SEC", mar = c(0,0,1,0))


# ==============================================================================
# BLOQUE 5: RELACIÓN ENTRE ATRIBUTOS PAISAJÍSTICOS Y VALORIZACIÓN DE SEC
# ==============================================================================
cols_attr <- c("aes_formas_terreno", "aes_vistas", "aes_agua", "aes_homogeneidad", 
               "aes_vegetacion", "aes_conservacion", "aes_accesibilidad", "aes_ele_patrimoniales")
cols_sec <- c("idx_aes", "idx_sop", "idx_her", "idx_edu", "idx_rec", "idx_wel", "idx_spi")

nombres_attr <- c("Formas terreno", "Vistas", "Agua", "Homogeneidad", 
                  "Vegetación", "Conservación", "Accesibilidad", "Patrimonio")
nombres_sec <- c("Estética", "Sentido Lugar", "Patrimonio Cult.", 
                 "Educación", "Recreación", "Bienestar", "Espiritual")

paleta_mia <- colorRampPalette(c("#ffffd9", "#41b6c4", "#091d56"))(200)

generar_grafico_estilo <- function(matriz, titulo) {
  corrplot(matriz, method = "color", col = paleta_mia, 
           tl.col = "black", tl.cex = 0.8,
           addCoef.col = "black", number.cex = 0.6,
           title = titulo, mar = c(0,0,2,0), cl.lim = c(-1, 1))
}

# --- A. ANÁLISIS GLOBAL ---
matriz_global <- cor(depuracion_de_datos[, cols_attr], depuracion_de_datos[, cols_sec], 
                     method = "spearman", use = "pairwise.complete.obs")
rownames(matriz_global) <- nombres_attr
colnames(matriz_global) <- nombres_sec
generar_grafico_estilo(matriz_global, "Correlación Global: Atributos vs Servicios")

# --- B. ZOOM POR SERVICIO ---
for (j in seq_along(cols_sec)) {
  servicio_actual <- cols_sec[j]
  nombre_servicio <- nombres_sec[j]
  lista_cor_unidades <- list()
  
  for (u in unique(depuracion_de_datos$unidad)) {
    datos_u <- depuracion_de_datos %>% filter(unidad == u)
    if(nrow(datos_u) > 3) {
      cor_vec <- sapply(cols_attr, function(a) {
        cor(as.numeric(datos_u[[a]]), as.numeric(datos_u[[servicio_actual]]), 
            method = "spearman", use = "pairwise.complete.obs")
      })
      lista_cor_unidades[[as.character(u)]] <- cor_vec
    }
  }
  matriz_zoom <- do.call(rbind, lista_cor_unidades)
  colnames(matriz_zoom) <- nombres_attr
  generar_grafico_estilo(t(matriz_zoom), paste("Zoom:", nombre_servicio, "vs Atributos por Unidad"))
}

# Imprimir Matriz Global en Consola
cat("\n==================================================================\n")
cat("DATOS - MATRIZ DE CORRELACIÓN GLOBAL (N=95)\n")
cat("==================================================================\n")
print(round(matriz_global, 3))

# Imprimir Zooms en Consola
cat("\n==================================================================\n")
cat("DATOS - LOS 7 ZOOMS POR SERVICIO\n")
cat("==================================================================\n")
for (j in seq_along(cols_sec)) {
  servicio_actual <- cols_sec[j]
  nombre_servicio <- nombres_sec[j]
  lista_cor_unidades <- list()
  for (u in unique(depuracion_de_datos$unidad)) {
    datos_u <- depuracion_de_datos %>% filter(unidad == u)
    if(nrow(datos_u) > 3) {
      cor_vec <- sapply(cols_attr, function(a) {
        cor(as.numeric(datos_u[[a]]), as.numeric(datos_u[[servicio_actual]]), 
            method = "spearman", use = "pairwise.complete.obs")
      })
      lista_cor_unidades[[as.character(u)]] <- cor_vec
    }
  }
  matriz_zoom <- do.call(rbind, lista_cor_unidades)
  colnames(matriz_zoom) <- nombres_attr
  cat(paste("--- MATRIZ DE CORRELACIÓN PARA EL SERVICIO:", toupper(nombre_servicio), "---\n"))
  print(round(matriz_zoom, 3))
}



# ==============================================================================
# BLOQUE 6: EFECTO SOCIODEMOGRÁFICO Y VINCULACIÓN TERRITORIAL (CORREGIDO)
# ==============================================================================

# 1. Emparejar variables sociodemográficas
depuracion_de_datos$sexo <- depuracion_de_datos$genero
depuracion_de_datos$edad_grupo <- depuracion_de_datos$edad
depuracion_de_datos$vinculacion <- depuracion_de_datos$vinculo_emocional

# 2. ASIGNACIÓN CORRECTA: Usamos los índices reales del Bloque 2 sin recalcular ni redondear
depuracion_de_datos$Estética      <- depuracion_de_datos$idx_aes
depuracion_de_datos$Sentido_Lugar <- depuracion_de_datos$idx_sop
depuracion_de_datos$Patrimonio    <- depuracion_de_datos$idx_her
depuracion_de_datos$Educación     <- depuracion_de_datos$idx_edu
depuracion_de_datos$Recreación    <- depuracion_de_datos$idx_rec
depuracion_de_datos$Bienestar     <- depuracion_de_datos$idx_wel
depuracion_de_datos$Espiritual    <- depuracion_de_datos$idx_spi

cols_sec_b6 <- c("Estética", "Sentido_Lugar", "Patrimonio", "Educación", "Recreación", "Bienestar", "Espiritual")
nombres_sec_b6 <- c("Estética", "Sentido Lugar", "Patrimonio", "Educación", "Recreación", "Bienestar", "Espiritual")

variables_grupo <- c("edad_grupo", "sexo", "nivel_estudios", "ocupacion", 
                     "relacion_territorio", "residencia", "tiempo_conocimiento", 
                     "frecuencia_visita", "vinculacion")

for(variable_grupo in variables_grupo) {
  # Corregido: cambiada la errata %in= por %in%
  if(!variable_grupo %in% colnames(depuracion_de_datos)) next
  
  datos_temp <- depuracion_de_datos[!is.na(depuracion_de_datos[[variable_grupo]]), ]
  
  cat("\n==================================================================\n")
  cat(">>> VARIABLE PREDICTORA:", toupper(variable_grupo), "\n")
  cat("==================================================================\n")
  
  for(i in seq_along(cols_sec_b6)) {
    sec <- cols_sec_b6[i]
    nombre_sec <- nombres_sec_b6[i]
    cat("\n  -", nombre_sec, "vs", variable_grupo, ":\n")
    
    test <- kruskal.test(datos_temp[[sec]] ~ factor(datos_temp[[variable_grupo]]))
    p_valor <- test$p.value
    sig_label <- if(p_valor < 0.05) " *SIGNIFICATIVO*" else ""
    
    H_estadistico <- test$statistic
    N_total <- nrow(datos_temp)
    eff_size <- H_estadistico / (N_total - 1)
    
    cat("    Test aplicado: Kruskal-Wallis | p-valor =", round(p_valor, 4), sig_label, "\n")
    cat("    Tamaño del efecto (Epsilon^2) =", round(as.numeric(eff_size), 3), "\n")
    
    medianas <- aggregate(datos_temp[[sec]], by = list(Group = datos_temp[[variable_grupo]]), FUN = median, na.rm = TRUE)
    tendencia_str <- paste(apply(medianas, 1, function(r) paste(r[1], "=", round(as.numeric(r[2]), 2))), collapse = " | ")
    cat("    Tendencia (Medianas): [", tendencia_str, "]\n")
  }
}

# ==============================================================================
# BLOQUE 7: IMPACTO DE INFRAESTRUCTURAS ENERGÉTICAS
# ==============================================================================
variable_impacto <- "impacto_infra"

valores_originales <- trimws(as.character(depuracion_de_datos[[variable_impacto]]))
valores_numericos <- as.numeric(valores_originales)

valores_numericos[is.na(valores_numericos)] <- 5
depuracion_de_datos[[variable_impacto]] <- valores_numericos

depuracion_de_datos$unidad <- trimws(as.character(depuracion_de_datos$unidad))

depuracion_de_datos <- depuracion_de_datos %>%
  mutate(categoria_impacto = case_when(
    get(variable_impacto) == 5 ~ "Muy Alto",
    get(variable_impacto) == 4 ~ "Alto",
    get(variable_impacto) == 3 ~ "Medio",
    get(variable_impacto) == 2 ~ "Bajo",
    get(variable_impacto) == 1 ~ "Muy Bajo",
    TRUE ~ "Muy Alto"
  ))

niveles_impacto <- c("Muy Alto", "Alto", "Medio", "Bajo", "Muy Bajo")
depuracion_de_datos$categoria_impacto <- factor(depuracion_de_datos$categoria_impacto, levels = niveles_impacto)

test_kruskal <- kruskal.test(get(variable_impacto) ~ unidad, data = depuracion_de_datos)

cat("\n========================================================================\n")
cat("RESULTADO KRUSKAL-WALLIS (H4):\n")
cat("========================================================================\n")
cat("Estadístico H:", round(test_kruskal$statistic, 4), "\n")
cat("p-valor:", round(test_kruskal$p.value, 4), "\n\n")

# --- ESTADÍSTICOS DESCRIPTIVOS COMPLETOS GLOBAL VS UNIDADES ---
descriptivos_globales <- depuracion_de_datos %>%
  summarise(
    Ambito = "GLOBAL (Total Muestra)",
    N = n(),
    Media = mean(get(variable_impacto), na.rm = TRUE),
    Mediana = median(get(variable_impacto), na.rm = TRUE),
    Desv_Est = sd(get(variable_impacto), na.rm = TRUE),
    Minimo = min(get(variable_impacto), na.rm = TRUE),
    Maximo = max(get(variable_impacto), na.rm = TRUE)
  )

descriptivos_unidades <- depuracion_de_datos %>%
  filter(!is.na(unidad) & unidad != "") %>%
  group_by(unidad) %>%
  summarise(
    Ambito = paste("Unidad", unique(unidad)),
    N = n(),
    Media = mean(get(variable_impacto), na.rm = TRUE),
    Mediana = median(get(variable_impacto), na.rm = TRUE),
    Desv_Est = sd(get(variable_impacto), na.rm = TRUE),
    Minimo = min(get(variable_impacto), na.rm = TRUE),
    Maximo = max(get(variable_impacto), na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  select(-unidad) %>%
  arrange(Ambito)

tabla_descriptivos_completa <- rbind(descriptivos_globales, descriptivos_unidades)

tabla_impresion <- tabla_descriptivos_completa %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

cat("========================================================================\n")
cat("TABLA COMPARATIVA DE ESTADÍSTICOS (GLOBAL VS UNIDADES DE PAISAJE):\n")
cat("========================================================================\n")
print(as.data.frame(tabla_impresion))
cat("\n")

# --- DISTRIBUCIÓN PORCENTUAL Y GRÁFICO DE TARTA EN DOS CAPAS ---
df_porcentajes <- depuracion_de_datos %>% 
  count(categoria_impacto) %>%
  mutate(porcentaje = (n / 95) * 100)

cat("========================================================================\n")
cat("Distribución Porcentual Global (Muestra Rígida Total N=95):\n")
cat("========================================================================\n")
print(df_porcentajes)
cat("\n")

paleta_tarta <- c(
  "Muy Alto" = "#0c1d49", 
  "Alto"     = "#5cb1c5", 
  "Medio"    = "#a8d5ba", 
  "Bajo"     = "#e2f0db", 
  "Muy Bajo" = "#fcfebc"
)

df_grafico <- df_porcentajes %>%
  arrange(desc(categoria_impacto)) %>%
  mutate(
    pos_y_centro = cumsum(porcentaje) - (porcentaje / 2),
    etiqueta = paste0(sprintf("%.1f", porcentaje), "%")
  )

df_grandes <- df_grafico %>% filter(porcentaje > 10)
df_pequenos <- df_grafico %>% filter(porcentaje <= 10)

grafico_tarta <- ggplot(df_grafico, aes(x = "", y = porcentaje, fill = categoria_impacto)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 0.5) +
  coord_polar("y", start = 0, direction = -1) +
  scale_fill_manual(values = paleta_tarta, name = "Categorías de Impacto") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11, color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  
  geom_text(
    data = df_grandes,
    aes(y = pos_y_centro, label = etiqueta),
    color = ifelse(df_grandes$categoria_impacto %in= c("Muy Alto", "Alto"), "white", "black"),
    size = 4.5,
    fontface = "bold"
  ) +
  
  geom_text_repel(
    data = df_pequenos,
    aes(y = pos_y_centro, label = etiqueta),
    color = "black",
    size = 4.5,
    fontface = "bold",
    nudge_x = 0.65,
    segment.color = "grey50",
    segment.size = 0.4,
    direction = "y",
    show.legend = FALSE
  )

print(grafico_tarta)

# Análisis cualitativo final porcentual por unidades
tabla_unidades_porcentaje <- depuracion_de_datos %>%
  filter(!is.na(unidad)) %>%
  group_by(unidad, categoria_impacto) %>%
  summarise(n = n(), .groups = 'drop_last') %>%
  mutate(porcentaje = (n / sum(n)) * 100) %>%
  select(-n) %>%
  pivot_wider(names_from = categoria_impacto, values_from = porcentaje, values_fill = 0)

cat("========================================================================\n")
cat("Análisis Cualitativo por Unidad (%):\n")
cat("========================================================================\n")
print(as.data.frame(tabla_unidades_porcentaje))


