install.packages("quantmod")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("lubridate")
install.packages("PerformanceAnalytics")
install.packages("purrr")
install.packages("zoo")

library(quantmod)
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(lubridate)
library(PerformanceAnalytics)
library(purrr)
library(zoo)

simbolos <- c(
  VIX = "^VIX",
  SP500 = "^GSPC",
  DOW = "^DJI",
  NASDAQ = "^IXIC",
  RUSSELL = "^RUT",
  GOLD = "GC=F",
  OIL = "CL=F",
  US10Y = "^TNX",
  US2Y = "^IRX",
  DOLLAR = "DX-Y.NYB",
  EUROPE = "^STOXX50E"
)

fecha_inicio <- "2015-01-01"
fecha_fin <- Sys.Date()
fecha_guerra <- as.Date("2022-02-24")
fecha_guerra_fin <- fecha_guerra + months(6)  # 6 meses después

descargar_datos_semanales <- function(simbolos, fecha_inicio, fecha_fin) {
  datos <- list()
  for (nombre in names(simbolos)) {
    simbolo <- simbolos[nombre]
    cat("Descargando", nombre, "...\n")
    
    tryCatch({
      temp <- getSymbols(simbolo, from = fecha_inicio, to = fecha_fin,
                         auto.assign = FALSE, warnings = FALSE)
      
      temp_weekly <- to.weekly(temp, indexAt = "endof")
      cierre <- Ad(temp_weekly)
      
      df <- data.frame(
        Fecha = index(cierre),
        Valor = as.numeric(cierre)
      )
      colnames(df)[2] <- nombre
      datos[[nombre]] <- df
    }, error = function(e) {
      cat("Error al descargar", nombre, ":", e$message, "\n")
    })
  }
  
  datos <- purrr::compact(datos)
  
  if (length(datos) == 0) stop("No se pudieron descargar datos.")
  
  datos_completos <- reduce(datos, full_join, by = "Fecha")
  return(datos_completos)
}

datos_semanales <- descargar_datos_semanales(simbolos, fecha_inicio, fecha_fin)

datos_semanales <- datos_semanales[rowSums(is.na(datos_semanales)) < (ncol(datos_semanales)-1)/2, ]

datos_semanales <- datos_semanales %>%
  arrange(Fecha) %>%
  mutate(across(-Fecha, ~na.approx(.x, na.rm = FALSE)))

datos_semanales <- na.omit(datos_semanales)


datos_semanales <- datos_semanales %>%
  mutate(Guerra6m = ifelse(Fecha >= fecha_guerra & Fecha <= fecha_guerra_fin, 1, 0))

write.csv(datos_semanales, "datos_mercados_semanales.csv", row.names = FALSE)

# 1. Estadísticas descriptivas (sin agrupar por periodo)
estadisticas_descriptivas <- datos_semanales %>%
  dplyr::select(-Fecha) %>%
  summarise(across(everything(),
                   list(
                     Media = ~mean(., na.rm = TRUE),
                     SD = ~sd(., na.rm = TRUE),
                     Min = ~min(., na.rm = TRUE),
                     Max = ~max(., na.rm = TRUE),
                     Mediana = ~median(., na.rm = TRUE)
                   )))

print(estadisticas_descriptivas)
write.csv(estadisticas_descriptivas, "estadisticas_descriptivas.csv", row.names = FALSE)

# 2. Visualización de serie temporal del VIX
ggplot(datos_semanales, aes(x = Fecha, y = VIX)) +
  geom_line(linewidth = 1, color = "steelblue") +
  geom_vline(xintercept = as.numeric(fecha_guerra), linetype = "dashed", color = "red") +
  geom_vline(xintercept = as.numeric(fecha_guerra_fin), linetype = "dashed", color = "darkred") +
  labs(title = "Evolución del VIX (con periodo de los primeros 6 meses de guerra)",
       x = "Fecha", y = "VIX") +
  theme_minimal()

# 3. Matriz de correlación
correlaciones <- cor(datos_semanales %>% dplyr::select(-Fecha), use = "complete.obs")

corrplot(correlaciones, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Matriz de Correlación entre Variables",
         mar = c(0, 0, 2, 0))

# Correlaciones con el VIX
correlaciones_vix <- data.frame(
  Variable = rownames(correlaciones),
  Correlacion_con_VIX = correlaciones[, "VIX"]
) %>%
  arrange(desc(abs(Correlacion_con_VIX))) %>%
  filter(Variable != "VIX")

print(correlaciones_vix)
write.csv(correlaciones, "matriz_correlaciones.csv", row.names = TRUE)
write.csv(correlaciones_vix, "correlaciones_vix.csv", row.names = FALSE)


# 4. Gráfico de caja del VIX por dummy de guerra
ggplot(datos_semanales, aes(x = factor(Guerra6m), y = VIX, fill = factor(Guerra6m))) +
  geom_boxplot() +
  labs(title = "Distribución del VIX durante y fuera de los primeros 6 meses de guerra",
       x = "Guerra6m (1 = primeros 6 meses de guerra)", y = "VIX") +
  theme_minimal()

# 5. Cambios porcentuales (retornos semanales)
datos_cambios <- datos_semanales %>%
  arrange(Fecha) %>%
  mutate(across(-c(Fecha, Guerra6m), ~ .x / lag(.x) - 1)) %>%
  na.omit()

correlaciones_cambios <- cor(datos_cambios %>% dplyr::select(-Fecha, -Guerra6m),
                             use = "complete.obs")

corrplot(correlaciones_cambios, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         title = "Correlación entre Cambios Semanales (Retornos)",
         mar = c(0, 0, 2, 0))

write.csv(correlaciones_cambios, "correlaciones_cambios.csv", row.names = TRUE)

# Modelos
##1. Impacto de la guerra en el VIX (modelo simple)

modelo_guerra <- lm(VIX ~ Guerra6m, data = datos_semanales)
summary(modelo_guerra)

# Modelos por Asset Class
#2. Riesgosos

modelo_riesgo <- lm(VIX~ Guerra6m + SP500 + DOW + NASDAQ + RUSSELL + EUROPE, data = datos_semanales)

summary(modelo_riesgo)
#3. Refugio + COMMODITIES

modelo_refugio <- lm(VIX~ Guerra6m + GOLD + OIL + US10Y + US2Y + DOLLAR, data = datos_semanales)

summary(modelo_refugio)

# 4. Modelo Multivariable (Datos Semanales)
modelo_multivariable <- lm(VIX ~ Guerra6m + SP500 + DOW + NASDAQ + RUSSELL + 
       GOLD + OIL + US10Y + US2Y + DOLLAR + EUROPE,
      data = datos_semanales)
summary(modelo_multivariable)

## 5. Modelo en términos de retornos semanales 
datos_retornos <- datos_semanales %>%
  arrange(Fecha) %>%
  mutate(across(-c(Fecha, Guerra6m), ~ (.x / lag(.x) - 1))) %>%
  na.omit()

modelo_retornos <- lm(VIX ~ Guerra6m + SP500 + DOW + NASDAQ + RUSSELL + 
                        GOLD + OIL + US10Y + US2Y + DOLLAR + EUROPE,
                      data = datos_retornos)

summary(modelo_retornos)

# Prueba T
test_vix <- t.test(VIX ~ Guerra6m, data = datos_semanales)

print(test_vix)

aggregate(VIX ~ Guerra6m, data = datos_semanales, mean)