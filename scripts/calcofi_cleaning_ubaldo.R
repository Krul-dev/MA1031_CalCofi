# ==================================================
# Data Cleaning
# Proyecto MA1031_CalCofi
# por Ubaldo Martínez
# ==================================================


# ============================================================
# 1. Carga de datos originales (se conserva el dataframe completo)
# ============================================================
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))
getwd()

datos_originales <- read.csv("194903-202105_Bottle.csv")

# Identificador como factor
datos_originales$Btl_Cnt <- as.factor(datos_originales$Btl_Cnt)


# ============================================================
# 2. Porcentaje de celdas vacías por variable (orden descendente)
# ============================================================
missing_summary <- data.frame(
  variable = names(datos_originales),
  pct_missing = round(
    sapply(datos_originales, function(x) mean(is.na(x)) * 100),
    2
  )
)

missing_summary <- missing_summary[order(-missing_summary$pct_missing), ]
missing_summary
rm(missing_summary)


# ============================================================
# 3. Subconjunto del dataframe original
# ============================================================
datos <- datos_originales[,
                          c("Btl_Cnt", "Depthm", "T_degC",
                            "Salnty", "O2ml_L", "O2Sat",
                            "NO3uM", "PO4uM", "ChlorA", 
                            "R_Depth",
                            "STheta")]

# ============================================================
# 4. Subconjunto de renglones con información completa
#    (dataset final para análisis estadístico)
# ============================================================
datos <- datos[complete.cases(datos), ]


# ============================================================
# 5. Crea variable categórica de zona de profundidad usando cuartiles
# ============================================================
quantile(datos$R_Depth)
# Depth zones based on quartiles
datos$Depth_zone <- cut(
  datos$R_Depth,
  breaks = c(0, 20, 61, 120, Inf),
  labels = c(
    "Very shallow",
    "Shallow",
    "Intermediate",
    "Deep"
  ),
  include.lowest = TRUE
)

# Eliminar la variable  original usada para crear categorías
datos$R_Depth <- NULL

# ============================================================
# 6. Crea variable categórica Water density class usando terciles
# ============================================================

datos$Water_density_class <- cut(
  datos$STheta,
  breaks = quantile(datos$STheta, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
  labels = c("Light", "Intermediate", "Dense"),
  include.lowest = TRUE
)

# Eliminar la variable  original usada para crear categorías
datos$STheta  <- NULL


# ============================================================
# 7. Verificación final de formato del dataframe
# ============================================================

names(datos)

datos$Depth_zone <- as.factor(datos$Depth_zone)
datos$Water_density_class <- as.factor(datos$Water_density_class)

table(datos$Depth_zone)
table(datos$Water_density_class)

summary(datos$T_degC)
quantile(datos$T_degC)
boxplot(datos$T_degC, horizontal = TRUE)
hist(datos$T_degC)

boxplot(T_degC ~ Depth_zone, data = datos, horizontal=TRUE)
boxplot(T_degC ~ Water_density_class, data = datos, horizontal=TRUE)

library(ggplot2)
# Histogram by group in ggplot2
ggplot(datos, aes(x = T_degC, fill = Water_density_class)) + 
  geom_histogram(alpha = .5, position = "identity") 

ggplot(datos, aes(x = Depthm, fill = Depth_zone)) + 
  geom_histogram(alpha = .5, position = "identity") 

quantile(datos$Depthm)
boxplot(datos$Depthm, horizontal=TRUE)

library(dplyr)
datos <- datos %>% filter(Btl_Cnt != "890616")


# ============================================================
# 8. Guarda dataframe en archivo CSV
# ============================================================

write.csv(
  datos,
  file = "datos_calcofi2026.csv",
  row.names = FALSE,
  na = "")

# ======
# FIN
# ======