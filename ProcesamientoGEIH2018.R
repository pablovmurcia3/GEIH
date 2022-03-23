library(readr)
caracteristicas <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Características generales, seguridad social en salud y educación.csv")
hogarVivie <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Datos del hogar y la vivienda.csv")
ft <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Fuerza de trabajo.csv")
migra <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Migración.csv")
noOcu <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/No ocupados.csv")
ocu <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Ocupados.csv")
otrasFor <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Otras formas de trabajo.csv")
OtrosIng  <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Otros ingresos e impuestos.csv")
tipo <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Enero/Tipo de investigación.csv")

unique(caracteristicas$PERIODO)
unique(migra$PERIODO)

# Tienen orden
grep("ORDEN", names(caracteristicas),value = TRUE) # no
grep("ORDEN", names(hogarVivie) ,value = TRUE)
grep("ORDEN", names(ft), value = TRUE)
grep("ORDEN", names(migra), value = TRUE)
names(noOcu)
names(ocu)
names(otrasFor)
names(OtrosIng)
names(tipo)
