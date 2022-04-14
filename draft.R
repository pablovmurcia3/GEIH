library(readr)
caracteristicas <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Características generales, seguridad social en salud y educación.csv")
hogarVivie <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Datos del hogar y la vivienda.csv")
ft <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Fuerza de trabajo.csv")
migra <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Migración.csv")
noOcu <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/No ocupados.csv")
ocu <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Ocupados.csv")
otrasFor <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Otras formas de trabajo.csv")
OtrosIng  <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Otros ingresos e impuestos.csv")
tipo <- read_csv("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Tipo de investigación.csv")

caracteristicas <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Características generales, seguridad social en salud y educación.csv",";")
hogarVivie <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Datos del hogar y la vivienda.csv", ";")
ft <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Fuerza de trabajo.csv", ";")
migra <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Migración.csv", ";")
noOcu <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/No ocupados.csv", ";")
ocu <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Ocupados.csv", ";")
otrasFor <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Otras formas de trabajo.csv", ";")
OtrosIng  <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Otros ingresos e impuestos.csv", ";")
tipo <- read_delim("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - 2022/Febrero/Tipo de investigación.csv",";")




unique(caracteristicas$PERIODO)
unique(migra$PERIODO)

# Tienen orden
grep("ORDEN", names(caracteristicas),value = TRUE) # no
grep("ORDEN", names(hogarVivie) ,value = TRUE) # no
grep("ORDEN", names(ft), value = TRUE)
grep("ORDEN", names(migra), value = TRUE)
grep("ORDEN", names(noOcu), value = TRUE)
grep("ORDEN", names(ocu), value = TRUE)
grep("ORDEN", names(otrasFor), value = TRUE)
grep("ORDEN", names(OtrosIng), value = TRUE)
grep("ORDEN", names(tipo), value = TRUE)

# Tiene llaves al comienzo 

caracteristicas <- caracteristicas[,-grep("PERIODO", names(caracteristicas))]
hogarVivie <- hogarVivie[,-grep("PERIODO", names(hogarVivie))]
ft <- ft[,-grep("PERIODO", names(ft))]
migra <- migra[,-grep("PERIODO", names(migra))]
noOcu <- noOcu[,-grep("PERIODO", names(noOcu))]
ocu <- ocu[,-grep("PERIODO", names(ocu))]
otrasFor <- otrasFor[,-grep("PERIODO", names(otrasFor))]
OtrosIng <- OtrosIng[,-grep("PERIODO", names(OtrosIng))]
tipo <- tipo[,-grep("PERIODO", names(tipo))]



names(caracteristicas)[1:3]
names(hogarVivie)[1:3]
names(ft)[1:3]
names(migra)[1:3]
names(noOcu)[1:3]
names(ocu)[1:3]
names(otrasFor)[1:3]
names(OtrosIng)[1:3]
names(tipo)[1:3]



# merge
hogarVivie
caracteristicas
# 1 - los que no tienen orden
orden <- !(names(hogarVivie) %in% names(caracteristicas))
orden[1:2] <- TRUE
orden
hogarVivie <- hogarVivie[,orden]

merge1 <- merge(caracteristicas,hogarVivie, 
                by = c("DIRECTORIO", "SECUENCIA_P"),
                all = TRUE)




#  


orden <- !(names(ft) %in% names(merge1))
orden[1:3] <- TRUE
orden
ft <- ft[,orden]

merge2 <- merge(ft,merge1, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


# 2
orden <- !(names(migra) %in% names(merge2))
orden[1:3] <- TRUE
orden
migra <- migra[,orden]

merge3 <- merge(migra,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)

#3
orden <- !(names(noOcu) %in% names(merge3))
orden[1:3] <- TRUE
orden
noOcu <- noOcu[,orden]

merge4 <- merge(noOcu,merge3, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#4
orden <- !(names(ocu) %in% names(merge4))
orden[1:3] <- TRUE
orden
ocu <- ocu[,orden]

merge5 <- merge(ocu,merge4, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#5
orden <- !(names(otrasFor) %in% names(merge5))
orden[1:3] <- TRUE
orden
otrasFor <- otrasFor[,orden]

merge6 <- merge(otrasFor,merge5, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#6
orden <- !(names(OtrosIng) %in% names(merge6))
orden[1:3] <- TRUE
orden
OtrosIng <- OtrosIng[,orden]

merge7 <- merge(OtrosIng,merge6, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)




#7
orden <- !(names(tipo) %in% names(merge7))
orden[1:3] <- TRUE
orden
tipo <- tipo[,orden]

merge8 <- merge(tipo,merge7, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)

