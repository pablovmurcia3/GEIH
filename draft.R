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



names(caracteristicas)[1:2]
names(hogarVivie)[1:2]
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
names(merge1)
identical(merge1, caracteristicas)


sum(merge1$SECUENCIA_P)
sum(d$SECUENCIA_P)


# 1 los que tienen orden


orden <- !(names(ft) %in% names(migra))
orden[1:3] <- TRUE
orden
ft <- ft[,orden]

merge2 <- merge(ft,migra, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


# 2
orden <- !(names(noOcu) %in% names(merge2))
orden[1:3] <- TRUE
orden
noOcu <- noOcu[,orden]

merge2 <- merge(noOcu,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)

#3
orden <- !(names(ocu) %in% names(merge2))
orden[1:3] <- TRUE
orden
ocu <- ocu[,orden]

merge2 <- merge(ocu,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#4
orden <- !(names(otrasFor) %in% names(merge2))
orden[1:3] <- TRUE
orden
otrasFor <- otrasFor[,orden]

merge2 <- merge(otrasFor,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#5
orden <- !(names(OtrosIng) %in% names(merge2))
orden[1:3] <- TRUE
orden
OtrosIng <- OtrosIng[,orden]

merge2 <- merge(OtrosIng,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)


#6
orden <- !(names(tipo) %in% names(merge2))
orden[1:3] <- TRUE
orden
tipo <- tipo[,orden]

merge2 <- merge(tipo,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                all = TRUE)




orden <- !(names(merge1) %in% names(merge2))
orden[1:2] <- TRUE
orden
merge1 <- merge1[,orden]

mergeF <- merge(merge1,merge2, 
                by = c("DIRECTORIO", "SECUENCIA_P"),
                all = TRUE)
d <-list(ocu, merge1,otrasFor)
z <- d[-2]
identical(d,mergeF)
