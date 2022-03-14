library(readr)

file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)

file_listC <- grep("Cabecera",file_list, value=TRUE)
file_listR <- grep("Resto",file_list, value=TRUE)
file_listA <- grep("Área",file_list, value=TRUE)

L2021C <- list()
L2021R <- list()
L2021A <- list()

for (i in 1:length(file_listC)) {
  L2021C[[i]] <- read.csv(file_listC[i], sep = ";")
  L2021R[[i]] <- read.csv(file_listR[i], sep = ";")
  L2021A[[i]] <- read.csv(file_listA[i], sep = ";")
}

file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
names(L2021A) <- grep("Cabecera",file_list, value=TRUE)
names(L2021C) <- grep("Cabecera",file_list, value=TRUE)
names(L2021R) <- grep("Resto",file_list, value=TRUE)



#

C <- d[[1]]
D <- d[[2]]                          
H   <- d[[3]]                  
Inactivos     <- L2021A[[4]]                       
Ocupados   <- L2021A[[5]]                          
Otras<- L2021A[[6]]
Otrosingresos  <- L2021A[[7]]                       
vivien <- L2021A[[8]] # único que no tiene orden



C$ID <- paste(as.character(C$DIRECTORIO),
              as.character(C$SECUENCIA_P),
              as.character(C$ORDEN), sep = "")


D$ID <- paste(as.character(D$DIRECTORIO),
              as.character(D$SECUENCIA_P),
              as.character(D$ORDEN), sep = "")



H$ID <- paste(as.character(H$DIRECTORIO),
              as.character(H$SECUENCIA_P),
              as.character(H$ORDEN), sep = "")


library(plyr)
S<-join(C,D, by ="ID") ##
co



?join
names(C)
names(S)
dim(S)
unique(D$DSCY)
unique(C$RAMA2D_D_R4)
unique(S$DSCY)

##############################################################################




procesamiento <- function(zona) {
  file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)
  
  file_zona <- grep(zona, file_list, value=TRUE)
  
  L2021 <- list()
  for (i in 1:length(file_zona)) {
    L2021[[i]] <- read.csv(file_zona[i], sep = ";")
  }
  
  file_names <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
  names(L2021) <- grep(zona,file_names, value=TRUE)
  L2021
  
}


d <- procesamiento("Cabecera")
names(d)

Características <- d[[1]]
Desocupados <- d[[2]]



orden <- !(names(Desocupados) %in% names(Características))
orden[1:3] <- TRUE
orden

Desocupados <- Desocupados[,orden]


merg1 <- merge(Características,Desocupados, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)

names(merg1)


Fuerza <- d[[3]]

orden <- !(names(Fuerza) %in% names(merg1))
orden[1:3] <- TRUE
orden

Fuerza <- Fuerza[,orden]

merg1 <- merge(merg1, Fuerza, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)

Inactivos <- d[[4]]
names(Inactivos)
orden <- !(names(Inactivos) %in% names(merg1))
orden[1:3] <- TRUE
orden

Inactivos <- Inactivos[, orden]

merg1 <- merge(merg1, Inactivos, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)


Ocupados <- d[[5]]
names(Ocupados)
orden <- !(names(Ocupados) %in% names(merg1))
orden[1:3] <- TRUE
orden

Ocupados <- Ocupados[, orden]

merg1 <- merge(merg1, Ocupados, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)



otrasAct <- d[[6]]
names(otrasAct)
orden <- !(names(otrasAct) %in% names(merg1))
orden[1:3] <- TRUE
orden

otrasAct <- otrasAct[, orden]

merg1 <- merge(merg1, otrasAct, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)


names(d)
otroIngresos <- d[[7]]
names(otrasAct)
orden <- !(names(otroIngresos) %in% names(merg1))
orden[1:3] <- TRUE
orden

otroIngresos <- otroIngresos[, orden]

merg1 <- merge(merg1, otroIngresos, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)

names(d)
Vivienda <- d[[8]]
names(Vivienda)
orden <- !(names(Vivienda) %in% names(merg1))
orden[1:2] <- TRUE
orden


Vivienda <- Vivienda[, orden]

merg1 <- merge(merg1, Vivienda, 
               by = c("DIRECTORIO", "SECUENCIA_P"),
               all = TRUE)

###########################################################################


merge

d <- procesamiento("Cabecera")
names(d)


Características <- d[[1]]
Vivienda <- d[[8]]


orden <- !(names(Vivienda) %in% names(Características))
orden[1:2] <- TRUE
orden


Vivienda <- Vivienda[, orden]

merg2 <- merge(Características, Vivienda, 
               by = c("DIRECTORIO", "SECUENCIA_P"),
               all = TRUE)



Desocupados <- d[[2]]

orden <- !(names(Desocupados) %in% names(merg2))
orden[1:3] <- TRUE
orden

Desocupados <- Desocupados[,orden]


merg2 <- merge(merg2,Desocupados, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)




Fuerza <- d[[3]]

orden <- !(names(Fuerza) %in% names(merg2))
orden[1:3] <- TRUE
orden

Fuerza <- Fuerza[,orden]

merg2 <- merge(merg2, Fuerza, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)


Inactivos <- d[[4]]
names(Inactivos)
orden <- !(names(Inactivos) %in% names(merg2))
orden[1:3] <- TRUE
orden

Inactivos <- Inactivos[, orden]

merg2 <- merge(merg2, Inactivos, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)


Ocupados <- d[[5]]
names(Ocupados)
orden <- !(names(Ocupados) %in% names(merg2))
orden[1:3] <- TRUE
orden

Ocupados <- Ocupados[, orden]

merg2 <- merge(merg2, Ocupados, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)



otrasAct <- d[[6]]
names(otrasAct)
orden <- !(names(otrasAct) %in% names(merg2))
orden[1:3] <- TRUE
orden

otrasAct <- otrasAct[, orden]

merg2 <- merge(merg2, otrasAct, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)


names(d)
otroIngresos <- d[[7]]
names(otrasAct)
orden <- !(names(otroIngresos) %in% names(merg2))
orden[1:3] <- TRUE
orden

otroIngresos <- otroIngresos[, orden]

merg2 <- merge(merg2, otroIngresos, 
               by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
               all = TRUE)
names(merg1)


identical(dataJoin,merg2)



merg1 <-merg1[ , order(names(merg1))]
merg2 <-merg2[ , order(names(merg2))]

identical(merg1, merg2)



##################################################################

procesamiento <- function(zona) {
  
  file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)
  
  file_zona <- grep(zona, file_list, value=TRUE)
  
  lista <- list()
  
  for (i in 1:length(file_zona)) {
    lista[[i]] <- read.csv(file_zona[i], sep = ";")
  }
  
  file_names <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
  names(lista) <- grep(zona,file_names, value=TRUE)
  lista

}

lista <- procesamiento("Resto")
names(lista) <- toupper(stri_trans_general(names(lista),"Latin-ASCII"))


datCaracteristicas <- lista[[grep("CARACTERISTICAS", names(lista))]]
datVivienda <- lista[[grep("VIVIENDA", names(lista))]]

orden <- !(names(datVivienda) %in% names(datCaracteristicas))
orden[1:2] <- TRUE
datVivienda <- datVivienda[, orden]
dataJoin <- merge(datCaracteristicas, datVivienda, 
                  by = c("DIRECTORIO", "SECUENCIA_P"),
                  all = TRUE)

lista <- lista[-grep("VIVIENDA", names(lista))]
lista <- lista[-grep("CARACTERISTICAS", names(lista))]


for (i in 1:length(lista)) {
  data <- lista[[i]]
  orden <- !(names(data) %in% names(dataJoin))
  orden[1:3] <- TRUE
  orden
  data <- data[,orden]
  dataJoin <- merge(dataJoin,data, 
                    by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                    all = TRUE)
  
}

