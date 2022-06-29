Ene <- ProcesamientoGEIH2005("Área","2021" ,"Enero")
Feb <- ProcesamientoGEIH2005("Área","2021" ,"Febrero")
Mar <- ProcesamientoGEIH2005("Área","2021" ,"Marzo")
May <- ProcesamientoGEIH2005("Área","2021" ,"Mayo")
Jun <- ProcesamientoGEIH2005("Área","2021" ,"Junio")
Jul <- ProcesamientoGEIH2005("Área","2021" ,"Julio")
Ago <- ProcesamientoGEIH2005("Área","2021" ,"Agosto")
Octu <- ProcesamientoGEIH2005("Área","2021" ,"Octubre")
Nov <- ProcesamientoGEIH2005("Área","2021" ,"Noviembre")
Dic <- ProcesamientoGEIH2005("Área","2021" ,"Diciembre")
library(haven)
Abr <- read_dta("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/Faltantes/Area2021m4.dta")
Sep <- read_dta("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/Faltantes/area2021m9.dta")
Nov <- read_dta("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/Faltantes/area2021m11.dta")
Oct <- read_dta("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/Faltantes/area2021m10.dta")
################################################################################

names(Abr) <- toupper(names(Abr))
names(Sep) <- toupper(names(Sep))
names(Nov) <- toupper(names(Nov))
names(Oct) <- toupper(names(Oct))

names(Abr)[names(Abr) == "FEX_C_2011"] <-"fex_c_2011"
names(Sep)[names(Sep) == "FEX_C_2011"] <-"fex_c_2011"
names(Nov)[names(Nov) == "FEX_C_2011"] <-"fex_c_2011"
names(Oct)[names(Oct) == "FEX_C_2011"] <-"fex_c_2011"


##################### Gráfica 3 ############################################

G3 <- function(A){
  # Filtrad0
  bog <- A[A$AREA == 11, ]
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  
  # Preparación  
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))
  
  ##################### Gráfica 3 ############################################
  
  PET_H <- sum(bog[bog$PET == 1 & bog$P6020 == 1,]$fex_c_2011)
  PET_M <- sum(bog[bog$PET == 1 & bog$P6020 == 2,]$fex_c_2011)
  
  PEA_H <- sum(bog[bog$PEA == 1 & bog$P6020 == 1,]$fex_c_2011)
  PEA_M <- sum(bog[bog$PEA == 1 & bog$P6020 == 2,]$fex_c_2011)
  
  INAC_H <- PET_H - PEA_H
  INAC_M <- PET_M - PEA_M
  INAC_H
  INAC_M
  
  ##################### Vector Resultante ####################################
  
  
  Bogota <- c(INAC_H, INAC_M)
  
  names <- c("Inactivos hombres", "Inactivos mujeres")
  
  stats <- data.frame(names, Bogota)
  stats$Bogota <- round(as.numeric(stats$Bogota))
  stats
  
  
}


G3(Ene)
G3(Feb)
G3(Mar)
G3(Abr)
G3(May)
G3(Jun)
G3(Jul)
G3(Ago)
G3(Sep)
G3(Oct)
G3(Nov)
G3(Dic)


##################### Gráfica 4 ############################################

G4 <- function(A){
  # Filtrad0
  bog <- A[A$AREA == 11, ]
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  
  # Preparación  
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))
  
  ##################### Gráfica 4 ############################################
  
  bog_H <- bog[complete.cases(bog$P7458) & bog$P6020 == 1,]
  bog_M <- bog[complete.cases(bog$P7458) & bog$P6020 == 2,]
  
  list_H <- split(bog_H,bog_H$P7458) 
  
  Razones_H <- sapply(list_H, function(x) {
    r <- sum(x[x$INI ==1,]$fex_c_2011)
  }) 
  Razones_H
  
  
  list_M <- split(bog_M,bog_M$P7458) 
  
  Razones_M <- sapply(list_M, function(x) {
    r <- sum(x[x$INI ==1,]$fex_c_2011)
  }) 
  Razones_M
  
  
  
  dictio_replace= c("1"= "No hay trabajo disponible en la ciudad o región", 
                    "2"= "Para dedicarse a estudiar",
                    "3"= "No sabe como buscarlo",
                    "4"= "Por enfermedad",
                    "5"= "Está cansado de buscar", 
                    "6" = "No encuentra el trabajo apropiado", 
                    "7"= "Considera que no está calificado",
                    "8" = "Por la edad",
                    "9" ="Responsabilidades familiares",
                    "10" ="Jubilación o retiro",
                    "11"="No desea trabajar",
                    "12"= "Otra")
  
  library(stringr)
 

  
  ##################### Vector Resultante ####################################
  
  
  Bogota <- c(Razones_H, Razones_M)
  
  names <- c(names(Razones_H),names(Razones_M))
  
  stats <- data.frame(names, round(Bogota))
  stats
  
  
}

G4(Ene)
G4(Feb)
G4(Mar)
G4(Abr)
G4(May)
G4(Jun)
G4(Jul)
G4(Ago)
G4(Sep)
G4(Oct)
G4(Nov)
G4(Dic)



bog <- A1[A1$AREA == 11, ]
bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))

bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
bog$PEA[is.na(bog$PEA)] <- 0
bog$INI[is.na(bog$INI)] <- 0
bog$PET <- ifelse(bog$P6040 >= 12,1,0)

bog_H <- bog[bog$P6020 == 1,]
bog_M <- bog[bog$P6020 == 2,]

sum(bog[bog$INI == 1,]$fex_c_2011)
sum(bog_H[bog_H$INI == 1,]$fex_c_2011)
sum(bog_M[bog_M$INI == 1,]$fex_c_2011)

INAC  <- sum(bog[bog$INI == 1,]$fex_c_2011)
INAC_H <- sum(bog_H[bog_H$INI ==1,]$fex_c_2011)
INAC_M <-  sum(bog_M[bog_M$INI ==1,]$fex_c_2011)

#INAC <- PET - PEA
#INAC

#PET_H <- sum(bog[bog$PET == 1 & bog$P6020 == 1,]$fex_c_2011)
#PET_M <- sum(bog[bog$PET == 1 & bog$P6020 == 2,]$fex_c_2011)

#PEA_H <- sum(bog[bog$PEA == 1 & bog$P6020 == 1,]$fex_c_2011)
#PEA_M <- sum(bog[bog$PEA == 1 & bog$P6020 == 2,]$fex_c_2011)


#INAC_H <- PET_H - PEA_H
#INAC_M <- PET_M - PEA_M
#INAC_H
#INAC_M


# G4


bog_H_1 <- bog_H[complete.cases(bog_H$P7458),]
bog_M_1 <- bog_M[complete.cases(bog_M$P7458),]

list_H <- split(bog_H_1,bog_H_1$P7458) 
INAC_H_1 <- sum(bog_H_1[bog_H_1$INI ==1,]$fex_c_2011)

Razones_H <- sapply(list_H, function(x) {
  r <- sum(x[x$INI ==1,]$fex_c_2011)
  INAC_H_1
  prop <- r/INAC_H_1*100
}) 
Razones_H
sum(Razones_H)


list_M <- split(bog_M_1,bog_M_1$P7458) 
INAC_M_1 <- sum(bog_M_1[bog_M_1$INI ==1,]$fex_c_2011)

Razones_M <- sapply(list_M, function(x) {
  r <- sum(x[x$INI ==1,]$fex_c_2011)
  INAC_M_1
  prop <- r/INAC_M_1*100
}) 
Razones_M
sum(Razones_M)

names(Razones_H)
dictio_replace= c("1"= "No hay trabajo disponible en la ciudad o región", 
                  "2"= "Para dedicarse a estudiar",
                  "3"= "No sabe como buscarlo",
                  "4"= "Por enfermedad",
                  "5"= "Está cansado de buscar", 
                  "6" = "No encuentra el trabajo apropiado", 
                  "7"= "Considera que no está calificado",
                  "8" = "Por la edad",
                  "9" ="Responsabilidades familiares",
                  "10" ="Jubilación o retiro",
                  "11"="No desea trabajar",
                  "12"= "Otra")

library(stringr)
names(Razones_H) <- str_replace_all(names(Razones_H) ,dictio_replace)
names(Razones_M) <- str_replace_all(names(Razones_M) ,dictio_replace)
names(Razones_H) <- paste(names(Razones_H), "(H)")
names(Razones_M) <- paste(names(Razones_M), "(M)")


c(names(Razones_M),names(Razones_H) )
Razones_H <- unname(Razones_H)
Razones_M <- unname(Razones_M)


c(INAC_M,Razones_M)

# G8

bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))


bog_age_H <- bog_H[complete.cases(bog_H$age),]
bog_age_M <- bog_M[complete.cases(bog_M$age),]

list_H_age <- split(bog_age_H,bog_age_H$age) 
TD_H_age <- sapply(list_H_age, function(x) {
  PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
  Desempleados <- sum(x[complete.cases(x$DSI) & x$PET == 1,]$fex_c_2011)
  TD <- Desempleados/PEA*100
}) 
TD_H_age

list_M_age <- split(bog_age_M,bog_age_M$age) 
TD_M_age <- sapply(list_M_age, function(x) {
  PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
  Desempleados <- sum(x[complete.cases(x$DSI) & x$PET == 1,]$fex_c_2011)
  TD <- Desempleados/PEA*100
}) 
TD_M_age


names(TD_H_age) <- paste(names(TD_H_age), "(H)")
names(TD_M_age) <- paste(names(TD_M_age), "(M)")


Razones_H
sum(Razones_H)

class(bog$P6040)






colombia$age <- cut(colombia$P6040, breaks = c(12,28,39,49, 59, 130))

colombiaAge <- colombia[complete.cases(colombia$age),]

list <- split(colombiaAge, colombiaAge$age) 
TDA_COL <- sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA) & x$P6040 >= 15 ,]$FEX_C18)
  Desempleados <- sum(x[complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
  TD <- Desempleados/PEA*100
})   



