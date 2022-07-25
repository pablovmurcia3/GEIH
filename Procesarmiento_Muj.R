install.packages("writexl")
library(writexl)
Ene <- ProcesamientoGEIH2005("Área","2021" ,"Enero")
Feb <- ProcesamientoGEIH2005("Área","2021" ,"Febrero")
Mar <- ProcesamientoGEIH2005("Área","2021" ,"Marzo")
May <- ProcesamientoGEIH2005("Área","2021" ,"Mayo")
Jun <- ProcesamientoGEIH2005("Área","2021" ,"Junio")
Jul <- ProcesamientoGEIH2005("Área","2021" ,"Julio")
Ago <- ProcesamientoGEIH2005("Área","2021" ,"Agosto")
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


# G8

G8 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  
  ##################### Gráfica 8 ############################################
  
  bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))
  bog_H <- bog[complete.cases(bog$age) & bog$P6020 == 1,]
  bog_M <- bog[complete.cases(bog$age) & bog$P6020 == 2,]
  
  PEA_H <- sum(bog_H[bog_H$PEA == 1 & bog_H$PET == 1,]$fex_c_2011)
  Desempleados_H <- sum(bog_H[bog_H$DSI == 1 & bog_H$PET == 1,]$fex_c_2011)
  TD_H <- Desempleados_H/PEA_H
  
  PEA_M <- sum(bog_M[bog_M$PEA == 1 & bog_M$PET == 1,]$fex_c_2011)
  Desempleados_M <- sum(bog_M[bog_M$DSI == 1 & bog_M$PET == 1,]$fex_c_2011)
  TD_M <- Desempleados_M/PEA_M
  
  
  
  list_H_age <- split(bog_H,bog_H$age) 
  TD_H_age <- sapply(list_H_age, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[x$DSI == 1 & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA
  }) 
  TD_H_age
  
  list_M_age <- split(bog_M,bog_M$age) 
  TD_M_age <- sapply(list_M_age, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[x$DSI == 1 & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA
  }) 
  TD_M_age
  

  
  library(stringr)
  
  
  
  ##################### Vector Resultante ####################################
  
  hombres <- c(TD_H_age, TD_H)
  mujeres <- c(TD_M_age, TD_M)
  
  names_H <-  c(names(TD_H_age), "total")
  names_M <-  c(names(TD_M_age),  "total")
  
  stats <- data.frame(names_H, hombres,  names_M, mujeres )
  
  stats
  
}


write_xlsx(G8(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G8(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G8(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G8(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G8(May),paste0("output/May.xlsx"))
write_xlsx(G8(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G8(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G8(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G8(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G8(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G8(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G8(Dic),paste0("output/Dic.xlsx"))


# G9



G9 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  
  ##################### Gráfica 9 ############################################
  
  bog_H <- bog[complete.cases(bog$P6220) & bog$P6020 == 1,]
  bog_M <- bog[complete.cases(bog$P6220) & bog$P6020 == 2,]
  
  
  list_H_educ <- split(bog_H,bog_H$P6220) 
  TD_H_educ <- sapply(list_H_educ, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[x$DSI == 1 & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA
  }) 
  
  
  list_M_educ <- split(bog_M,bog_M$P6220) 
  TD_M_educ <- sapply(list_M_educ, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[x$DSI == 1 & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA
  }) 
  
  
  
  ##################### Vector Resultante ####################################
  
 
  names <-  c(names(TD_H_educ), names(TD_M_educ))
  values <- c(TD_H_educ, TD_M_educ)
  
  stats <- data.frame(names,values)
  
  stats
  
}





write_xlsx(G9(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G9(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G9(May),paste0("output/May.xlsx"))
write_xlsx(G9(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G9(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G9(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G9(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G9(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G9(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G9(Dic),paste0("output/Dic.xlsx"))

# G12

Ene$OCI

G12 <- function(A){
  # Filtrad0
  bog <- A[A$AREA == 11, ]
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  
  # Preparación  
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$OCI[is.na(bog$OCI)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  
  ##################### Gráfica 12 ############################################
  
  OCU_H <- sum(bog[bog$PET == 1 & bog$P6020 == 1 & bog$OCI == 1,]$fex_c_2011)
  OCU_M <- sum(bog[bog$PET == 1 & bog$P6020 == 2 & bog$OCI == 1,]$fex_c_2011)
  
  ##################### Vector Resultante ####################################
  
  
  Bogota <- c(OCU_H, OCU_M)
  
  names <- c("Ocupados hombres", "Ocupados mujeres")
  
  stats <- data.frame(names, Bogota)
  stats$Bogota <- round(as.numeric(stats$Bogota))
  stats
  
}


write_xlsx(G12(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G12(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G12(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G12(Abr),paste0("output/Abr.xlsx"))

write_xlsx(G12(May),paste0("output/May.xlsx"))
write_xlsx(G12(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G12(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G12(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G12(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G12(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G12(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G12(Dic),paste0("output/Dic.xlsx"))


# G13




G13 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  bog$OCI[is.na(bog$OCI)] <- 0
  
  ##################### Gráfica 8 ############################################
  
  bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))
  bog_H <- bog[complete.cases(bog$age) & bog$P6020 == 1,]
  bog_M <- bog[complete.cases(bog$age) & bog$P6020 == 2,]

  
  list_H_ocu <- split(bog_H,bog_H$age) 
  Ocu_H_age <- sapply(list_H_ocu, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 

  list_M_ocu <- split(bog_M,bog_M$age) 
  Ocu_M_age <- sapply(list_M_ocu, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 

  
  ##################### Vector Resultante ####################################
  
  names_H <-  c(names(Ocu_H_age))
  names_M <-  c(names(Ocu_M_age))
  
  stats <- data.frame(names_H, Ocu_H_age,  names_M, Ocu_M_age )
  
  stats
  
}


write_xlsx(G13(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G13(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G13(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G13(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G13(May),paste0("output/May.xlsx"))
write_xlsx(G13(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G13(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G13(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G13(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G13(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G13(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G13(Dic),paste0("output/Dic.xlsx"))


# G14


G14 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  bog$OCI[is.na(bog$OCI)] <- 0
  bog$P6220[is.na(bog$P6220)] <- 1
  
  
  ##################### Gráfica 14 ############################################
  
  bog_H <- bog[bog$P6020 == 1,]
  bog_M <- bog[bog$P6020 == 2,]
  
  
  list_H_ocu <- split(bog_H,bog_H$P6220)
  Ocu_H_educ <- sapply(list_H_ocu, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 
  
  list_M_ocu <- split(bog_M,bog_M$P6220) 
  Ocu_M_educ <- sapply(list_M_ocu, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 
  
  
  ##################### Vector Resultante ####################################
  
  names <-  c(names(Ocu_H_educ), names(Ocu_M_educ))
  values <- c(Ocu_H_educ, Ocu_M_educ)
  
  stats <- data.frame(names,values)
  
  stats
  
}


write_xlsx(G14(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G14(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G14(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G14(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G14(May),paste0("output/May.xlsx"))
write_xlsx(G14(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G14(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G14(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G14(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G14(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G14(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G14(Dic),paste0("output/Dic.xlsx"))



# G15

Ene$P6220[P6220 == 5] <- 4

unique(Ene$P6220)
G15 <- function(A){
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  bog$OCI[is.na(bog$OCI)] <- 0
  bog$P6220[is.na(bog$P6220)] <- 1
  bog$P6220[bog$P6220 == 5] <- 4
  bog$age <- cut(bog$P6040, breaks = c(13,28,59, 130))
  
  ##################### Gráfica 15.1 ############################################
  
  
  
  
  bog_H <- bog[bog$P6020 == 1,]
  bog_M <- bog[bog$P6020 == 2,]
  
  hombre <- weighted.mean(bog_H$INGLABO, bog_H$fex_c_2011, na.rm =TRUE)
  mujer <- weighted.mean(bog_M$INGLABO, bog_M$fex_c_2011, na.rm =TRUE)
  
  
  list_H_ocu <- split(bog_H,bog_H$P6220)
  Ing_H_educ <- sapply(list_H_ocu, function(x) {
    wm <- weighted.mean(x$INGLABO, x$fex_c_2011, na.rm =TRUE)
  }) 
  
  list_M_ocu <- split(bog_M,bog_M$P6220) 
  Ing_M_educ <- sapply(list_M_ocu, function(x) {
    wm <- weighted.mean(x$INGLABO, x$fex_c_2011, na.rm =TRUE)
  }) 
  
  ##################### Gráfica 15.2 ############################################
  
  list_H_age <- split(bog_H,bog_H$age) 
  Ing_H_age <- sapply(list_H_age, function(x) {
    wm <- weighted.mean(x$INGLABO, x$fex_c_2011, na.rm =TRUE)
  }) 
  
  list_M_age <- split(bog_M,bog_M$age) 
  Ing_M_age <- sapply(list_M_age, function(x) {
    wm <- weighted.mean(x$INGLABO, x$fex_c_2011, na.rm =TRUE)
  }) 
  
  
  ##################### Vector Resultante ####################################
  
  names <-  c(names(Ing_H_educ), names(list_M_ocu), names(Ing_H_age),
              names(Ing_M_age), "hombre", "mujer")
  values <- c(Ing_H_educ, Ing_M_educ,Ing_H_age,Ing_M_age, hombre, mujer)
  
  stats <- data.frame(names,values)
  
  stats
}

write_xlsx(G15(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G15(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G15(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G15(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G15(May),paste0("output/May.xlsx"))
write_xlsx(G15(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G15(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G15(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G15(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G15(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G15(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G15(Dic),paste0("output/Dic.xlsx"))

# G16

G16 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  bog$OCI[is.na(bog$OCI)] <- 0
  bog$P6220[is.na(bog$P6220)] <- 1
  bog$RAMA2D_R4 <- as.numeric(bog$RAMA2D_R4)
  
  ##################### Gráfica 14 ############################################
  
  bog$Sector <- character(length = dim(bog)[1])
  
  bog$Sector[bog$RAMA2D_R4 == 1 | bog$RAMA2D_R4 == 2] <- "Agricultura"
  bog$Sector[bog$RAMA2D_R4 == 5 | bog$RAMA2D_R4 == 6 | bog$RAMA2D_R4 == 7 |
               bog$RAMA2D_R4 == 8] <- "Explotación de minas"
  bog$Sector[bog$RAMA2D_R4 == 10 | bog$RAMA2D_R4 == 11 | bog$RAMA2D_R4 == 12 |
               bog$RAMA2D_R4 == 13 | bog$RAMA2D_R4 == 14 |bog$RAMA2D_R4 == 15 |
               bog$RAMA2D_R4 == 16 |bog$RAMA2D_R4 == 17 | bog$RAMA2D_R4 == 18 |
               bog$RAMA2D_R4 == 19 | bog$RAMA2D_R4 == 20 | bog$RAMA2D_R4 == 21 |
               bog$RAMA2D_R4 == 22 | bog$RAMA2D_R4 == 23 | bog$RAMA2D_R4 == 24 |
               bog$RAMA2D_R4 == 25 | bog$RAMA2D_R4 == 26 | bog$RAMA2D_R4 == 27 |
               bog$RAMA2D_R4 == 28 | bog$RAMA2D_R4 == 29 | bog$RAMA2D_R4 == 30 |
               bog$RAMA2D_R4 == 31 | bog$RAMA2D_R4 == 32 | bog$RAMA2D_R4 == 33] <- "Manufacturas"
  bog$Sector[bog$RAMA2D_R4 == 35 | bog$RAMA2D_R4 == 36 | bog$RAMA2D_R4 == 37 |
               bog$RAMA2D_R4 == 38 | bog$RAMA2D_R4 == 39] <- "Electricidad,  gas y agua"
  bog$Sector[bog$RAMA2D_R4 == 41 | bog$RAMA2D_R4 == 42 | bog$RAMA2D_R4 == 43 |
               bog$RAMA2D_R4 == 68] <- "Construcción e inmobiliarias"
  bog$Sector[bog$RAMA2D_R4 == 45 | bog$RAMA2D_R4 == 46 | bog$RAMA2D_R4 == 47] <- "Comercio"
  bog$Sector[bog$RAMA2D_R4 == 49| bog$RAMA2D_R4 == 50 | bog$RAMA2D_R4 == 51 |
               bog$RAMA2D_R4 == 52 | bog$RAMA2D_R4 == 53] <- "Transporte y almacenamiento"
  bog$Sector[bog$RAMA2D_R4 == 55 | bog$RAMA2D_R4 == 56] <- "Alojamiento y comida"
  bog$Sector[bog$RAMA2D_R4 == 58| bog$RAMA2D_R4 == 59 | bog$RAMA2D_R4 == 60 |
               bog$RAMA2D_R4 == 61 | bog$RAMA2D_R4 == 62 | bog$RAMA2D_R4 == 63] <- "Información y comunicaciones"
  bog$Sector[bog$RAMA2D_R4 == 64 | bog$RAMA2D_R4 == 65 | bog$RAMA2D_R4 == 66] <- "Financieras y seguros"
  bog$Sector[bog$RAMA2D_R4 == 69 | bog$RAMA2D_R4 == 70 | bog$RAMA2D_R4 == 71 |
               bog$RAMA2D_R4 == 72 | bog$RAMA2D_R4 == 73 |bog$RAMA2D_R4 == 74 |
               bog$RAMA2D_R4 == 75 |bog$RAMA2D_R4 == 77 | bog$RAMA2D_R4 == 78 |
               bog$RAMA2D_R4 == 79 | bog$RAMA2D_R4 == 80 | bog$RAMA2D_R4 == 81 |
               bog$RAMA2D_R4 == 82 ] <- "Actividades profesionales y de servicios"
  bog$Sector[bog$RAMA2D_R4 == 84] <- "Admon pública"
  bog$Sector[bog$RAMA2D_R4 == 85] <- "Educación"
  bog$Sector[bog$RAMA2D_R4 == 86 | bog$RAMA2D_R4 == 87 | bog$RAMA2D_R4 == 88] <- "Salud y asistencia social"
  bog$Sector[bog$RAMA2D_R4 == 90 | bog$RAMA2D_R4 == 91 | bog$RAMA2D_R4 == 92 |
               bog$RAMA2D_R4 == 93] <- "Artísticas, entretenimiento y recreación"
  bog$Sector[bog$RAMA2D_R4 == 94 | bog$RAMA2D_R4 == 95 | bog$RAMA2D_R4 == 96] <- "Otras actividades y servicios"
  bog$Sector[bog$RAMA2D_R4 == 97 | bog$RAMA2D_R4 == 98] <- "Actividades del hogar"
  bog$Sector[bog$Sector == ""] <- "otro"
  
  bog_H <- bog[bog$P6020 == 1,]
  bog_M <- bog[bog$P6020 == 2,]
  
  
  list_H_sec <- split(bog_H,bog_H$Sector)
  Ocu_H_sec <- sapply(list_H_sec, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 
  
  list_M_sec <- split(bog_M,bog_M$Sector) 
  Ocu_M_sec <- sapply(list_M_sec, function(x) {
    Ocu <- sum(x[x$PET == 1 & x$OCI == 1,]$fex_c_2011)
  }) 
  
  
  ##################### Vector Resultante ####################################
  
  names <-  c(names(Ocu_H_sec), names(Ocu_M_sec))
  values <- c(Ocu_H_sec, Ocu_M_sec)
  
  stats <- data.frame(names,values)
  
  stats
  
}


write_xlsx(G16(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G16(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G16(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G16(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G16(May),paste0("output/May.xlsx"))
write_xlsx(G16(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G16(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G16(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G16(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G16(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G16(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G16(Dic),paste0("output/Dic.xlsx"))


# G17


G17 <- function(A){
  
  # Preparación  
  bog <- A[A$AREA == 11, ]
  bog <- bog[bog$P6040 > 13,]
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$DSI[is.na(bog$DSI)] <- 0
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))
  bog$OCI[is.na(bog$OCI)] <- 0
  bog$P6220[is.na(bog$P6220)] <- 1
  bog$RAMA2D_R4 <- as.numeric(bog$RAMA2D_R4)
  
  ##################### Gráfica 16 ############################################
  
  bog$P6870 <- as.numeric(bog$P6870)
  bog$P6430 <- as.numeric(bog$P6430)
  bog$OFICIO <- as.numeric(bog$OFICIO)
  
  
  bog$Informalidad <- numeric(length = dim(bog)[1])
  
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 1] <- 1
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 6] <- 1
  bog$Informalidad[bog$P6430 == 7] <- 1
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 3] <- 1
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 8 ] <- 1
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 4 & bog$OFICIO > 20] <- 1
  bog$Informalidad[bog$P6870 <= 3 & bog$P6430 == 5 & bog$P6040 >= 15] <- 1

  
  Ocu <- sum(bog[bog$PET == 1 & bog$OCI == 1,]$fex_c_2011)
  inf <- sum(bog[bog$PET == 1 & bog$Informalidad == 1,]$fex_c_2011)
  TI <- inf/Ocu
  
  bog_H <- bog[bog$P6020 == 1,]
  bog_M <- bog[bog$P6020 == 2,]
  
  
  
  Ocu_H <- sum(bog_H[bog_H$PET == 1 & bog_H$OCI == 1,]$fex_c_2011)
  inf_H <- sum(bog_H[bog_H$PET == 1 & bog_H$Informalidad == 1,]$fex_c_2011)
  TI_H <- inf_H/Ocu_H
  
  Ocu_M <- sum(bog_M[bog_M$PET == 1 & bog_M$OCI == 1,]$fex_c_2011)
  inf_M <- sum(bog_M[bog_M$PET == 1 & bog_M$Informalidad == 1,]$fex_c_2011)
  TI_M <- inf_M/Ocu_M
  
  
  ##################### Vector Resultante ####################################
  
  names <-  c("Total", "Hombres", "Mujeres")
  values <- c(TI, TI_H, TI_M)
  stats <- data.frame(names,values)
  
  stats  
  
}


write_xlsx(G17(Ene),paste0("output/Ene.xlsx"))
write_xlsx(G17(Feb),paste0("output/Feb.xlsx"))
write_xlsx(G17(Mar),paste0("output/Mar.xlsx"))
write_xlsx(G17(Abr),paste0("output/Abr.xlsx"))
write_xlsx(G17(May),paste0("output/May.xlsx"))
write_xlsx(G17(Jun),paste0("output/Jun.xlsx"))
write_xlsx(G17(Jul),paste0("output/Jul.xlsx"))
write_xlsx(G17(Ago),paste0("output/Ago.xlsx"))
write_xlsx(G17(Sep),paste0("output/Sep.xlsx"))
write_xlsx(G17(Oct),paste0("output/Oct.xlsx"))
write_xlsx(G17(Nov),paste0("output/Nov.xlsx"))
write_xlsx(G17(Dic),paste0("output/Dic.xlsx"))









unique(Oct$RAMA2D_R4)

sort(unique(as.numeric(Ene$RAMA2D_R4)))
Ene$Sector <- character(length = dim(Ene)[1])

Ene$Sector[Ene$RAMA2D_R4 == 1 | Ene$RAMA2D_R4 == 2] <- "Agricultura"
Ene$Sector[Ene$RAMA2D_R4 == 5 | Ene$RAMA2D_R4 == 6 | Ene$RAMA2D_R4 == 7 |
             Ene$RAMA2D_R4 == 8] <- "Explotación de minas"
Ene$Sector[Ene$RAMA2D_R4 == 10 | Ene$RAMA2D_R4 == 11 | Ene$RAMA2D_R4 == 12 |
             Ene$RAMA2D_R4 == 13 | Ene$RAMA2D_R4 == 14 |Ene$RAMA2D_R4 == 15 |
             Ene$RAMA2D_R4 == 16 |Ene$RAMA2D_R4 == 17 | Ene$RAMA2D_R4 == 18 |
             Ene$RAMA2D_R4 == 19 | Ene$RAMA2D_R4 == 20 | Ene$RAMA2D_R4 == 21 |
             Ene$RAMA2D_R4 == 22 | Ene$RAMA2D_R4 == 23 | Ene$RAMA2D_R4 == 24 |
             Ene$RAMA2D_R4 == 25 | Ene$RAMA2D_R4 == 26 | Ene$RAMA2D_R4 == 27 |
             Ene$RAMA2D_R4 == 28 | Ene$RAMA2D_R4 == 29 | Ene$RAMA2D_R4 == 30 |
             Ene$RAMA2D_R4 == 31 | Ene$RAMA2D_R4 == 32 | Ene$RAMA2D_R4 == 33] <- "Manufacturas"
Ene$Sector[Ene$RAMA2D_R4 == 35| Ene$RAMA2D_R4 == 36 | Ene$RAMA2D_R4 == 37 |
             Ene$RAMA2D_R4 == 38 | Ene$RAMA2D_R4 == 39] <- "Electricidad,  gas y agua"
Ene$Sector[Ene$RAMA2D_R4 == 41 | Ene$RAMA2D_R4 == 42 | Ene$RAMA2D_R4 == 43 |
             Ene$RAMA2D_R4 == 68] <- "Construcción e inmobiliarias de minas"
Ene$Sector[Ene$RAMA2D_R4 == 45 | Ene$RAMA2D_R4 == 46 | Ene$RAMA2D_R4 == 47] <- "Comercio"
Ene$Sector[Ene$RAMA2D_R4 == 49, Ene$RAMA2D_R4 == 50 | Ene$RAMA2D_R4 == 51 |
             Ene$RAMA2D_R4 == 52 | Ene$RAMA2D_R4 == 53] <- "Transporte y almacenamiento"
Ene$Sector[Ene$RAMA2D_R4 == 55 | Ene$RAMA2D_R4 == 56] <- "Alojamiento y comida"
Ene$Sector[Ene$RAMA2D_R4 == 58, Ene$RAMA2D_R4 == 59 | Ene$RAMA2D_R4 == 60 |
            Ene$RAMA2D_R4 == 61 | Ene$RAMA2D_R4 == 62 | Ene$RAMA2D_R4 == 63] <- "Información y comunicaciones"
Ene$Sector[Ene$RAMA2D_R4 == 64 | Ene$RAMA2D_R4 == 65 | Ene$RAMA2D_R4 == 66] <- "Financieras y seguros"
Ene$Sector[Ene$RAMA2D_R4 == 69 | Ene$RAMA2D_R4 == 70 | Ene$RAMA2D_R4 == 71 |
             Ene$RAMA2D_R4 == 72 | Ene$RAMA2D_R4 == 73 |Ene$RAMA2D_R4 == 74 |
             Ene$RAMA2D_R4 == 75 |Ene$RAMA2D_R4 == 77 | Ene$RAMA2D_R4 == 78 |
             Ene$RAMA2D_R4 == 79 | Ene$RAMA2D_R4 == 80 | Ene$RAMA2D_R4 == 81 |
             Ene$RAMA2D_R4 == 82 ] <- "Actividades profesionales y de servicios"
Ene$Sector[Ene$RAMA2D_R4 == 84] <- "Admon pública"
Ene$Sector[Ene$RAMA2D_R4 == 85] <- "Educación"
Ene$Sector[Ene$RAMA2D_R4 == 86 | Ene$RAMA2D_R4 == 87 | Ene$RAMA2D_R4 == 88] <- "Salud y asistencia social"
Ene$Sector[Ene$RAMA2D_R4 == 90 | Ene$RAMA2D_R4 == 91 | Ene$RAMA2D_R4 == 92 |
             Ene$RAMA2D_R4 == 93] <- "Actividades profesionales y de servicios"
Ene$Sector[Ene$RAMA2D_R4 == 94 | Ene$RAMA2D_R4 == 95 | Ene$RAMA2D_R4 == 96] <- "Otras actividades y servicios"
Ene$Sector[Ene$RAMA2D_R4 == 97 | Ene$RAMA2D_R4 == 98] <- "Actividades del hogar"
           


Ene$Sector[Ene$Sector == ""] <- "otro"


table(Ene$Sector)

library("spastat")
Ene$OCI
weighted.mean(Ene$INGLABO, Ene$fex_c_2011, na.rm =TRUE)
median(Ene$INGLABO, na.rm = TRUE)
