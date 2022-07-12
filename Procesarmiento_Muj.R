install.packages("writexl")
library(writexl)

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


# G9
