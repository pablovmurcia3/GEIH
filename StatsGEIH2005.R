StatsGEIH2005 <- function(area, cabecera,  rural){
  
  # Transformación base de área
  area$PET <- ifelse(area$P6040 >= 15,1,0)
  area$PEA <- ifelse(area$OCI == 1 | area$DSI ==1 ,1,0)
  if(is.character(area$FEX_C18)){
    area$FEX_C18 <- sub(",",".",area$FEX_C18)
    area$FEX_C18 <-as.numeric(area$FEX_C18)
  }
  
  ################ Bogotá ##############################################
  
  # TGP
  FT_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6040 >= 15 ,]$FEX_C18)
  PET_BOG <- sum(area[area$PET ==1 & area$AREA == 11 ,]$FEX_C18)
  TGP_BOG <- FT_BOG/PET_BOG*100
  
  # Población Inactiva
  PFFT_BOG <- PET_BOG - FT_BOG
  
  #Ocupación 
  Ocupados_BOG <- sum(area[area$AREA == 11 & complete.cases(area$OCI) & area$P6040 >= 15,]$FEX_C18)
  TO_Bog <- Ocupados_BOG/PET_BOG*100
  
  #Desempleo 
  
  FT_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6040 >= 15 ,]$FEX_C18)
  Desempleados_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI)  & area$P6040 >= 15,]$FEX_C18)
  TD_BOG <- Desempleados_BOG/FT_BOG*100
  
  #Desempleo por sexo
  
  FTH_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosH_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI) & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  TDH_BOG <- DesempleadosH_BOG/FTH_BOG*100
  
  FTM_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosM_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI) & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  TDM_BOG <- DesempleadosM_BOG/FTM_BOG*100
  
  #Desempleo por edad
  
  area$age <- cut(area$P6040, breaks = c(12,28,39,49, 59, 130))
  Aage <- area[complete.cases(area$age),]
  list <- split(Aage, Aage$age) 
  
  TDA_BOG <- sapply(list, function(x) {
                PEA <- sum(x[complete.cases(x$PEA) & x$AREA == 11 & x$P6040 >= 15 ,]$FEX_C18)
                Desempleados <- sum(x[x$AREA == 11 & complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
                TD <- Desempleados/PEA*100
              })  
            
  TDA_BOG <- unname(TDA_BOG)
  
  #Informalidad
  area$Informalidad <- rep(0, nrow(area))
  
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 1 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 6 & area$P6040 >= 15] <- 1
  area$Informalidad[A$P6870 <= 3 & area$P6430 == 3 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 8 & area$P6040 >= 15] <- 1
  area$Informalidad[A$P6870 <= 3 & area$P6430 == 4 & area$OFICIO>20 & complete.cases(area$OFICIO) & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 5 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6430 == 7 & area$P6040 >= 15] <- 1
  
  Informales_BOG <- sum(area[area$AREA == 11 & area$Informalidad == 1 & area$P6040 >= 15,]$FEX_C18)
  TI_BOG <- Informales_BOG/Ocupados_BOG*100
  
  PET_BOG <- as.character(PET_BOG)
  FT_BOG <- as.character(FT_BOG)
  PFFT_BOG <- as.character(PFFT_BOG)
  Ocupados_BOG <- as.character(Ocupados_BOG)
  Desempleados_BOG <- as.character(Desempleados_BOG)
  TGP_BOG <- as.character(TGP_BOG)
  TO_Bog <- as.character(TO_Bog)
  TD_BOG <- as.character(TD_BOG)
  TDH_BOG <- as.character(TDH_BOG)
  TDM_BOG <- as.character(TDM_BOG)
  TDA_BOG <- as.character(TDA_BOG)
  Informales_BOG <- as.character(Informales_BOG)
  TI_BOG <- as.character(TI_BOG)
    
  Bogota <- c(PET_BOG,FT_BOG,PFFT_BOG, Ocupados_BOG,Desempleados_BOG,TGP_BOG,
              TO_Bog,TD_BOG, TDH_BOG, TDM_BOG, TDA_BOG)
  
  ################ 13 áreas  ##############################################
  
  # TGP
  FT_13A <- sum(area[complete.cases(area$PEA) & area$P6040 >= 15 ,]$FEX_C18)
  PET_13A <- sum(area[area$PET ==1,]$FEX_C18)
  TGP_13A <- FT_13A/PET_13A*100
  
  # Población Inactiva
  PFFT_13A <- PET_13A - FT_13A
  
  #Ocupación 
  Ocupados_13A <- sum(area[complete.cases(area$OCI) & area$P6040 >= 15,]$FEX_C18)
  TO_13A <- Ocupados_13A/PET_13A*100
  
  #Desempleo 
  
  FT_13A <- sum(area[complete.cases(area$PEA) & area$P6040 >= 15 ,]$FEX_C18)
  Desempleados_13A <- sum(area[complete.cases(area$DSI)  & area$P6040 >= 15,]$FEX_C18)
  TD_13A <- Desempleados_13A/FT_13A*100
  
  #Desempleo por sexo
  
  FTH_13A <- sum(area[complete.cases(area$PEA) & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosH_13A <- sum(area[complete.cases(area$DSI) & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  TDH_13A <- DesempleadosH_13A/FTH_13A*100
  
  FTM_13A <- sum(area[complete.cases(area$PEA) & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosM_13A <- sum(area[complete.cases(area$DSI) & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  TDM_13A <- DesempleadosM_13A/FTM_13A*100
  
  #Desempleo por edad

  TDA_13A <- sapply(list, function(x) {
    PEA <- sum(x[complete.cases(x$PEA) & x$P6040 >= 15 ,]$FEX_C18)
    Desempleados <- sum(x[complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
    TD <- Desempleados/PEA*100
  })  
  
  TDA_13A <- unname(TDA_13A)
  
  Informales_13A <- sum(area[area$Informalidad == 1 & area$P6040 >= 15,]$FEX_C18)
  TI_13A <- Informales_13A/Ocupados_13A*100
  
  PET_13A <- as.character(PET_13A)
  FT_13A <- as.character(FT_13A)
  PFFT_13A <- as.character(PFFT_13A)
  Ocupados_13A <- as.character(Ocupados_13A)
  Desempleados_13A <- as.character(Desempleados_13A)
  TGP_13A <- as.character(TGP_13A)
  TO_13A <- as.character(TO_13A)
  TD_13A <- as.character(TD_13A)
  TDH_13A <- as.character(TDH_13A)
  TDM_13A <- as.character(TDM_13A)
  TDA_13A <- as.character(TDA_13A)
  Informales_13A <- as.character(Informales_13A)
  TI_13A <- as.character(TI_13A)
  
  Areas13 <- c(PET_13A,FT_13A,PFFT_13A, Ocupados_13A,Desempleados_13A,TGP_13A,
               TO_13A,TD_13A, TDH_13A, TDM_13A, TDA_13A)
  
  ################ Colombia ##############################################
  
  cabecera <- cabecera[, names(cabecera) %in% names(rural)]
  rural <- rural[, names(rural) %in% names(cabecera)]

  cabecera <- cabecera[ , order(names(cabecera))]
  rural <- rural[ , order(names(rural))]
  
  colombia <- rbind(cabecera,rural)
  
  # Transformación
  
  colombia$PET <- ifelse(colombia$P6040 >= 15,1,0)
  colombia$PEA <- ifelse(colombia$OCI == 1 | colombia$DSI ==1 ,1,0)
  if(is.character(colombia$FEX_C18)){
    colombia$FEX_C18 <- sub(",",".",colombia$FEX_C18)
    colombia$FEX_C18 <-as.numeric(colombia$FEX_C18)
  }
  
  # TGP
  FT_COL<- sum(colombia[complete.cases(colombia$PEA)  & colombia$P6040 >= 15 ,]$FEX_C18)
  PET_COL <- sum(colombia[colombia$PET ==1 ,]$FEX_C18)
  TGP_COL <- FT_COL/PET_COL*100
  
  # Población Inactiva
  PFFT_COL <- PET_COL - FT_COL
  
  ## Ocupación 
  
  PET_COL <- sum(colombia[colombia$PET ==1 ,]$FEX_C18)
  Ocupados_COL <- sum(colombia[complete.cases(colombia$OCI) & colombia$P6040 >= 15,]$FEX_C18)
  TO_COL <- Ocupados_COL/PET_COL*100
  
  # Desempleo 
  
  PEA_COL <- sum(colombia[complete.cases(colombia$PEA)  & colombia$P6040 >= 15 ,]$FEX_C18)
  Desempleados_COL <- sum(colombia[complete.cases(colombia$DSI) & colombia$P6040 >= 15,]$FEX_C18)
  TD_COL <- Desempleados_COL/PEA_COL*100
  
  # Desempleo por sexo 
  
  PEAH_COL <- sum(colombia[complete.cases(colombia$PEA) & colombia$P6020 == 1 & colombia$P6040 >= 15,]$FEX_C18)
  DesempleadosH_COL <- sum(colombia[complete.cases(colombia$DSI) & colombia$P6020 == 1 & colombia$P6040 >= 15,]$FEX_C18)
  TDH_COL <- DesempleadosH_COL/PEAH_COL*100
  
  PEAM_COL <- sum(colombia[complete.cases(colombia$PEA) & colombia$P6020 == 2 & colombia$P6040 >= 15,]$FEX_C18)
  DesempleadosM_COL <- sum(colombia[complete.cases(colombia$DSI) & colombia$P6020 == 2 & colombia$P6040 >= 15,]$FEX_C18)
  TDM_COL <- DesempleadosM_COL/PEAM_COL*100
  
  # Desempleo por edad 
  
  colombia$age <- cut(colombia$P6040, breaks = c(12,28,39,49, 59, 130))
  colombiaAge <- colombia[complete.cases(colombia$age),]
  
  list <- split(colombiaAge, colombiaAge$age) 
  TDA_COL <- sapply(list, function(x) {
    PEA <- sum(x[complete.cases(x$PEA) & x$P6040 >= 15 ,]$FEX_C18)
    Desempleados <- sum(x[complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
    TD <- Desempleados/PEA*100
  })   
  
  TDA_COL <- unname(TDA_COL)
  
  # Informalidad
  
  colombia$Informalidad <- rep(0, nrow(colombia))
  
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 1 & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 6 & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 3 & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 8 & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 4 & colombia$OFICIO>20 & complete.cases(colombia$OFICIO) & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6870 <= 3 & colombia$P6430 == 5 & colombia$P6040 >= 15] <- 1
  colombia$Informalidad[colombia$P6430 == 7 & colombia$P6040 >= 15] <- 1
  
  Informales_COL <- sum(colombia[colombia$Informalidad == 1 & colombia$P6040 >= 15,]$FEX_C18)
  TI_COL <- Informales_COL/Ocupados_COL*100
  
  PET_COL <- as.character(PET_COL)
  FT_COL <- as.character(FT_COL)
  PFFT_COL <- as.character(PFFT_COL)
  Ocupados_COL <- as.character(Ocupados_COL)
  Desempleados_COL <- as.character(Desempleados_COL)
  TGP_COL <- as.character(TGP_COL)
  TO_COL <- as.character(TO_COL)
  TD_COL <- as.character(TD_COL)
  TDH_COL <- as.character(TDH_COL)
  TDM_COL <- as.character(TDM_COL)
  TDA_COL <- as.character(TDA_COL)
  Informales_COL <- as.character(Informales_COL)
  TI_COL <- as.character(TI_COL)
  
  Colombia <- c(PET_COL,FT_COL,PFFT_COL, Ocupados_COL,Desempleados_COL,TGP_COL,
                TO_COL,TD_COL, TDH_COL, TDM_COL, TDA_COL)
  
  
  names <- c("PET", "FT","PFFT", "Ocupados", "Desocupados", "TGP", "TO", "TD",
             "TD hombres", "TD mujeres", "TD < 29", "29 < TD < 39","40 < TD < 49",
             "50 < TD < 59","TD > 60")
  
  stats <- data.frame(names,Bogota, Areas13, Colombia)
  stats$Bogota <- as.numeric(stats$Bogota)
  stats$Areas13 <- as.numeric(stats$Areas13)
  stats$Colombia <- as.numeric(stats$Colombia)
  stats
  
}


o <- StatsGEIH2005(A,C,R)

library("writexl")
write_xlsx(o,"o.xlsx")






### Bogotá Stats ###

# TGP

A$PET <- ifelse(A$P6040 >= 15,1,0)
A$PEA <- ifelse(A$OCI == 1 | A$DSI ==1 ,1,0)

FT_BOG <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6040 >= 15 ,]$FEX_C18)
PET_BOG <- sum(A[A$PET ==1 & A$AREA == 11 ,]$FEX_C18)
TGP_BOG <- FT_BOG/PET_BOG*100

# Población Inactiva
PFFT_BOG <- PET_BOG - FT_BOG


