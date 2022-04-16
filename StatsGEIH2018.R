StatsGEIH2018 <- function(base) {
  
  base$FT <- ifelse(base$OCI == 1 | base$DSI ==1 ,1,0)
  bogota <- base[base$AREA == "11" & complete.cases(base$AREA),]
  area13 <- base[base$AREA %in% c("11" ,"05", "76", "08", "68", 
                                  "17", "66", "54", "52", "73", "23",
                                  "13", "50", "5", "8"),]
  
  # Creación de matrices necesarias y de FT
  
  
  ################ Bogotá ##############################################
  
  # TGP
  FT_BOG <- sum(bogota[complete.cases(bogota$FT) & bogota$FT == 1 ,]$FEX_C18)
  PET_BOG <- sum(bogota[complete.cases(bogota$PET) ,]$FEX_C18)
  TGP_BOG <- FT_BOG/PET_BOG*100
  
  # Población Inactiva
  PFFT_BOG <- PET_BOG - FT_BOG
  
  # ocupación  
  Ocupados_BOG <- sum(bogota[complete.cases(bogota$OCI),]$FEX_C18)
  TO_Bog <- Ocupados_BOG/PET_BOG*100
  
  ## Desempleo   
  FT_BOG <- sum(bogota[complete.cases(bogota$FT),]$FEX_C18)
  Desempleados_BOG <- sum(bogota[complete.cases(bogota$DSI),]$FEX_C18)
  TD_BOG <- Desempleados_BOG/FT_BOG*100
  
  #Desempleo por sexo
  FTH_BOG <- sum(bogota[complete.cases(bogota$P3039) & bogota$P3039 == 1 & complete.cases(bogota$FT),]$FEX_C18)
  DesempleadosH_BOG <- sum(bogota[complete.cases(bogota$P3039) & bogota$P3039 == 1 & complete.cases(bogota$DSI),]$FEX_C18)
  TDH_BOG <- DesempleadosH_BOG/FTH_BOG*100
  
  FTM_BOG <- sum(bogota[complete.cases(bogota$P3039) & bogota$P3039 == 2 & complete.cases(bogota$FT),]$FEX_C18)
  DesempleadosM_BOG <- sum(bogota[complete.cases(bogota$P3039) & bogota$P3039 == 2 & complete.cases(bogota$DSI),]$FEX_C18)
  TDM_BOG <- DesempleadosH_BOG/FTM_BOG*100
  
  #Desempleo por edad
  bogota$age <- cut(bogota$P6040, breaks = c(12,28,39,49, 59, 130))
  bogAge <- bogota[complete.cases(bogota$age),]
  list <- split(bogAge, bogAge$age) 
  
  TDA_BOG <- sapply(list, function(x) {
    FT <- sum(x[complete.cases(x$FT),]$FEX_C18)
    Desempleados <- sum(x[complete.cases(x$DSI),]$FEX_C18)
    TD <- Desempleados/FT*100
  })  
  
  TDA_BOG <- unname(TDA_BOG)

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
  
  Bogota <- c(PET_BOG,FT_BOG,PFFT_BOG, Ocupados_BOG,Desempleados_BOG,TGP_BOG,
              TO_Bog,TD_BOG, TDH_BOG, TDM_BOG, TDA_BOG)
  
  ################ 13 áreas  ##############################################
  
  # Participación 
  FT_13A <- sum(area13[complete.cases(area13$FT) & area13$FT == 1 ,]$FEX_C18)
  PET_13A <- sum(area13[complete.cases(area13$PET) ,]$FEX_C18)
  TGP_13A <- FT_13A/PET_13A*100
  
  # Inacivos
  PFFT_13A <- PET_13A - FT_13A
  PFFT_13A
  
  # ocupación  
  Ocupados_13A <- sum(area13[complete.cases(area13$OCI),]$FEX_C18)
  TO_13A <- Ocupados_13A/PET_13A*100
  
  # Desempleo   
  FT_13A <- sum(area13[complete.cases(area13$FT),]$FEX_C18)
  Desempleados_13A <- sum(area13[complete.cases(area13$DSI),]$FEX_C18)
  TD_13A <- Desempleados_13A/FT_13A*100
  
  # Desempleo por sexo
  FTH_13A <- sum(area13[complete.cases(area13$P3039) & area13$P3039 == 1 & complete.cases(area13$FT),]$FEX_C18)
  DesempleadosH_13A <- sum(area13[complete.cases(area13$P3039) & area13$P3039 == 1 & complete.cases(area13$DSI),]$FEX_C18)
  TDH_13A <- DesempleadosH_13A/FTH_13A*100
  
  FTM_13A <- sum(area13[complete.cases(area13$P3039) & area13$P3039 == 2 & complete.cases(area13$FT),]$FEX_C18)
  DesempleadosM_13A <- sum(area13[complete.cases(area13$P3039) & area13$P3039 == 2 & complete.cases(area13$DSI),]$FEX_C18)
  TDM_13A <- DesempleadosM_13A/FTM_13A*100
  
  #Desempleo por edad
  area13$age <- cut(area13$P6040, breaks = c(12,28,39,49, 59, 130))
  area13Age <- area13[complete.cases(area13$age),]
  list <- split(area13Age, area13Age$age) 
  
  TDA_13A <- sapply(list, function(x) {
    FT <- sum(x[complete.cases(x$FT),]$FEX_C18)
    Desempleados <- sum(x[complete.cases(x$DSI),]$FEX_C18)
    TD <- Desempleados/FT*100
  })  
  
  TDA_13A <- unname(TDA_13A)
  
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
  
  Areas13 <- c(PET_13A,FT_13A,PFFT_13A, Ocupados_13A,Desempleados_13A,TGP_13A,
              TO_13A,TD_13A, TDH_13A, TDM_13A, TDA_13A)
  
  ################ Colombia ##############################################
  
  # Participación 
  FT_COL <- sum(base[complete.cases(base$FT) & base$FT == 1 ,]$FEX_C18)
  PET_COL <- sum(base[complete.cases(base$PET) ,]$FEX_C18)
  TGP_COL <- FT_COL/PET_COL*100
  
  # Inacivos
  PFFT_COL <- PET_COL - FT_COL
  
  # Ocupación  
  Ocupados_COL <- sum(base[complete.cases(base$OCI),]$FEX_C18)
  TO_COL <- Ocupados_COL/PET_COL*100
  
  ## Desempleo   
  FT_COL <- sum(base[complete.cases(base$FT),]$FEX_C18)
  Desempleados_COL <- sum(base[complete.cases(base$DSI),]$FEX_C18)
  TD_COL <- Desempleados_COL/FT_COL*100
  
  #Desempleo por sexo
  FTH_COL <- sum(base[complete.cases(base$P3039) & base$P3039 == 1 & complete.cases(base$FT),]$FEX_C18)
  DesempleadosH_COL <- sum(base[complete.cases(base$P3039) & base$P3039 == 1 & complete.cases(base$DSI),]$FEX_C18)
  TDH_COL <- DesempleadosH_COL/FTH_COL*100
  
  FTM_COL <- sum(base[complete.cases(base$P3039) & base$P3039 == 2 & complete.cases(base$FT),]$FEX_C18)
  DesempleadosM_COL <- sum(base[complete.cases(base$P3039) & base$P3039 == 2 & complete.cases(base$DSI),]$FEX_C18)
  TDM_COL <- DesempleadosM_COL/FTM_COL*100
  
  #Desempleo por edad
  
  base$age <- cut(base$P6040, breaks = c(12,28,39,49, 59, 130))
  baseAge <- base[complete.cases(base$age),]
  list <- split(baseAge, baseAge$age) 
  
  TDA_COL <- sapply(list, function(x) {
    FT <- sum(x[complete.cases(x$FT),]$FEX_C18)
    Desempleados <- sum(x[complete.cases(x$DSI),]$FEX_C18)
    TD <- Desempleados/FT*100
  })  
  
  TDA_COL <- unname(TDA_COL)
  
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

n <-
library("writexl")
write_xlsx(d,"d.xlsx")




