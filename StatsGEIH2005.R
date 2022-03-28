StatsGEIH2005 <- function(area, cabecera,  rural){
  
  # Transformación base de área
  area$PET <- ifelse(area$P6040 >= 15,1,0)
  area$PEA <- ifelse(area$OCI == 1 | area$DSI ==1 ,1,0)
  if(is.character(area$FEX_C18)){
    area$FEX_C18 <- sub(",",".",area$FEX_C18)
    area$FEX_C18 <-as.numeric(area$FEX_C18)
  }
  
  ### Bogotá Stats ###
  
  #Ocupación 
  
  PET_BOG <- sum(area[area$PET ==1 & area$AREA == 11 ,]$FEX_C18)
  Ocupados_BOG <- sum(area[area$AREA == 11 & complete.cases(area$OCI) & area$P6040 >= 15,]$FEX_C18)
  TO_Bog <- Ocupados_BOG/PET_BOG*100
  
  #Desempleo 
  
  PEA_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6040 >= 15 ,]$FEX_C18)
  Desempleados_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI)  & area$P6040 >= 15,]$FEX_C18)
  TD_BOG <- Desempleados_BOG/PEA_BOG*100
  
  #Desempleo por sexo
  
  PEAH_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosH_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI) & area$P6020 == 1 & area$P6040 >= 15,]$FEX_C18)
  TDH_BOG <- DesempleadosH_BOG/PEAH_BOG*100
  
  PEAM_BOG <- sum(area[complete.cases(area$PEA) & area$AREA == 11 & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  DesempleadosM_BOG <- sum(area[area$AREA == 11 & complete.cases(area$DSI) & area$P6020 == 2 & area$P6040 >= 15,]$FEX_C18)
  TDM_BOG <- DesempleadosM_BOG/PEAM_BOG*100
  
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
  
  
  area$Informalidad <- rep(0, nrow(area))
  
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 1 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 6 & area$P6040 >= 15] <- 1
  area$Informalidad[A$P6870 <= 3 & area$P6430 == 3 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 8 & area$P6040 >= 15] <- 1
  area$Informalidad[A$P6870 <= 3 & area$P6430 == 4 & area$OFICIO>20 & complete.cases(area$OFICIO) & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6870 <= 3 & area$P6430 == 5 & area$P6040 >= 15] <- 1
  area$Informalidad[area$P6430 == 7 & area$P6040 >= 15] <- 1
  
  # Bogotá
  Informales_BOG <- sum(area[area$AREA == 11 & area$Informalidad == 1 & area$P6040 >= 15,]$FEX_C18)
  TI_BOG <- Informales_BOG/Ocupados_BOG*100
  
  
  
  PET_BOG <- as.character(PET_BOG)
  PEA_BOG <- as.character(PEA_BOG)
  Ocupados_BOG <- as.character(Ocupados_BOG)
  Desempleados_BOG <- as.character(Desempleados_BOG)
  TO_Bog <- as.character(TO_Bog)
  TD_BOG <- as.character(TD_BOG)
  TDH_BOG <- as.character(TDH_BOG)
  TDM_BOG <- as.character(TDM_BOG)
  TDA_BOG <- as.character(TDA_BOG)
  Informales_BOG <- as.character(Informales_BOG)
  TI_BOG <- as.character(TI_BOG)
    
  Bogotá <- c(PET_BOG,PEA_BOG,Ocupados_BOG,Desempleados_BOG,TO_Bog,TD_BOG, 
              TDH_BOG, TDM_BOG, TDA_BOG, Informales_BOG, TI_BOG)
  Bogotá
  
  
  
  
}

b <- StatsGEIH2005(A,C,R)

b
x<-3
as.character(b,x)

