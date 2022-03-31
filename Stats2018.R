
# Stats 

# Variables 

base$FT <- ifelse(base$OCI == 1 | base$DSI ==1 ,1,0)


# dividir bases

# Bogotá
bogota <- base[base$AREA == 11 & complete.cases(base$AREA),]

unique(base$AREA)
#13 ÁREAS

#
unique(caracteristicas$AREA)
unique(hogarVivie$AREA)
unique(ft$AREA)
unique(noOcu$AREA)
unique(ocu$AREA)
unique(otrasFor$AREA)
unique(OtrosIng$AREA)
unique(tipo$AREA)
#

area13 <- base[complete.cases(base$AREA),]
unique(area13$AREA)
# Bogotá


## ocupación  
PET_BOG <- sum(bogota[complete.cases(bogota$PET) ,]$FEX_C18)
Ocupados_BOG <- sum(bogota[complete.cases(bogota$OCI),]$FEX_C18)
TO_Bog <- Ocupados_BOG/PET_BOG*100

## desempleo   
FT_BOG <- sum(bogota[complete.cases(bogota$FT),]$FEX_C18)
Desempleados_BOG <- sum(bogota[complete.cases(bogota$DSI),]$FEX_C18)
TD_BOG <- Desempleados_BOG/FT_BOG*100

#Desempleo por sexo
bogota$P3039

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


#Informalidad
bogota$Informalidad <- rep(0, nrow(bogota))
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P6870 <= 3 & bogota$P6430 == 4] <- 1
bogota$Informalidad[bogota$P6870 <= 3 & bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P6430 == 7] <- 1

bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P6430 == 4] <- 1
bogota$Informalidad[bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P6430 == 7] <- 1


Informales_BOG <- sum(bogota[bogota$Informalidad == 1,]$FEX_C18)
TI_BOG <- Informales_BOG/Ocupados_BOG*100


# 13 áreas


## ocupación  
PET_13A <- sum(base[complete.cases(base$PET) ,]$FEX_C18)

PET_13A <- sum(area13[complete.cases(area13$PET) ,]$FEX_C18)
Ocupados_13A <- sum(area13[complete.cases(area13$OCI),]$FEX_C18)
TO_13A <- Ocupados_13A/PET_13A*100

## desempleo   
FT_BOG <- sum(bogota[complete.cases(bogota$FT),]$FEX_C18)
Desempleados_BOG <- sum(bogota[complete.cases(bogota$DSI),]$FEX_C18)
TD_BOG <- Desempleados_BOG/FT_BOG*100

#Desempleo por sexo
bogota$P3039

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


#Informalidad
bogota$Informalidad <- rep(0, nrow(bogota))
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P6870 <= 3 & bogota$P6430 == 4] <- 1
bogota$Informalidad[bogota$P6870 <= 3 & bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P6430 == 7] <- 1

bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P6430 == 4] <- 1
bogota$Informalidad[bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P6430 == 7] <- 1


Informales_BOG <- sum(bogota[bogota$Informalidad == 1,]$FEX_C18)
TI_BOG <- Informales_BOG/Ocupados_BOG*100

# regreso a presencialidad - ver en pulso social y anexarlo a tasa de ocupación por sexo 
# aumento de TO femenina 


# Y por sectores? 