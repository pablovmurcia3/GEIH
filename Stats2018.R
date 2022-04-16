
# Stats 

# Variables 

base$FT <- ifelse(base$OCI == 1 | base$DSI ==1 ,1,0)


# dividir bases

# Bogotá
bogota <- base[base$AREA == "11" & complete.cases(base$AREA),]

unique(bogota$AREA)


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

## Particpación  


## Participación 

FT_BOG <- sum(bogota[complete.cases(bogota$FT) & bogota$FT == 1 ,]$FEX_C18)
PET_BOG <- sum(bogota[complete.cases(bogota$PET) ,]$FEX_C18)
TGP_BOG <- FT_BOG/PET_BOG*100

##Inacivos

PFFT_BOG <- PET_BOG - FT_BOG
PFFT_BOG

## ocupación  
PET_BOG <- sum(bogota[complete.cases(bogota$PET) ,]$FEX_C18)
Ocupados_BOG <- sum(bogota[complete.cases(bogota$OCI),]$FEX_C18)
TO_Bog <- Ocupados_BOG/PET_BOG*100

## desempleo   
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


#Informalidad
bogota$Informalidad <- rep(0, nrow(bogota))
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 1] <- 1
bogota$Informalidad[bogota$P3069 <= 3 &  bogota$P6430 == 6] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 3] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 8] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 4 
                  & bogota$P3042 != 8 & bogota$P3042 != 10
                  & bogota$P3042 != 11 & bogota$P3042 != 12
                  & bogota$P3042 != 13] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 5] <- 1
bogota$Informalidad[bogota$P3069 <= 3 & bogota$P6430 == 7] <- 1

Informales_BOG <- sum(bogota[bogota$Informalidad == 1,]$FEX_C18)
TI_BOG <- Informales_BOG/Ocupados_BOG*100
# 13 áreas
unique(base$AREA)



# Colombia

base$FT <- ifelse(base$OCI == 1 | base$DSI ==1 ,1,0)
unique(base$AREA)




## Participación 

FT_COL <- sum(base[complete.cases(base$FT) & base$FT == 1 ,]$FEX_C18)
PET_COL <- sum(base[complete.cases(base$PET) ,]$FEX_C18)
TGP_COL <- FT_COL/PET_COL*100


##Inacivos

PFFT_COL <- PET_COL - FT_COL
PFFT_COL
## ocupación  

Ocupados_COL <- sum(base[complete.cases(base$OCI),]$FEX_C18)
TO_COL <- Ocupados_COL/PET_COL*100
TO_COL

## desempleo   
FT_COL <- sum(base[complete.cases(base$FT),]$FEX_C18)
Desempleados_COL <- sum(base[complete.cases(base$DSI),]$FEX_C18)
TD_COL <- Desempleados_COL/FT_COL*100
TD_COL
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

# Informalidad

base$Informalidad <- rep(0, nrow(base))
base$Informalidad[base$P3069 <= 3 & base$P6430 == 1] <- 1
base$Informalidad[base$P3069 <= 3 &  base$P6430 == 6] <- 1
base$Informalidad[base$P3069 <= 3 & base$P6430 == 3] <- 1
base$Informalidad[base$P3069 <= 3 & base$P6430 == 8] <- 1
base$Informalidad[base$P3069 <= 3 & base$P6430 == 4 
                  & base$P3042 != 8 & base$P3042 != 10
                  & base$P3042 != 11 & base$P3042 != 12
                  & base$P3042 != 13] <- 1
base$Informalidad[base$P3069 <= 3 & base$P6430 == 5] <- 1
base$Informalidad[base$P3069 <= 3 & base$P6430 == 7] <- 1

Informales_COL <- sum(base[base$Informalidad == 1,]$FEX_C18)
TI_COL <- Informales_COL/Ocupados_COL*100


# #13 ÁREAS
unique((base$AREA))

area13 <- base[base$AREA %in% c("11" ,"05", "76", "08", "68", "17", "66", "54", "52", "73", "23", "13", "50", "5", "8"),]
}
## Participación 

FT_13A <- sum(area13[complete.cases(area13$FT) & area13$FT == 1 ,]$FEX_C18)
PET_13A <- sum(area13[complete.cases(area13$PET) ,]$FEX_C18)
TGP_13A <- FT_13A/PET_13A*100

##Inacivos

PFFT_13A <- PET_13A - FT_13A
PFFT_13A

## ocupación  

PET_13A <- sum(area13[complete.cases(area13$PET) ,]$FEX_C18)
Ocupados_13A <- sum(area13[complete.cases(area13$OCI),]$FEX_C18)
TO_13A <- Ocupados_13A/PET_13A*100
TO_13A
## desempleo   
FT_13A <- sum(area13[complete.cases(area13$FT),]$FEX_C18)
Desempleados_13A <- sum(area13[complete.cases(area13$DSI),]$FEX_C18)
TD_13A <- Desempleados_13A/FT_13A*100

#Desempleo por sexo

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

area13

TDA_13A <- sapply(list, function(x) {
  FT <- sum(x[complete.cases(x$FT),]$FEX_C18)
  Desempleados <- sum(x[complete.cases(x$DSI),]$FEX_C18)
  TD <- Desempleados/FT*100
})  


# Informalidad
area13$Informalidad <- rep(0, nrow(area13))
#1
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 1] <- 1
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 5] <- 1
#2 
area13$Informalidad[area13$P3069 <= 3 &  area13$P6430 == 6] <- 1
#3  
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 3] <- 1
#4
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 8] <- 1
#5
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 4 
                  & area13$P3042 != 8 & area13$P3042 != 10
                  & area13$P3042 != 11 & area13$P3042 != 12
                  & area13$P3042 != 13] <- 1
#6
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 5] <- 1

#7
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 7] <- 1

Informales_13A <- sum(area13[area13$Informalidad == 1,]$FEX_C18)
TI_13A <- Informales_13A/Ocupados_13A*100



area13$Informalidad <- rep(0, nrow(area13))
table(area13$Informalidad)
#1Los empleados particulares y los obreros que laboran en establecimientos,
#negocios o empresas que ocupen hasta cinco personas en todas sus agencias y
#sucursales, incluyendo al patrono y/o socio;

area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 1] <- 1

#2 Los trabajadores familiares sin remuneración; 
area13$Informalidad[area13$P6430 == 6] <- 1

#3  3. Los trabajadores sin remuneración en empresas o negocios de otros hogares; 
area13$Informalidad[area13$P6430 == 7] <- 1
#4 Los empleados domésticos; 

area13$Informalidad[ area13$P6430 == 3] <- 1
#5 5. Los jornaleros o peones; 
area13$Informalidad[ area13$P6430 == 8] <- 1
#6Los trabajadores por cuenta propia que laboran en establecimientos hasta cinco
#personas, excepto los independientes profesionales;
area13$Informalidad[area13$P6430 == 4 
                    & area13$P3042 != 10
                    & area13$P3042 != 11 & area13$P3042 != 12
                    & area13$P3042 != 13] <- 1
#7. Los patrones o empleadores en empresas de cinco trabajadores o menos; 
area13$Informalidad[area13$P3069 <= 3 & area13$P6430 == 5] <- 1

# 8. Se excluyen los obreros o empleados del gobierno. 
area13$Informalidad[area13$P6430 == 2] <- 0

Informales_13A <- sum(area13[area13$Informalidad == 1,]$FEX_C18)
TI_13A <- Informales_13A/Ocupados_13A*100
 
# regreso a presencialidad - ver en pulso social y anexarlo a tasa de ocupación por sexo 
# aumento de TO femenina 

# Y por sectores? 