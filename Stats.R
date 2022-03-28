library(dplyr)

arrange(select(chicago, pm25:year), year)

# Special operator -- chain different operations together -- pipe line



A$PET <- ifelse(A$P6040 >= 15,1,0)
A$PEA <- ifelse(A$OCI == 1 | A$DSI ==1 ,1,0)
A$fex_c_2011 <- sub(",",".",A$fex_c_2011)
A$fex_c_2011 <-as.numeric(A$fex_c_2011)



## Ocupación 


# Bogotá


PET <- sum(A[A$PET ==1 & A$AREA == 11 ,]$FEX_C18)
Ocupados <- sum(A[A$AREA == 11 & complete.cases(A$OCI) & A$P6040 >= 15,]$FEX_C18)
TO <- Ocupados/PET*100


# 13 áreas 

PET <- sum(A[A$PET ==1,]$FEX_C18)
Ocupados <- sum(A[complete.cases(A$OCI) & A$P6040 >= 15,]$FEX_C18)
TO <- Ocupados/PET*100

a<- complete.cases(A$OCI)

bz- A$OCI==1

## Desenpleo 

# Bogotá
PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6040 >= 15 ,]$FEX_C18)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI)  & A$P6040 >= 15,]$FEX_C18)
TD <- Desempleados/PEA*100


# 13 áreas 
PEA <- sum(A[complete.cases(A$PEA) & A$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(A[complete.cases(A$DSI) & A$P6040 >= 15,]$FEX_C18)
TD <- Desempleados/PEA*100


## por sexo - Desempleo 

# Bogotá
PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6020 == 1 & A$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI) & A$P6020 == 1 & A$P6040 >= 15,]$FEX_C18)
TDH <- Desempleados/PEA*100

PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6020 == 2 & A$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI) & A$P6020 == 2 & A$P6040 >= 15,]$FEX_C18)
TDM <- Desempleados/PEA*100

# 13 áreas
PEA <- sum(A[complete.cases(A$PEA) & A$P6020 == 1 & A$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(A[complete.cases(A$DSI) & A$P6020 == 1 & A$P6040 >= 15,]$FEX_C18)
TDH <- Desempleados/PEA*100

PEA <- sum(A[complete.cases(A$PEA) & A$P6020 == 2 & A$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(A[complete.cases(A$DSI) & A$P6020 == 2 & A$P6040 >= 15,]$FEX_C18)
TDM <- Desempleados/PEA*100


## por años - Desempleo 


A$age <- cut(A$P6040, breaks = c(12,28,39,49, 59, 130))

levels(A$age)
Aage <- A[complete.cases(A$age),]


list <- split(Aage, Aage$age) 
s<-sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA) & x$AREA == 11 & x$P6040 >= 15 ,]$FEX_C18)
  Desempleados <- sum(x[x$AREA == 11 & complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
  TD <- Desempleados/PEA*100
})          

sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA)  & x$P6040 >= 15,]$FEX_C18)
  Desempleados <- sum(x[complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
  TD <- Desempleados/PEA*100
}) 

unname(s)

## Informalidad

A$Informalidad <- rep(0, nrow(A))

A$Informalidad[A$P6870 <= 3 & A$P6430 == 1 & A$P6040 >= 15] <- 1
A$Informalidad[A$P6870 <= 3 & A$P6430 == 6 & A$P6040 >= 15] <- 1
A$Informalidad[A$P6870 <= 3 & A$P6430 == 3 & A$P6040 >= 15] <- 1
A$Informalidad[A$P6870 <= 3 & A$P6430 == 8 & A$P6040 >= 15] <- 1
A$Informalidad[A$P6870 <= 3 & A$P6430 == 4 & A$OFICIO>20 & complete.cases(A$OFICIO) & A$P6040 >= 15] <- 1
A$Informalidad[A$P6870 <= 3 & A$P6430 == 5 & A$P6040 >= 15] <- 1
A$Informalidad[A$P6430 == 7 & A$P6040 >= 15] <- 1

# Bogotá
Ocupados <- sum(A[A$AREA == 11 & complete.cases(A$OCI) & A$P6040 >= 15,]$FEX_C18)
Informales <- sum(A[A$AREA == 11 & A$Informalidad == 1 & A$P6040 >= 15,]$FEX_C18)
TI <- Informales/Ocupados*100


# 13 áreas
Ocupados <- sum(A[complete.cases(A$OCI) & A$P6040 >= 15,]$FEX_C18)
Informales <- sum(A[A$Informalidad == 1 & A$P6040 >= 15,]$FEX_C18)
TI <- Informales/Ocupados*100


#colombia

C <- C[, names(C) %in% names(R)]
R <- R[, names(R) %in% names(C)]

C <- C[ , order(names(C))]
R <- R[ , order(names(R))]

Col <- rbind(C,R)

# Col$PET <- ifelse(Col$P6040 >= 12,1,0)

Col$PET <- ifelse(Col$P6040 >= 15,1,0)
Col$PEA <- ifelse(Col$OCI == 1 | Col$DSI ==1 ,1,0)

Col$fex_c_2011 <- sub(",",".",Col$fex_c_2011)
Col$fex_c_2011 <-as.numeric(Col$fex_c_2011)

## Ocupación 
#Col$PET[Col$CLASE == 1 & Col$P6040 >= 12] <- 1
#Col$PET[Col$CLASE == 2 & Col$P6040 >= 10] <- 1

PET <- sum(Col[Col$PET ==1 ,]$FEX_C18)
Ocupados <- sum(Col[complete.cases(Col$OCI) & Col$P6040 >= 15,]$FEX_C18)
TO <- Ocupados/PET*100



## Desempleo  
PEA <- sum(Col[complete.cases(Col$PEA)  & Col$P6040 >= 15 ,]$FEX_C18)
Desempleados <- sum(Col[complete.cases(Col$DSI) & Col$P6040 >= 15,]$FEX_C18)
TD <- Desempleados/PEA*100

## por sexo - Desempleo 

PEA <- sum(Col[complete.cases(Col$PEA) & Col$P6020 == 1 & Col$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(Col[complete.cases(Col$DSI) & Col$P6020 == 1 & Col$P6040 >= 15,]$FEX_C18)
TDH <- Desempleados/PEA*100

PEA <- sum(Col[complete.cases(Col$PEA) & Col$P6020 == 2 & Col$P6040 >= 15,]$FEX_C18)
Desempleados <- sum(Col[complete.cases(Col$DSI) & Col$P6020 == 2 & Col$P6040 >= 15,]$FEX_C18)
TDM <- Desempleados/PEA*100


## por años - Desempleo 

Col$age <- cut(Col$P6040, breaks = c(12,28,39,49, 59, 130))
Colage <- Col[complete.cases(Col$age),]

list <- split(Colage, Colage$age) 
sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA) & x$P6040 >= 15 ,]$FEX_C18)
  Desempleados <- sum(x[complete.cases(x$DSI) & x$P6040 >= 15,]$FEX_C18)
  TD <- Desempleados/PEA*100
})   



## Informalidad

Col$Informalidad <- rep(0, nrow(Col))

Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 1 & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 6 & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 3 & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 8 & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 4 & Col$OFICIO>20 & complete.cases(Col$OFICIO) & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6870 <= 3 & Col$P6430 == 5 & Col$P6040 >= 15] <- 1
Col$Informalidad[Col$P6430 == 7 & Col$P6040 >= 15] <- 1


Ocupados <- sum(Col[complete.cases(Col$OCI)  & Col$P6040 >= 15,]$FEX_C18)
Informales <- sum(Col[Col$Informalidad == 1 & Col$P6040 >= 15,]$FEX_C18)
TI <- Informales/Ocupados*100


