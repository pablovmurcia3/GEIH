library(dplyr)

arrange(select(chicago, pm25:year), year)

# Special operator -- chain different operations together -- pipe line



A$PET <- ifelse(A$P6040 >= 12,1,0)
A$PEA <- ifelse(A$OCI == 1 | A$DSI ==1 ,1,0)


## Ocupación 


# Bogotá


PET <- sum(A[A$PET ==1 & A$AREA == 11 ,]$fex_c_2011)
Ocupados <- sum(A[A$AREA == 11 & complete.cases(A$OCI),]$fex_c_2011)
TO <- Ocupados/PET*100


# 13 áreas 

PET <- sum(A[A$PET ==1,]$fex_c_2011)
Ocupados <- sum(A[complete.cases(A$OCI),]$fex_c_2011)
TO <- Ocupados/PET*100

a<- complete.cases(A$OCI)

bz- A$OCI==1

## Desenpleo 

# Bogotá
PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 ,]$fex_c_2011)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI),]$fex_c_2011)
TD <- Desempleados/PEA*100


# 13 áreas 
PEA <- sum(A[complete.cases(A$PEA),]$fex_c_2011)
Desempleados <- sum(A[complete.cases(A$DSI),]$fex_c_2011)
TD <- Desempleados/PEA*100


## por sexo - Desempleo 

# Bogotá
PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6020 == 1,]$fex_c_2011)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI) & A$P6020 == 1,]$fex_c_2011)
TDH <- Desempleados/PEA*100

PEA <- sum(A[complete.cases(A$PEA) & A$AREA == 11 & A$P6020 == 2,]$fex_c_2011)
Desempleados <- sum(A[A$AREA == 11 & complete.cases(A$DSI) & A$P6020 == 2,]$fex_c_2011)
TDM <- Desempleados/PEA*100

# 13 áreas
PEA <- sum(A[complete.cases(A$PEA) & A$P6020 == 1,]$fex_c_2011)
Desempleados <- sum(A[complete.cases(A$DSI) & A$P6020 == 1,]$fex_c_2011)
TDH <- Desempleados/PEA*100

PEA <- sum(A[complete.cases(A$PEA) & A$P6020 == 2,]$fex_c_2011)
Desempleados <- sum(A[complete.cases(A$DSI) & A$P6020 == 2,]$fex_c_2011)
TDM <- Desempleados/PEA*100


## por años - Desempleo 


A$age <- cut(A$P6040, breaks = c(12,28,39,49, 59, 130))

levels(A$age)
Aage <- A[complete.cases(A$age),]


list <- split(Aage, Aage$age) 
sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA) & x$AREA == 11 ,]$fex_c_2011)
  Desempleados <- sum(x[x$AREA == 11 & complete.cases(x$DSI),]$fex_c_2011)
  TD <- Desempleados/PEA*100
})          

sapply(list, function(x) {
  PEA <- sum(x[complete.cases(x$PEA) ,]$fex_c_2011)
  Desempleados <- sum(x[complete.cases(x$DSI),]$fex_c_2011)
  TD <- Desempleados/PEA*100
}) 
