library(readr)

file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)

file_listC <- grep("Cabecera",file_list, value=TRUE)
file_listR <- grep("Resto",file_list, value=TRUE)
file_listA <- grep("Área",file_list, value=TRUE)

L2021C <- list()
L2021R <- list()
L2021A <- list()

for (i in 1:length(file_listC)) {
  L2021C[[i]] <- read.csv(file_listC[i], sep = ";")
  L2021R[[i]] <- read.csv(file_listR[i], sep = ";")
  L2021A[[i]] <- read.csv(file_listA[i], sep = ";")
}

file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
names(L2021A) <- grep("Cabecera",file_list, value=TRUE)
names(L2021C) <- grep("Cabecera",file_list, value=TRUE)
names(L2021R) <- grep("Resto",file_list, value=TRUE)



#

C <- d[[1]]
D <- d[[2]]                          
H   <- d[[3]]                  
Inactivos     <- L2021A[[4]]                       
Ocupados   <- L2021A[[5]]                          
Otras<- L2021A[[6]]
Otrosingresos  <- L2021A[[7]]                       
vivien <- L2021A[[8]] # único que no tiene orden



C$ID <- paste(as.character(C$DIRECTORIO),
              as.character(C$SECUENCIA_P),
              as.character(C$ORDEN), sep = "")


D$ID <- paste(as.character(D$DIRECTORIO),
              as.character(D$SECUENCIA_P),
              as.character(D$ORDEN), sep = "")



H$ID <- paste(as.character(H$DIRECTORIO),
              as.character(H$SECUENCIA_P),
              as.character(H$ORDEN), sep = "")


library(plyr)
S<-join(C,D, by ="ID") ##
co



?join
names(C)
names(S)
dim(S)
unique(D$DSCY)
unique(C$RAMA2D_D_R4)
unique(S$DSCY)

##############################################################################




procesamiento <- function(zona) {
  file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)
  
  file_zona <- grep(zona, file_list, value=TRUE)
  
  L2021 <- list()
  for (i in 1:length(file_listC)) {
    L2021[[i]] <- read.csv(file_zona[i], sep = ";")
  }
  
  file_names <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
  names(L2021) <- grep(zona,file_names, value=TRUE)
  L2021
  
}


d <- procesamiento("Cabecera")





