Stats_Muj <- function(A){
  # Filtrad0
  bog <- A[A$AREA == 11, ]
  bog$fex_c_2011 <- as.numeric(sub(",",".",bog$fex_c_2011))

  
  # Preparación  
  bog$PEA <- ifelse(bog$OCI == 1 | bog$DSI ==1 ,1,0)  
  bog$PEA[is.na(bog$PEA)] <- 0
  bog$PET <- ifelse(bog$P6040 >= 12,1,0)
  bog$age <- cut(bog$P6040, breaks = c(13,28,39,49, 59, 130))
  
  # Stats básicas
  PT <- sum(bog$fex_c_2011)
  PET <- sum(bog[bog$PET ==1,]$fex_c_2011)
  PEA <- sum(bog[bog$PEA ==1,]$fex_c_2011)
  
  INAC <- PET - PEA
  
  ##################### Gráfica 3 ############################################
  
  PET_H <- sum(bog[bog$PET == 1 & bog$P6020 == 1,]$fex_c_2011)
  PET_M <- sum(bog[bog$PET == 1 & bog$P6020 == 2,]$fex_c_2011)
  
  PEA_H <- sum(bog[bog$PEA == 1 & bog$P6020 == 1,]$fex_c_2011)
  PEA_M <- sum(bog[bog$PEA == 1 & bog$P6020 == 2,]$fex_c_2011)
  
  INAC_H <- PET_H - PEA_H
  INAC_M <- PET_M - PEA_M
  INAC_H
  INAC_M
  
  

  
  ##################### Gráfica 4 ############################################
  bog_H_1 <- bog_H[complete.cases(bog_H$P7458),]
  bog_M_1 <- bog_M[complete.cases(bog_M$P7458),]
  
  list_H <- split(bog_H_1,bog_H_1$P7458) 
  
  Razones_H <- sapply(list_H, function(x) {
    r <- sum(x[x$INI ==1,]$fex_c_2011)
  }) 
  Razones_H
  
  
  list_M <- split(bog_M_1,bog_M_1$P7458) 
  
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
  names(Razones_H) <- str_replace_all(names(Razones_H) ,dictio_replace)
  names(Razones_M) <- str_replace_all(names(Razones_M) ,dictio_replace)
  names(Razones_H) <- paste(names(Razones_H), "(H)")
  names(Razones_M) <- paste(names(Razones_M), "(M)")
  
  ##################### Gráfica 8 ############################################
  
  bog_age_H <- bog_H[complete.cases(bog_H$age),]
  bog_age_M <- bog_M[complete.cases(bog_M$age),]
  
  list_H_age <- split(bog_age_H,bog_age_H$age) 
  TD_H_age <- sapply(list_H_age, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[complete.cases(x$DSI) & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA*100
  }) 
  
  
  list_M_age <- split(bog_age_M,bog_age_M$age) 
  TD_M_age <- sapply(list_M_age, function(x) {
    PEA <- sum(x[x$PEA == 1 & x$PET == 1,]$fex_c_2011)
    Desempleados <- sum(x[complete.cases(x$DSI) & x$PET == 1,]$fex_c_2011)
    TD <- Desempleados/PEA*100
  }) 
  
  names(TD_H_age) <- paste(names(TD_H_age), "(H)")
  names(TD_M_age) <- paste(names(TD_M_age), "(M)")
  
  
  ##################### Vector Resultante ####################################
  
  
  Bogota <- c(INAC_H, INAC_M, Razones_H, Razones_M, TD_H_age, TD_M_age)
  
  names <- c("Inactivos hombres", "Inactivos mujeres", names(Razones_H), 
             names(Razones_M),   names(TD_H_age),  names(TD_M_age))
  
  stats <- data.frame(names, Bogota)
  stats$Bogota <- as.numeric(stats$Bogota)
  stats
  
  
  
}




Ene <- Stats_Muj(A1)
library("writexl")
write_xlsx(Feb,"Feb.xlsx")

Feb <- Stats_Muj(A2)

Mar <- Stats_Muj(A3)

