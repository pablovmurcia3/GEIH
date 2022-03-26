
procesamientoGEIH2018 <- function(año, mes) {
  
  library(stringi)
  # Paquete necesario para manipular nombres
  
  file_list <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - ", año,"/", mes), full.names = TRUE)
  # 1) Se crea una lista con todos los archivos

  lista <- list()
  for (i in 1:length(file_list)) {
    lista[[i]] <- read.csv(file_list[i], sep = ",")
    lista[[i]] <- lista[[i]][,-grep("PERIODO", names(lista[[i]]))]
  }
  # 3) se leen todos los archos de la lista. 
  # Estas data frames se meten en otra lista
 
  file_names <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH18 - ", año,"/", mes))
  names(lista) <- toupper(stri_trans_general(file_names,"Latin-ASCII"))
  # 4) Se les da nombres a los elementos de la lista 
  # (mismos que el DANE)
  # 5) Se modifican los nombres (En mayúsculas y sin tildes)
  
  
  datCaracteristicas <- lista[[grep("CARACTERISTICAS", names(lista))]]
  datVivienda <- lista[[grep("VIVIENDA", names(lista))]]
  # 6) Se extraen las dos data frames que se uniran de primeras
  # (esta unión es diferente a las que vienen entonces 
  # se hace fuera del loop)
  
  
  orden <- !(names(datVivienda) %in% names(datCaracteristicas))
  orden[1:2] <- TRUE
  datVivienda <- datVivienda[, orden]
  dataJoin1 <- merge(datCaracteristicas, datVivienda, 
                     by = c("DIRECTORIO", "SECUENCIA_P"),
                     all = TRUE)
  # 7) De la data frame de vivienda se eliminan las columnas repetidas
  # con la  data frame de características (esto se hace para 
  # que no  se repitan las columnas después del merge)
  # 8) Se procede con la unión
  
  lista <- lista[-grep("VIVIENDA", names(lista))]
  lista <- lista[-grep("CARACTERISTICAS", names(lista))]
  # Como ya se usaron, se eliminan las bases de viviendas 
  # y de características de la lista que contienen las demás
  
  dataJoin2 <- lista[[1]]
  lista <- lista[-1]
  
  for (i in 1:length(lista)) {
    data <- lista[[i]]
    orden <- !(names(data) %in% names(dataJoin2))
    orden[1:3] <- TRUE
    data <- data[,orden]
    dataJoin2 <- merge(dataJoin2,data, 
                      by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                      all = TRUE)
  }
  
  
  orden <- !(names(dataJoin1) %in% names(dataJoin2))
  orden[1:2] <- TRUE
  dataJoin1 <- dataJoin1[, orden]
  dataJoin <- merge(dataJoin1, dataJoin2, 
                     by = c("DIRECTORIO", "SECUENCIA_P"),
                     all = TRUE)
  
  dataJoin
  
}

d <- procesamientoGEIH2018("2022", "Enero" )
