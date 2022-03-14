library(stringi)


procesamiento <- function(zona) {
      
      file_list <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre", full.names = TRUE)
      
      file_zona <- grep(zona, file_list, value=TRUE)
      
      lista <- list()
      
      for (i in 1:length(file_zona)) {
        lista[[i]] <- read.csv(file_zona[i], sep = ";")
      }
      
      file_names <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
      names(lista) <- grep(zona,file_names, value=TRUE)
      names(lista) <- toupper(stri_trans_general(names(lista),"Latin-ASCII"))
      
      datCaracteristicas <- lista[[grep("CARACTERISTICAS", names(lista))]]
      datVivienda <- lista[[grep("VIVIENDA", names(lista))]]
      
      orden <- !(names(datVivienda) %in% names(datCaracteristicas))
      orden[1:2] <- TRUE
      datVivienda <- datVivienda[, orden]
      dataJoin <- merge(datCaracteristicas, datVivienda, 
                        by = c("DIRECTORIO", "SECUENCIA_P"),
                        all = TRUE)
      
      lista <- lista[-grep("VIVIENDA", names(lista))]
      lista <- lista[-grep("CARACTERISTICAS", names(lista))]
      
      for (i in 1:length(lista)) {
        data <- lista[[i]]
        orden <- !(names(data) %in% names(dataJoin))
        orden[1:3] <- TRUE
        orden
        data <- data[,orden]
        dataJoin <- merge(dataJoin,data, 
                          by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"),
                          all = TRUE)
        
      }
      dataJoin
}


g <- procesamiento("Resto")

