  ProcesamientoGEIH2005 <- function(zona, año, mes) {
    
    library(stringi)
    # Paquete necesario para manipular nombres
    
    file_list <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH05 - ", año,"/", mes), full.names = TRUE)
    file_zona <- grep(zona, file_list, value=TRUE)
    # 1) Se crea una lista con todos los archivos
    # 2) se crea una lista con los archivos de la zona seleccioonada en la función
    
    lista <- list()
    for (i in 1:length(file_zona)) {
      lista[[i]] <- read.csv(file_zona[i], sep = ";")
      names(lista[[i]])[1] <- "DIRECTORIO"
    }
    # 3) se leen todos los archos de la lista. 
    # Estas data frames se meten en otra lista
    
    
    file_names <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH05 - ", año,"/", mes))
    names(lista) <- grep(zona,file_names, value=TRUE)
    names(lista) <- toupper(stri_trans_general(names(lista),"Latin-ASCII"))
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
    dataJoin <- merge(datCaracteristicas, datVivienda, 
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
    
    fex18 <- read.csv(paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/GEIH05 - ", año,"/Fex proyeccion CNPV_2018.csv"),sep = ";")
    names(fex18) <- toupper(stri_trans_general(names(fex18),"Latin-ASCII"))
    dataJoin<- merge(dataJoin,fex18, by = c("DIRECTORIO", "SECUENCIA_P", "ORDEN"))
    
    
    
  }

  
A <- ProcesamientoGEIH2005("Área","2020" ,"Diciembre")
R <- ProcesamientoGEIH2005("Resto", "2020",  "Diciembre")
C <- ProcesamientoGEIH2005("Cabecera", "2020", "Diciembre")  


