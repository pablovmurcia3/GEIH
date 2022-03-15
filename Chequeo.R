
Chequeo <- function(mes) {
  
      library(stringi)
      file_list <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/", mes), full.names = TRUE)
      
      lista <- list()
      for (i in 1:length(file_list)) {
        lista[[i]] <- read.csv(file_list[i], sep = ";")
      }
      
      file_names <- list.files(path = paste0("C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/", mes))
      names(lista) <- toupper(stri_trans_general(file_names,"Latin-ASCII"))
      lista
      lista <- lista[-grep("VIVIENDA", names(lista))]
      
      for (i in 1:length(lista)) {
        print(names(lista[[i]][1:3]))
      }
  
}

Chequeo("noviembre")
names(d)


file_names <- list.files(path = "C:/Users/pablo/OneDrive - Universidad del rosario/Probogota/Observatorio/Mercado Laboral/Análisis de datos/GEIH/2021/Diciembre")
