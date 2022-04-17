

# Crear carpeta de año 

# Generar bases

# Correr las funciones de estadísticas 

# Meter todo en la carpeta 




OutputGEIH <- function(año, mes){
  
  library("writexl")
  
  if(!file.exists("./Output")){dir.create("./Output")}
  if(!file.exists(paste0("./Output/", año))){dir.create(paste0("./Output/", año))}

  antiguaGEIH = c("2017","2018","2019","2020")   
  nuevosGEIH = c("2021","2022")
     
  if (año %in% antiguaGEIH){
    area <- ProcesamientoGEIH2005("Área", año , mes)
    resto <- ProcesamientoGEIH2005("Resto", año,  mes)
    cabecera <- ProcesamientoGEIH2005("Cabecera", año, mes) 
    
    output <- StatsGEIH2005(area,cabecera,resto)
   
    write_xlsx(output, paste0("./Output/", año,"/", mes, ".xlsx"))
    
  } else if (año == "2021" &  (mes %in% c("Enero","Febrero", "Marzo"))) {
    area <- ProcesamientoGEIH2005("Área",año , mes)
    resto <- ProcesamientoGEIH2005("Resto", año,  mes)
    cabecera <- ProcesamientoGEIH2005("Cabecera", año, mes) 
    
    output <- StatsGEIH2005(area,cabecera,resto)
    
    write_xlsx(output, paste0("./Output/", año,"/", mes, ".xlsx"))
    
  } else if (año %in% nuevosGEIH &  !(año == "2021" &  (mes %in% c("Enero","Febrero", "Marzo")))){
    
    base <- procesamientoGEIH2018(año, mes)
    
    output <- StatsGEIH2018(base)
    
    write_xlsx(output, paste0("./Output/", año,"/", mes, ".xlsx"))
  }
}

output("2019","Diciembre")  
  