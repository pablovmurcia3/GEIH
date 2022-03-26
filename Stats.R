library(dplyr)

arrange(select(chicago, pm25:year), year)

# Special operator -- chain different operations together -- pipe line



Ocupacion <- C %>% filter(AREA == 11) %>%  select(OCI, fex_c_2011) %>%  group_by(OCI) %>%
  summarize(O = sum(fex_c_2011, na.rm = TRUE))

Ocupación2 <- cabecera2021m11 %>% filter(area == 11) %>% select(oci, fex_c_2011) %>% group_by(oci) %>%
  summarize(O = sum(fex_c_2011, na.rm = TRUE))


Desempleo <- C %>% filter(AREA == 11) %>%  select(DSI, fex_c_2011) %>%  group_by(DSI) %>%
  summarize(O = sum(fex_c_2011, na.rm = TRUE))

Desempleo2 <- cabecera2021m11 %>% filter(area == 11) %>% select(dsi, fex_c_2011) %>% group_by(dsi) %>%
  summarize(O = sum(fex_c_2011, na.rm = TRUE))


Ocupacion <- A %>% filter(AREA == 11) %>%  select(OCI, fex_c_2011) %>%  group_by(OCI) %>%
  summarize(O = sum(fex_c_2011, na.rm = TRUE))



## Ocupación 


#Bogotá
A$OCI = as.character(A$OCI)
D <- A[A$AREA == 11,]
D <- D[complete.cases(D$OCI),]
sum(D$fex_c_2011)

