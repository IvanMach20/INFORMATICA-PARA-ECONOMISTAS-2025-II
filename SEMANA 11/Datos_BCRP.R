# Verificar si pacman está instalado en la versión R ----

if(!require("pacman", quietly = T)){
  install.packages("pacman", dependencies = TRUE)
}else{
  cat("El paquete pacman se encuentra instalado en tu versión de R")
}

# Llamado de paquetes básicos para el tema -----

pacman::p_load(
  "httr",
  "jsonlite",
  "tidyverse",
  "curl"
)

## Programación para función de BCRP -----

url<-"https://estadisticas.bcrp.gob.pe/estadisticas/series/api/PN01288PM/json/2013-1/2016-9"

BCRP_api_series <- function(
    id_serie,
    inicio,
    final,
    periodo = c("Mensual", "Trimestral")){
  if(periodo=="Mensual"){
    ## Lectura de web
    url_API <- paste0(
      "https://estadisticas.bcrp.gob.pe/estadisticas/series/api/",
      id_serie, "/json/", inicio, "/", final
    )
    ## Llamando info del API
    Respuesta <- GET(url_API)
    Datos <- content(Respuesta, "text", encoding = "latin1")
    Datos <- paste(Datos)
    ## Obteneción de la lista de observaciones
    Datos <- fromJSON(Datos)
    ## Manipulación de datos
    Datos <- Datos[["periods"]] %>% 
      rename(Fecha=name,
             Series=values) %>% 
      mutate(
        Año = substr(Fecha, 5,8),
        Mes = substr(Fecha, 1,3),
        Mes = case_when(Mes=="Ene"~"01",
                        Mes=="Feb"~"02",
                        Mes=="Mar"~"03",
                        Mes=="Abr"~"04",
                        Mes=="May"~"05",
                        Mes=="Jun"~"06",
                        Mes=="Jul"~"07",
                        Mes=="Ago"~"08",
                        Mes=="Sep"~"09",
                        Mes=="Oct"~"10",
                        Mes=="Nov"~"11",
                        Mes=="Dic"~"12"),
        Series = as.numeric(Series),
        Fecha = paste0(Año, "-", Mes, "-01"),
        Fecha = as.Date(Fecha, format = "%Y-%m-%d")
        ) %>% 
     dplyr::select(Fecha, Series)
  }
  else{
    
  }
  if(periodo=="Trimestral"){
    ## Lectura de web
    url_API <- paste0(
      "https://estadisticas.bcrp.gob.pe/estadisticas/series/api/",
      id_serie, "/json/", inicio, "/", final
    )
    ## Llamando info del API
    Respuesta <- GET(url_API)
    Datos <- content(Respuesta, "text", encoding = "latin1")
    Datos <- paste(Datos)
    ## Obteneción de la lista de observaciones
    Datos <- fromJSON(Datos)
    ## Manipulación de datos
    Datos <- Datos[["periods"]] %>% 
      rename(Fecha=name,
             Series=values) %>% 
      mutate(
        Año = substr(Fecha, 4,5),
        Año = as.numeric(Año),
        Año = ifelse(
          Año %in%(0:9),
          paste0("200", Año),
          ifelse(Año %in% c(10:30),
                 paste0("20", Año),
                 paste0("19", Año))
        ),
        Trim = substr(Fecha, 1,2),
        Trim = case_when(Trim=="T1"~"01",
                        Trim=="T2"~"04",
                        Trim=="T3"~"07",
                        Trim=="T4"~"10"),
        Series = as.numeric(Series),
        Fecha = paste0(Año, "-", Trim, "-01"),
        Fecha = as.Date(Fecha, format = "%Y-%m-%d")
      ) %>% 
      dplyr::select(Fecha, Series)
  }
  else{
    
  }
  Datos
}

### Ejemplo de descarga de datos ----

# PBI <- BCRP_api_series(id_serie = "PN02538AQ",
#                        inicio = "1979-1",
#                        final = "2022-3",
#                        periodo = "Trimestral")
# 
# Imp_Aduana <- BCRP_api_series(id_serie = "PN01219PM",
#                               inicio = "2005-1",
#                               final = "2020-12",
#                               periodo = "Mensual")

