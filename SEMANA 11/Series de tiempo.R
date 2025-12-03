#===================================================================#
# Universidad Nacional del Centro del Perú                          #
# Facultad de Economía                                              #
# Curso: Informática para Economistas                               #
# Tema: Análisis de una serie de tiempo                             #
# Docente: Ciro Ivan Machacuay Meza                                 #
#===================================================================#

# Identificar si contamos con pacman -----

if(require("pacman", quietly = T)){
  print("El paque pacman se encuentra instalado")
} else{
  install.packages("pacman", dependencies = T)
}

# llamado de paquetes -----

pacman::p_load(
  "tidyverse",
  "foreign",
  "openxlsx",
  "scales"
)

# Cargar script de Datos_BCRP -----

source("FUNCIONES/Datos_BCRP.R")

# Llamar datos del PBI de 1979 a 2025 -----

PBI <- BCRP_api_series(id_serie = "PN02538AQ",
                       inicio = "1979-1",
                       final = "2025-3",
                       periodo = "Trimestral") %>% 
  
#para cambiar la variable en la tabla
  rename(pbi = Series)


# Gráfica de una serie de tiempo -----
install.packages("ggplot2")
library(ggplot2)
install.packages("magrittr")
library(magrittr)
install.packages("dplyr")
library(dplyr)

#Gráfico 1
PBI %>% # ctrl+shift+m o tambien escribes: |> que es lo mismo
  ggplot(aes(x = Fecha, y = pbi))+
  geom_line()
  

#Gráfico 2
PBI %>%
  ggplot(aes(x = Fecha, y = pbi)) +  # El + es importante aquí
  geom_line() +
  labs(title = "Serie de Tiempo del PBI",
       subtitle = "del trimestre I de 1979 al trimestre I de 2025\n(Cifras en millones de soles del 2007)",
       x = "",
       y = "PBI (millones de soles)") +
  theme(plot.title = element_text(size = 14, 
                                  hjust = 0.5, 
                                  face = "bold"),
        plot.subtitle = element_text(size = 12, 
                                     hjust = 0.5),
        plot.caption = element_text(size = 10, 
                                    hjust = 0),
        axis.text.x = element_text(angle = 90))


#Gráfico 3
PBI %>% # ctrl+shift+m o tambien escribes: |> que es lo mismo
  ggplot(aes(x = Fecha, y = pbi))+
  geom_line(linewidth = 1.1,
            color = "steelblue4")+
  scale_y_continuous(labels = comma)+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  labs(title = "Producto Bruto Interno de Perú",
       subtitle = "del trimestre I de 1979 al trimestre I de 2025\n(cifras en millones de soles del 2007)",
       x = "",
       y = "",
       caption = "Fuente. Banco Central de la Reversa del Perú (BCRP). BCRPData.")+
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12,
                                     hjust = 0.5),
        plot.caption = element_text(size = 10,
                                    hjust = 0),
        axis.text.x = element_text(angle = 90))


## Almacenar gráfico G1----

dir.create("salidas/Clase12")

ggsave(PBI %>% # ctrl+shift+m o tambien escribes: |> que es lo mismo
         ggplot(aes(x = Fecha, y = pbi))+
         geom_line(linewidth = 1.1,
                   color = "steelblue4")+
         scale_y_continuous(labels = comma)+
         scale_x_date(date_breaks = "1 year",
                      date_labels = "%Y")+
         labs(title = "Producto Bruto Interno de Perú",
              subtitle = "del trimestre I de 1979 al trimestre I de 2025\n(cifras en millones de soles del 2007)",
              x = "",
              y = "",
              caption = "Fuente. Banco Central de la Reversa del Perú (BCRP). BCRPData.")+
         theme(plot.title = element_text(size = 14,
                                         hjust = 0.5,
                                         face = "bold"),
               plot.subtitle = element_text(size = 12,
                                            hjust = 0.5),
               plot.caption = element_text(size = 10,
                                           hjust = 0),
               axis.text.x = element_text(angle = 90)),
       width = 9,
       height = 5,
       dpi = 500,
       units = "in",
       filename = "Salidas/Clase12/G1.png")


#Práctica guardar los dos primeros gráfico en la carpeta SALIDAS"




# Descomposición temporal ------

PBI_ts <- ts(PBI$pbi, start = c(1979,1), end = c(2025,3), frequency = 4)

## Idea original -----

plot(decompose(PBI_ts))

## Usando tidyverse para una visualización completa -----

PBI_componente <- data.frame(
  Fecha = PBI$Fecha,
  PBI = PBI$pbi,
  Estacional = decompose(PBI_ts)$seasonal,
  Tendencia = decompose(PBI_ts)$trend,
  Aleatorio = decompose(PBI_ts)$random
)



library(ggplot2)
library(magrittr)
library(dplyr)
library(tidyr)  # Cargar tidyr

PBI_componente %>% 
  gather("id_var", "valores", -Fecha) %>% 
  mutate(id_var = factor(id_var, levels = c("PBI",
                                            "Tendencia",
                                            "Estacional",
                                            "Aleatorio"))) %>% 
  ggplot(aes(x = Fecha, y = valores))+
  geom_line(aes(color = id_var),
            linewidth =1)+
  scale_y_continuous(labels = comma)+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  facet_wrap(.~id_var, scales = "free_y")+
  labs(title = "Descomposición del Producto Bruto Interno de Perú",
       subtitle = "del trimestre I de 1979 al trimestre I de 2025\n(cifras en millones de soles del 2007)",
       x = "",
       y = "",
       caption = "Fuente. Banco Central de la Reversa del Perú (BCRP). BCRPData.\nNota: El método de descomposición temporal es el aditivo.")+
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12,
                                     hjust = 0.5),
        plot.caption = element_text(size = 10,
                                    hjust = 0),
        axis.text.x = element_text(angle = 90),
        legend.position = "none")



G2 <- PBI_componente %>% 
  gather("id_var", "valores", -Fecha) %>% 
  mutate(id_var = factor(id_var, levels = c("PBI",
                                            "Tendencia",
                                            "Estacional",
                                            "Aleatorio"))) %>% 
  ggplot(aes(x = Fecha, y = valores))+
  geom_line(aes(color = id_var),
            linewidth =1)+
  scale_y_continuous(labels = comma)+
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y")+
  facet_wrap(.~id_var, scales = "free_y")+
  labs(title = "Descomposición del Producto Bruto Interno de Perú",
       subtitle = "del trimestre I de 1979 al trimestre I de 2025\n(cifras en millones de soles del 2007)",
       x = "",
       y = "",
       caption = "Fuente. Banco Central de la Reversa del Perú (BCRP). BCRPData.\nNota: El método de descomposición temporal es el aditivo.")+
  theme(plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 12,
                                     hjust = 0.5),
        plot.caption = element_text(size = 10,
                                    hjust = 0),
        axis.text.x = element_text(angle = 90),
        legend.position = "none")


G2

## Almacenar gráfico G2----

ggsave(G2,
       width = 9,
       height = 5,
       dpi = 500,
       units = "in",
       filename = "Salidas/Clase12/G2.png")
