
library(tidyverse)
library(magrittr)
library(here)

#### Lectura inicial ####

base_variables <- read_csv(here("datos_abiertos_ssa","diccionario_datos","base_variables.csv"),
                           locale = locale(encoding = "latin1")) 
tipos_cod <- read_csv(here("datos_abiertos_ssa","diccionario_datos","tipos_cod.csv"),
                      locale = locale(encoding = "latin1"),col_types = "ccc")
mx_grid_3 <- read_csv(here("datos_auxiliares","mx_state_grid3.csv"),
                      locale = locale(encoding = "latin1"))

tipos_col <- nrow(base_variables) %>% rep("c",.) %>% paste(collapse = "")
datos_covid <- read_csv(here("datos_abiertos_ssa","COVID19_Mexico_13.04.2020.csv"),
                        locale = locale(encoding = "latin1"),
                        col_types = tipos_col) %>% 
  mutate(BASE_ID = row_number() %>% paste("13_04_20",.,sep = "_")) %>% 
  select(BASE_ID,everything())

datos_covid %>% 
  filter(RESULTADO == 1) %>% 
  transmute(ENTIDAD_UM,FECHA_SINTOMAS = lubridate::ymd(FECHA_SINTOMAS)) %>% 
  count(ENTIDAD_UM,FECHA_SINTOMAS) %>% 
  spread(ENTIDAD_UM,n, fill = 0) %>% 
  gather(ENTIDAD_UM,n,-FECHA_SINTOMAS) %>% 
  arrange(FECHA_SINTOMAS) %>% 
  group_by(ENTIDAD_UM) %>% 
  mutate(n_acum = cumsum(n), 
         n_acum_mov = RcppRoll::roll_meanr(n_acum,5,fill = 1/5)) %>% 
  filter(n_acum >= 1) %>% 
  mutate(d_min = min(FECHA_SINTOMAS),
         DIAS_DESDE = FECHA_SINTOMAS - d_min) %>% 
  left_join(mx_grid_3, by = c("ENTIDAD_UM"="code_edo")) %>% 
  ungroup %>% 
  {ggplot(.,aes(x=DIAS_DESDE,y=n_acum_mov)) + 
      geom_line(data = select(.,-ENTIDAD_UM), aes(group=name), color = "gray85") + 
      geom_line(color = "chocolate2") + 
      geofacet::facet_geo(~ENTIDAD_UM,grid = mx_grid_3, label = "name") + 
      labs(title = "Casos confirmados acumulados de Covid19 por fecha de síntomas",
           subtitle = paste("Geofacet por entidad federativa con promedios móviles de 5 días.",
                            "Para cada entidad se muestra la evolución (naranja) comparada contra el resto de entidades (gris)",
                            sep = "\n"),
           x = "Días desde que el primer caso confirmado tuvo síntomas",
           y = "Escala logarítmica de casos acumulados",
           caption = "Elaborado por @fazepher. Fuente: Datos abiertos de la Secretaría de Salud al 13 de abril de 2020.") + 
      scale_y_log10() + 
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 90))}

geofacet_ingresos <- datos_covid %>% 
  filter(RESULTADO == 1) %>% 
  transmute(ENTIDAD_UM,FECHA_INGRESO = lubridate::ymd(FECHA_INGRESO)) %>% 
  count(ENTIDAD_UM,FECHA_INGRESO) %>% 
  spread(ENTIDAD_UM,n, fill = 0) %>% 
  gather(ENTIDAD_UM,n,-FECHA_INGRESO) %>% 
  arrange(FECHA_INGRESO) %>% 
  group_by(ENTIDAD_UM) %>% 
  mutate(n_acum = cumsum(n), 
         n_acum_mov = RcppRoll::roll_meanr(n_acum,5,fill = 1/5)) %>% 
  filter(n_acum >= 1) %>% 
  mutate(d_min = min(FECHA_INGRESO),
         DIAS_DESDE = FECHA_INGRESO - d_min) %>% 
  left_join(mx_grid_3, by = c("ENTIDAD_UM"="code_edo")) %>% 
  ungroup %>% 
  {ggplot(.,aes(x=DIAS_DESDE,y=n_acum_mov)) + 
      geom_line(data = select(.,-ENTIDAD_UM), aes(group=name), color = "gray85") + 
      geom_line(color = "chocolate2") + 
      geofacet::facet_geo(~ENTIDAD_UM,grid = mx_grid_3, label = "name") + 
      labs(title = "Casos confirmados acumulados de Covid19 por fecha de ingreso",
           subtitle = paste("Geofacet por entidad federativa con promedios móviles de 5 días.",
                            "Para cada entidad se muestra la evolución (naranja) comparada contra el resto de entidades (gris)",
                            sep = "\n"),
           x = "Días desde que el primer caso confirmado fue ingresado",
           y = "Escala logarítmica de casos acumulados",
           caption = "Elaborado por @fazepher. Fuente: Datos abiertos de la Secretaría de Salud al 13 de abril de 2020.") + 
      scale_y_log10() + 
      theme_classic() + 
      theme(axis.text.x = element_text(angle = 90))} %T>%
  ggsave(filename = here("grafs_ingresos","graf_ingresos_13_04_2020.pdf"),plot = .,
         device = cairo_pdf,width = 10, height = 10)
