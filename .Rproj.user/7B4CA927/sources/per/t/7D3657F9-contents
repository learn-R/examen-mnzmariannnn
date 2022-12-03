#1. Instalar paquetes ----------------------------------------------------------

#comentar codigos 
pacman::p_load(tidyverse,
               haven, 
               forcats,
               car,
               sjmisc,
               sjPlot,
               survey,
               srvyr,
               dplyr)

#2. Importar datos -------------------------------------------------------------
  #2.1 Del estudio Latinobarometro (2018) en formato.dta
Latinobarometro2018 <- read_dta("input/data/Latinobarometro_2018_Esp_Stata_v20190303.dta")

  #2.2 Explorar datos
names(Latinobarometro2018)
sjPlot::view_df(Latinobarometro2018)
desc(Latinobarometro2018)

#3. Selección de variables -----------------------------------------------------
frq(Latinobarometro2018$P25ST)
frq(Latinobarometro2018$P75TI.E)
find_var(Latinobarometro2018, "policía")
class(Latinobarometro2018$P25ST)
find_var(Latinobarometro2018, "corrupción")

# Las variables seleccionadas son:

# - "IDENPA" : Identificación del País 
# - "P25ST" : Acuerdo/Desacuerdo: Se puede pagar el precio de cierto grado de corrupción
# - "P75TI.E : Proporcion de personas involucradas en Corrupción: La policia
# - "P15STGBSC.B" : Confianza en la Policía
# - "P15STGBSC.A" : Confianza en las Fuerzas Armadas
# - "SEXO": Sexo de los entrevistados
# - "EDAD": Edad de los entrevistados
# - "WT" : WT Ponderador


Percepcion_FuerzasdeOrdenySeguridad <- select(Latinobarometro2018, IDENPA, SEXO, EDAD, ponderador = WT,
                                              preciocorrupcion = P25ST,
                                              corrupcionpolicias = P75TI.E, conf_policia = P15STGBSC.B , 
                                              conf_fuerzasarmadas = P15STGBSC.A)

sjPlot::view_df(Percepcion_FuerzasdeOrdenySeguridad)


# 5. Transformación de las variables -------------------------------------------

PER_FZAS_proc  <- Percepcion_FuerzasdeOrdenySeguridad %>% 
  mutate_at(vars(SEXO, preciocorrupcion, corrupcionpolicias, conf_policia, conf_fuerzasarmadas), ~(as.numeric(.))) %>%
  filter(IDENPA == 170) %>%
  mutate(preciocorrupcion = car::recode(.$preciocorrupcion, recodes = c("1='Muy de acuerdo'; 2= 'De acuerdo';
  3= 'En desacuerdo'; 4= 'Muy en Desacuerdo'; c(-1) = NA"), as.factor= T),
  SEXO = car::recode(.$SEXO, recodes = c("1 = 'Hombre'; 2 = 'Mujer'"), as.factor = T,  levels = c('Hombre', 'Mujer')),
  EDAD = case_when(EDAD >= 16 & EDAD <= 25 ~ "Joven",
                   EDAD >= 26 & EDAD <= 60 ~ "Adulto",
                   EDAD > 61 ~ "Adulto mayor",
                   TRUE ~ NA_character_),
  corrupcionpolicias = car::recode(.$corrupcionpolicias, recodes = c("1= 'Ninguno'; 2 = 'Algunos'; 3= 'Casi todos';
                                                                     4= 'Todos'; c(-5,-4,-3,-2,-1) = NA", as.factor = T)),
  conf_policia = car::recode(.$conf_policia, recodes = c("1='Mucha'; 2= 'Algo'; 3= 'Poca'; 4= 'Ninguna'; c(-4,-3,-2,-1) = NA"), as.factor = T),
  conf_fuerzasarmadas = car::recode(.$conf_fuerzasarmadas, recodes = c("1='Mucha'; 2= 'Algo'; 3= 'Poca'; 4= 'Ninguna'; c(-4,-3,-2,-1) = NA"), as.factor = T)) %>%
    mutate_at(vars(EDAD, SEXO, preciocorrupcion, corrupcionpolicias, conf_fuerzasarmadas, conf_policia), ~(forcats::as_factor(.))) 

# 6. Visualización el set de datos -------------------------------------------

head(PER_FZAS_proc)

# 7. Guardar y exportar los datos ----------------------------------------

saveRDS(PER_FZAS_proc, file = "output/data/datos_proc.rds") #Guardamos este único set de datos en datos_proc.rds con los datos procesados

#Creacion Objeto encuesta

PER_FZAS_proc %>% 
  group_by(SEXO) %>% 
  frq(conf_policia)

PER_FZAS_proc %>% 
  group_by(EDAD) %>% 
  frq(conf_fuerzasarmadas)

obj_encuesta <- PER_FZAS_proc %>%
  as_survey_design(ids = 1, 
                   weights = ponderador)

#Análisis Bivariado

obj_encuesta %>% 
  group_by(SEXO, conf_fuerzasarmadas) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

obj_encuesta %>% 
  group_by(SEXO, conf_policia) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

obj_encuesta %>%
  group_by(EDAD, preciocorrupcion) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()

obj_encuesta %>%
  group_by(EDAD, corrupcionpolicias) %>%
  summarise(prop = survey_prop(na.rm = T)) %>%
  mutate(per = prop*100) %>%
  ungroup()



