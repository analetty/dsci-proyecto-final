################################################
####           Proyecto Final               ####
####         Ana Leticia Ibarra             ####
####         Ana Gabriela Ibarra            ####
####           Angelo Mazzocca              ####
####            Reysbel Ramos               ####
####            Sinai Suarez                ####
################################################

# Prueba 2. Equipo: Mazzocca, Ramos, Suarez
# Relacion entre IPC y Gasto Publico (% de PIB) de miembros del OCDE de LAC


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(readxl)
library(psych)
library(patchwork)


# Parte 1: Importar datos -------------------------------------------------

# Cambiamos al directorio donde tenemos los datasets

getwd()
setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

# Importamos los datasets a usar

setwd('DSci-ALIL/proyecto/dsci-proyecto-final/datasets')

raw_data <- read_excel("WEO_Data.xlsx")


raw_data <- raw_data[,c(1:2, 6:37)] %>% 
  rename('descriptor' = 'Subject Descriptor') %>% 
  mutate(descriptor = recode(descriptor, 
                             'Inflation, average consumer prices' = 'ipc',
                             'Implied PPP conversion rate' = 'ppp',
                             'Population' = 'poblacion',
                             'General government total expenditure' = 'gasto_ml'))



data <- na.omit(raw_data)



# Elaboracion tabla1 para analisis grafico

data1 <- data %>%
  pivot_longer(cols = `1990`:`2021`,
               names_to = "Ano",
               values_to = "obs")

pivot_wider(data1,names_from='descriptor', values_from='obs') %>%
  mutate(gasto_ml = str_replace_all(gasto_ml, "[,.]", ""),
         ppp = str_replace_all(ppp, "[,.]", ""),) %>%
  transform(ppp = as.numeric(ppp),
            ipc = as.numeric(ipc),
            poblacion = as.numeric(poblacion),
            gasto_ml = as.numeric(gasto_ml)) %>% 
  mutate(gasto_ml = gasto_ml/10^3,
         ppp = ppp/10^3,
         poblacion = poblacion/10^3,
         ipc = ipc/10^3)  %>%
  mutate(gasto_dolares = gasto_ml/ppp)  

IPC <- data1 %>%
  filter(`Subject Descriptor` %in% c("IPC"))

Gasto <- data1 %>%
  filter(`Subject Descriptor` %in% c("Gasto"))

tabla1 <- IPC %>%
  mutate(Gasto = Gasto$obs) %>%
  rename("IPC" = "obs")

tabla1$`Subject Descriptor` <- NULL

tabla1$Country <- as.factor(tabla1$Country)

tabla1$Ano <- as.double(tabla1$Ano)

remove(data1, Gasto, IPC)

# Elaboracion tabla2 para estadistica descriptiva

tabla2 <- unite(data, col = 'Variable', c('Subject Descriptor', 'Country'), sep = ' - ')

tabla2 <- as.data.frame(t(tabla2))

colnames(tabla2) <- tabla2[1,]

tabla2 <- tabla2[-1,]


tabla2[] <- (sapply(tabla2, as.double))

#Grafico exploratorio 

inflacion_grf <- tabla1 %>%
  filter(Country %in% c("Chile", "Colombia", "Costa Rica", "Mexico")) %>%
  ggplot(aes(Ano, IPC, color = Country)) + 
  geom_line() + ggtitle("IPC de los Paises OCDE en LATAM")

gasto_grf <- tabla1 %>%
  filter(Country %in% c("Chile", "Colombia", "Costa Rica", "Mexico")) %>%
  ggplot(aes(Ano, Gasto, color = Country)) + 
  geom_line() + ggtitle("Gasto (%PIB) de los Paises OCDE en LATAM")

inflacion_grf + gasto_grf

# Estadistica descriptiva

summary(tabla2)

describe(tabla2)

scatterHist(tabla2[c("Gasto - Chile", "IPC - Chile")])
scatterHist(tabla2[c("Gasto - Colombia", "IPC - Colombia")])
scatterHist(tabla2[c("Gasto - Costa Rica", "IPC - Costa Rica")])
scatterHist(tabla2[c("Gasto - Mexico", "IPC - Mexico")])

