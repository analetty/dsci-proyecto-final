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
library(tmap)
library(sf)  

# Parte 1: Importar datos -------------------------------------------------

# Cambiamos al directorio donde tenemos los datasets

getwd()
#setwd('..')
#setwd("prueba2/dsci-prueba2/datasets") #Directorio donde tenemos el archivo

# Importamos los datasets a usar

setwd('dsci-proyecto-final/datasets')

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

clean_data <- pivot_wider(data1,names_from='descriptor', values_from='obs') %>%
  mutate(gasto_ml = str_replace_all(gasto_ml, "[,.]", ""),
         ppp = str_replace_all(ppp, "[,.]", ""),) %>%
  transform(
            Ano = as.double(Ano),
            ppp = as.numeric(ppp),
            ipc = as.numeric(ipc),
            poblacion = as.numeric(poblacion),
            gasto_ml = as.numeric(gasto_ml)) %>% 
  mutate(gasto_ml = gasto_ml/10^3,
         ppp = ppp/10^3,
         poblacion = poblacion/10^3,
         ipc = ipc/10^3)  %>%
  mutate(gasto_dolares = gasto_ml/ppp)  

# Parte 2: Análisis -------------------------------------------------------

# Regresión mínimos cuadrados ordinarios

# Se desea conocer cómo el gasto explica el ipc

reg_ipc_gasto <- lm(ipc~gasto_dolares, clean_data)

sum(reg_ipc_gasto$coefficients)


# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.06546

# Para Chile
cd_chile <- clean_data %>% filter(Country == 'Chile')
reg_ipc_gasto_cl <- lm(ipc~gasto_dolares, cd_chile)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.5658


# Para Colombia
cd_colombia <- clean_data %>% filter(Country == 'Colombia')
reg_ipc_gasto_col <- lm(ipc~gasto_dolares, cd_colombia)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.3984



# Para Costa Rica
cd_costa_rica <- clean_data %>% filter(Country == 'Costa Rica')
reg_ipc_gasto_cr <- lm(ipc~gasto_dolares, cd_costa_rica)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 4.674
# IPC más sensible al gasto


# Para Mexico

cd_mexico <- clean_data %>% filter(Country == 'Mexico')
reg_ipc_gasto_mex <- lm(ipc~gasto_dolares, cd_mexico)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.1574

# En un gráfico veremos más a detalle el tema del COVID


# Parte 3: Gráficos -------------------------------------------------------
# clean_data %>%
#   ggplot(aes(Ano, gasto_dolares, color = as.factor(Country), group = 1)) + 
#   geom_line() + ggtitle("Gasto (%PIB) de los Paises OCDE en LATAM")


# Para Chile

scatter_cl <-clean_data %>% 
  filter(Country == 'Chile')

rownames(scatter_cl) <-scatter_cl$Ano

scatterHist(scatter_cl[c("gasto_dolares", "ipc")])

# Para Colombia

scatter_col <-clean_data %>% 
  filter(Country == 'Colombia')

rownames(scatter_col) <-scatter_col$Ano

scatterHist(scatter_col[c("gasto_dolares", "ipc")])

# Para Costa Rica

scatter_cr <-clean_data %>% 
  filter(Country == 'Costa Rica')

rownames(scatter_cr) <-scatter_cr$Ano

scatterHist(scatter_cr[c("gasto_dolares", "ipc")])

# Para Mexico

scatter_mx <-clean_data %>% 
  filter(Country == 'Mexico')

rownames(scatter_mx) <-scatter_mx$Ano

scatterHist(scatter_mx[c("gasto_dolares", "ipc")])

# Mapa  -------------------------------------------------------------------
data(World)

dataWorld <- World %>% 
  mutate(Country = sovereignt) %>% 
  filter(continent == "South America" | continent == "North America") %>% 
  filter(!(Country %in% c("United States of America", "Canada", "Denmark"))) %>% 
  select(Country, geometry)  

a1 <- clean_data %>% 
  filter(Ano == 1990) %>% 
  mutate(gastopc = gasto_dolares / poblacion)

plotuno <- left_join(dataWorld, a1) 



tmap_mode("plot") 

  tm_shape(plotuno) +
  tm_borders(alpha = 0.4) + 
  tm_polygons("gastopc", style = 'quantile', n=5,
              title = "Gasto pc en USD", colorNA = NULL) +
  tm_layout(main.title = "Gasto per cápita en 1990",
            main.title.position = "center",
            main.title.size = 1)
