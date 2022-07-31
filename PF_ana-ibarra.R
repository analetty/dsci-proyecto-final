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
library(scales)


# Parte 1: Importar datos -------------------------------------------------

# Cambiamos al directorio donde tenemos los datasets

getwd()
# Importamos los datasets a usar

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
  transform(Country = as.factor(Country),
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


clean_data_pob <- data.frame(Country = factor(),
                       Ano = double(),
                       ppp = numeric(),
                       ipc = numeric(),
                       poblacion = numeric(),
                       gasto_ml =numeric(),
                       gasto_dolares =numeric(),
                       tasa_crecpob = numeric())

for(i in seq_along(levels(clean_data$Country))){
  tasa_pob_pais <- clean_data %>% filter(Country %in% c(levels(Country)[i])) %>% 
    mutate(tasa_crecpob = (poblacion/lag(poblacion))-1)
  
  clean_data_pob <-bind_rows(clean_data_pob, tasa_pob_pais)
}

# Parte 2: Análisis -------------------------------------------------------

# Regresión mínimos cuadrados ordinarios

# Se desea conocer cómo el gasto explica el ipc

reg_ipc_gasto <- lm(ipc~gasto_dolares, clean_data)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.06546

# Para Chile
lm1 <- clean_data %>% filter(Country == 'Chile') %>% 
  lm(ipc~gasto_dolares, data =.)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.5658


# Para Colombia

lm2 <- clean_data %>% filter(Country == 'Colombia') %>% 
  lm(ipc~gasto_dolares, data =.)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.3984



# Para Costa Rica

lm3 <- clean_data %>% filter(Country == 'Costa Rica') %>% 
  lm(ipc~gasto_dolares, data =.)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 4.674
# IPC más sensible al gasto


# Para Mexico
lm4 <- clean_data %>% filter(Country == 'Mexico') %>% 
  lm(ipc~gasto_dolares, data =.)

# Un incremento de 1 unidad (mil millones de dolares) implica un aumento del ipc del 0.1574

# Tabla de regresiones
msummary(list("Regional"= reg_ipc_gasto,
              "Chile"= lm1,
              "Colombia"=lm2, 
              "Costa Rica"= lm3, 
              "México" = lm4), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))
           
# En un gráfico veremos más a detalle el tema del COVID


# Parte 3: Gráficos -------------------------------------------------------

gasto_grf <- clean_data %>%
  ggplot(aes(Ano, gasto_dolares, color = Country)) + 
  geom_line() + ggtitle("Gasto de los Paises OCDE en LATAM") +
  geom_vline(xintercept = 2020)

# Para Chile

clean_data %>% 
  filter(Country == 'Chile') %>% 
  `rownames<-`(.$Ano) %>% 
  select(gasto_dolares, ipc) %>% 
  scatterHist() + 
  ggtitle("Resumen descriptivo Gasto-IPC en Chile") 

clean_data %>%
  filter(Country == 'Chile') %>% 
  ggplot(aes(Ano, gasto_dolares, color = Country)) + 
  geom_line() + ggtitle("Comportamiento del Gasto en Chile") +
  geom_vline(xintercept = 2009) + 
  xlab("Año") + ylab("Gasto (Mil Millones de USD)")

# Para Colombia

clean_data %>% 
  filter(Country == 'Colombia') %>% 
  `rownames<-`(.$Ano) %>% 
  select(gasto_dolares, ipc) %>% 
  scatterHist() + 
  ggtitle("Resumen descriptivo Gasto-IPC en Colombia")

clean_data %>%
  filter(Country == 'Colombia') %>% 
  ggplot(aes(Ano, gasto_dolares, color = Country)) + 
  geom_line() + ggtitle("Comportamiento del Gasto en Colombia") +
  geom_vline(xintercept = 2013) + 
  xlab("Año") + ylab("Gasto (Mil Millones de USD)") + labs(caption = "Nota: el año corresponde a la aprobación de la adhesion y hoja de ruta")

# Para Costa Rica

clean_data %>% 
  filter(Country == 'Costa Rica') %>% 
  `rownames<-`(.$Ano) %>% 
  select(gasto_dolares, ipc) %>% 
  scatterHist() + 
  ggtitle("Resumen descriptivo Gasto-IPC en Costa Rica")

clean_data %>%
  filter(Country == 'Costa Rica') %>% 
  ggplot(aes(Ano, gasto_dolares, color = Country)) + 
  geom_line() + ggtitle("Comportamiento del Gasto en Costa Rica") +
  geom_vline(xintercept = 2015) + 
  xlab("Año") + ylab("Gasto (Mil Millones de USD)") + labs(caption = "Nota: el año corresponde a la aprobación de la adhesion y hoja de ruta")


# Para Mexico

clean_data %>% 
  filter(Country == 'Mexico') %>% 
  `rownames<-`(.$Ano) %>% 
  select(gasto_dolares, ipc) %>% 
  scatterHist() + 
  ggtitle("Resumen descriptivo Gasto-IPC en México")

clean_data %>%
  filter(Country == 'Mexico') %>% 
  ggplot(aes(Ano, gasto_dolares, color = Country)) + 
  geom_line() + ggtitle("Comportamiento del Gasto en Mexico") +
  geom_vline(xintercept = 1994) + 
  xlab("Año") + ylab("Gasto (Mil Millones de USD)")

# Mapa  -------------------------------------------------------------------
data(World)

dataWorld <- World %>% 
  mutate(Country = sovereignt) %>% 
  filter(continent == "South America" | continent == "North America") %>% 
  filter(!(Country %in% c("United States of America", "Canada", "Denmark"))) %>% 
  select(Country, geometry)  

a1 <- clean_data %>% 
  filter(Ano == 1990) %>% 
  mutate(gastopc = gasto_dolares / poblacion * 10^3)

plotuno <- left_join(dataWorld, a1) 



tmap_mode("plot") 

  p1 <- tm_shape(plotuno) +
  tm_borders(alpha = 0.4) + 
  tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
              title = "Gasto pc en USD", colorNA = NULL) +
  tm_layout(main.title = "1990",
            main.title.position = "center",
            main.title.size = 3, 
            legend.show = F, 
            frame = F)

  a2 <- clean_data %>% 
    filter(Ano == 1995) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotdos <- left_join(dataWorld, a2) 
  
  
  
  tmap_mode("plot") 
  
  p2 <- tm_shape(plotdos) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "1995",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  a3 <- clean_data %>% 
    filter(Ano == 2000) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plottres <- left_join(dataWorld, a3) 
  
  
  
  tmap_mode("plot") 
  
  p3 <- tm_shape(plottres) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2000",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  
  a4 <- clean_data %>% 
    filter(Ano == 2005) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotcuatro <- left_join(dataWorld, a4) 
  
  
  
  tmap_mode("plot") 
  
  p4 <- tm_shape(plotcuatro) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2005",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  a5 <- clean_data %>% 
    filter(Ano == 2010) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotcinco <- left_join(dataWorld, a5) 
  
  
  
  tmap_mode("plot") 
  
  p5 <- tm_shape(plotcinco) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2010",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  a6 <- clean_data %>% 
    filter(Ano == 2015) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotseis <- left_join(dataWorld, a6) 
  
  
  
  tmap_mode("plot") 
  
  p6 <- tm_shape(plotseis) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2015",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  
  a7 <- clean_data %>% 
    filter(Ano == 2020) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotsiete <- left_join(dataWorld, a7) 
  
  
  
  tmap_mode("plot") 
  
  p7 <- tm_shape(plotsiete) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2020",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
  
  a8 <- clean_data %>% 
    filter(Ano == 2021) %>% 
    mutate(gastopc = gasto_dolares / poblacion * 10^3)
  
  plotocho <- left_join(dataWorld, a8) 
  
  
  
  tmap_mode("plot") 
  
  p8 <- tm_shape(plotocho) +
    tm_borders(alpha = 0.4) + 
    tm_fill("gastopc", breaks= c(800, 1300, 1800, 2300, 2800,3300, 3800, 4300, 4800, 5300, 5800, 6300, 6800, 7300, 7800, 8300, 8800),
            title = "Gasto pc en USD", colorNA = NULL) +
    tm_layout(main.title = "2021",
              main.title.position = "center",
              main.title.size = 3, 
              legend.show = F, 
              frame = F)
