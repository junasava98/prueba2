# Julieth Natalia Salazar Vargas 
# Mavelyn Sterling Londono
# Trabajo FINAL - Diseños

#### CASO 1

library(readxl)

CASO1= read_xlsx("/Users/mavelyn/Google Drive/ESTADÍSTICA/SEMESTRE 2019-2/DISEÑOS DE EXPERIMENTOS/trabajo final/caso1.xlsx")

## Convertir variables a factor
CASO1 <- within(CASO1, {
  Fertilizacion <- factor(Fertilizacion)
  Genotipo <- factor(Genotipo)
  Especie <- factor(Especie)
})
attach(CASO1)


###########  Descriptivas
library(ggplot2)
library(dplyr)


Descrip_Fertilizacion<- CASO1 %>%
  group_by(Fertilizacion) %>%
  summarise(mean_run = mean(Valor),
            sd_run=sd(Valor),
            coef_run = (sd(Valor)/mean(Valor))*100,
            min_run= min(Valor),
            quantile_1=quantile(Valor, probs = 0.25),
            median_run = median(Valor),
            quantile_3=quantile(Valor, probs = 0.75),
            max_run = max(Valor))


Descrip_Genotipo <- CASO1 %>%
  group_by(Genotipo) %>%
  summarise(mean_run = mean(Valor),
            sd_run=sd(Valor),
            coef_run = (sd(Valor)/mean(Valor))*100,
            min_run= min(Valor),
            quantile_1=quantile(Valor, probs = 0.25),
            median_run = median(Valor),
            quantile_3=quantile(Valor, probs = 0.75),
            max_run = max(Valor))

Descrip_Especie <- CASO1 %>%
  group_by(Especie) %>%
  summarise(mean_run = mean(Valor),
            sd_run=sd(Valor),
            coef_run = (sd(Valor)/mean(Valor))*100,
            min_run= min(Valor),
            quantile_1=quantile(Valor, probs = 0.25),
            median_run = median(Valor),
            quantile_3=quantile(Valor, probs = 0.75),
            max_run = max(Valor))


