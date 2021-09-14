setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list=ls())

library(readxl)
library(tidyverse)
library(factoextra)
library(ggrepel)
library(plm)
library(dplyr)

# Extraindo e arrumando os dados

data <- read_excel("Bases/ESG_database.xlsx")
data <- data[,-c(2)]

panel_data <- pdata.frame(data, index = c("Country.Name", "Time"))
panel_data <- panel_data[,-c(1,2)]

# PCA

## Ajustando escala dos dados
## Inverter o sinal para interpretabilidade

pca <- -panel_data %>%
  prcomp(scale = TRUE)

## Scree plot

fviz_eig(pca, addlabels = TRUE) +  # scree plot
  xlab("Componente Principal") +
  ylab("Proporção Explicada da Variância")

## Cargas

Phi <- pca$rotation

# View(Phi)

## Contribuições para as PCs

pca %>% fviz_contrib(choice = "var", axes = 1, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC1") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 2, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC2") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 3, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC3") + 
  coord_flip()



