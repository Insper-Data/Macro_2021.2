# PCA_Posicionamento.R

rm(list = ls())

options(digits = 2)

library(tidyverse)
library(factoextra)
library(ggrepel)

theme_set(theme_bw())

# a unidade amostral Ã© o par (respondente, marca avaliada)

library(readxl)
wb_data <- read_excel("~/Insper/Data/Projeto Macro/basepca2.xlsx")

View(wb_data)

result <- wb_data[-1]
row.names(result) <- wb_data$`Country Name`
 
wb_data <- result

wb_data <- wb_data[,-c(1)]

View(wb_data)

avaliacoes <- wb_data

pca <- avaliacoes %>%
  prcomp(scale = TRUE)

# proportion of variance explained
(pve <- cumsum(pca$sdev^2) / sum(pca$sdev^2))

fviz_eig(pca, addlabels = TRUE) +  # scree plot
  xlab("Componente Principal") +
  ylab("ProporÃ§Ã£o explicada da variÃ¢ncia")

# Kaiser, Henry F. 1961. "A Note on Guttman's Lower Bound for the Number of Common Factors." British Journal of Statistical Psychology 14: 1-2.
# An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the original variables in standardized data. This is commonly used as a cutoff point for which PCs are retained. This holds true only when the data are standardized.
get_eigenvalue(pca)[1:10, 1:2]
# equivalente a
(pca$sdev^2)[1:10]

Phi <- pca$rotation

# a contribuiÃ§Ã£o percentual de uma variÃ¡vel Ã© 100 * o quadrado
# da carga correspodente dividido pela soma dos quadrados das cargas
sort(100 * Phi[, 1]^2 / sum(Phi[, 1]^2), decreasing = TRUE)
# a linha tracejada seria o valor de uma contribuiÃ§Ã£o percentual
# igual para cada variÃ¡vel; ou seja, 100 / nÃºmero de variÃ¡veis

pca %>% fviz_contrib(choice = "var", axes = 1, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC1") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 2, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC2") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 3, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC3") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 4, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC4") + 
  coord_flip()

pca %>% fviz_contrib(choice = "var", axes = 5, sort.val = "asc", fill = "steelblue", color = "black") + 
  labs(x = "", title = "Contribuiçõess das variáveis para a PC5") + 
  coord_flip()
