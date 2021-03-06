# 1. Separar as vari�veis que entram na defini��o do �ndice ESG.
# 2. As demais vari�veis viram controles no modelo de painel de efeitos fixos.
# 3. Obter as tr�s primeiras PCs.
# 4. Olhar os scores das vari�veis originais (sem controles) nas PCs.
# 5. Para cada PC, ficar apenas com as vari�veis com maior score positivo.
# 6. Elevar ao quadro dos scores das vari�veis que entraram e zerar os scores das demais.
# 7. Isso define os sub�ndices G, S e E (checar ordem das componentes).
# 8. Definir o �ndice final ESG ponderando G, S e E pelo percentual da vari�ncia explicada pela PC correspondente.

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

panel_data <- scale(-panel_data, center = TRUE, scale = TRUE)

pca <- prcomp(panel_data)

## Scree plot

scree_plot <- fviz_eig(pca, addlabels = TRUE, barfill = "#0088C0", ggtheme = theme_classic()) +  # scree plot
                xlab("Componente Principal") +
                ylab("Propor��o Explicada da Vari�ncia") 

### Quantas PCs pegar

kaiser_criteria <- get_eigenvalue(pca) # Apenas se eigenvalue > 1
kaiser_criteria["count"] <- c(1:14)

# View(kaiser_criteria)

ggplot(data = kaiser_criteria, aes(x = count, y= eigenvalue)) +
  geom_bar(stat = "identity", color = "#0088C0", fill = "#0088C0") +
  theme_classic() +
  xlab("Principal Component") +
  ylab("Eigenvalue")+
  geom_line(aes(y = 1, x = c(0:13)), color = "red", linetype = "dashed")


## Cargas

Phi <- pca$rotation

# View(Phi)

## Contribui��es para as PCs

pc1 <- pca %>% fviz_contrib(choice = "var", axes = 1, sort.val = "asc", fill = "#0088C0", color = "black", ggtheme = theme_classic()) + 
        labs(x = "", title = "Contribui��ess das vari�veis para a PC1") + 
        coord_flip()

pc2 <- pca %>% fviz_contrib(choice = "var", axes = 2, sort.val = "asc", fill = "#0088C0", color = "black", ggtheme = theme_classic()) + 
        labs(x = "", title = "Contribui��ess das vari�veis para a PC2") + 
        coord_flip()

pc3 <- pca %>% fviz_contrib(choice = "var", axes = 3, sort.val = "asc", fill = "#0088C0", color = "black", ggtheme = theme_classic()) + 
        labs(x = "", title = "Contribui��ess das vari�veis para a PC3") + 
        coord_flip()

# Multiplicando pelos pesos

threshold <- 100 / ncol(panel_data)
colunas <- nrow(pc1[["data"]])

## Vetores dos pesos e nomes

### Para PC1

t = 1

pesos1 <- vector()
nomes1 <- vector()

for (i in 1:colunas) {
  if (pc1[["data"]][[i,2]] >= threshold) {
    
    pesos1[t] <- Phi[rownames(pc1[["data"]])[[i]], 1]
    nomes1[t] <- rownames(pc1[["data"]])[[i]]
    t = t + 1
    
  }
}

### Para PC2

t = 1

pesos2 <- vector()
nomes2 <- vector()

for (i in 1:colunas) {
  if (pc2[["data"]][[i,2]] >= threshold) {
    
    pesos2[t] <-Phi[rownames(pc1[["data"]])[[i]], 2]
    nomes2[t] <- rownames(pc2[["data"]])[[i]]
    t = t + 1
    
  }
}

### Para PC3

t = 1

pesos3 <- vector()
nomes3 <- vector()

for (i in 1:colunas) {
  if (pc3[["data"]][[i,2]] >= threshold) {
    
    pesos3[t] <- Phi[rownames(pc1[["data"]])[[i]], 3]
    nomes3[t] <- rownames(pc3[["data"]])[[i]]
    t = t + 1
    
  }
}

## Soma produto

linhas <- nrow(panel_data)

### Para PC1

m <- length(nomes1)
gov_ <- vector()
gov <- vector()

for (j in 1:linhas) {
  for (k in 1:m) {
    for (i in 1:colunas) {
      if (nomes1[k] == colnames(panel_data)[i]) {
        
        gov_[k] <- (pesos1[k]) * panel_data[j, i]

      } 
    }
  }
  
  gov[j] <- sum(gov_)
  
}
  
### Para PC2

m <- length(nomes2)
soc_ <- vector()
soc <- vector()

for (j in 1:linhas) {
  for (k in 1:m) {
    for (i in 1:colunas) {
      if (nomes2[k] == colnames(panel_data)[i]) {
        
        soc_[k] <- (pesos2[k]) * panel_data[j, i]
        
      } 
    }
  }
  
  soc[j] <- sum(soc_)
  
}

### Para PC3

m <- length(nomes3)
env_ <- vector()
env <- vector()

for (j in 1:linhas) {
  for (k in 1:m) {
    for (i in 1:colunas) {
      if (nomes3[k] == colnames(panel_data)[i]) {
        
        env_[k] <- (pesos3[k]) * panel_data[j, i]
        
      } 
    }
  }
  
  env[j] <- sum(env_)
  
}

# Inserindo de volta no dataframe

E_S_G <- panel_data

E_S_G <- cbind(E_S_G, Governance = gov) # Invertendo o sinal
E_S_G <- cbind(E_S_G, Social = soc)
E_S_G <- cbind(E_S_G, Environmental = env)

# Criando ESG

## Pesos

variance <- c(scree_plot[["data"]][[1,2]],
              scree_plot[["data"]][[2,2]],
              scree_plot[["data"]][[3,2]])

total <- sum(variance)

proportion <- c(scree_plot[["data"]][[1,2]]/total,
                scree_plot[["data"]][[2,2]]/total,
                scree_plot[["data"]][[3,2]]/total)

variance_exp <- data.frame(proportion)

## M�dia ponderada

ESGI <- vector()

for (i in 1:linhas) {
  ESGI[i] <- E_S_G[i, "Governance"] * variance_exp[1,1] +
            E_S_G[i, "Social"] * variance_exp[2,1] +
            E_S_G[i, "Environmental"] * variance_exp[3,1]
}


ESG <- E_S_G
ESG <- cbind(ESG, ESGI = ESGI)
ESG <- -ESG

# Ajustando as escalas dos �ndices

Gp <- vector()
Ep <- vector()
Sp <- vector()
ESGIp <- vector()

for (i in 1:linhas) {
  
  Gp[i] <- (ESG[i, "Governance"] - min(ESG[,"Governance"])) * 100 / (max(ESG[,"Governance"]) - min(ESG[,"Governance"]))
  Ep[i] <- (ESG[i, "Environmental"] - min(ESG[,"Environmental"])) * 100 / (max(ESG[,"Environmental"]) - min(ESG[,"Environmental"]))
  Sp[i] <- (ESG[i, "Social"] - min(ESG[,"Social"])) * 100 / (max(ESG[,"Social"]) - min(ESG[,"Social"]))
  ESGIp[i] <- (ESG[i, "ESGI"] - min(ESG[,"ESGI"])) * 100 / (max(ESG[,"ESGI"]) - min(ESG[,"ESGI"]))
    
}

ESG <- cbind(ESG, Gp = Gp, Ep = Ep, Sp = Sp, ESGIp = ESGIp)

ESG_indexes <- ESG[,c("Gp", "Ep", "Sp", "ESGIp")]

#write.csv(ESG_indexes, "Bases/ESG_PCA_indexes.csv")
