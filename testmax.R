setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

max_esg <- read_excel("max_esg.xlsx")

govv <- vector()

for (i in 1:length(nomes1)) {
  for (k in 1:ncol(max_esg)) {
    if (nomes1[i] == colnames(max_esg)[k]) {
      
      govv[i] <- pesos1[i] * max_esg[2, k]
      
    }
  }
}

gov_max <- sum(data.frame(govv))

socc <- vector()

for (i in 1:length(nomes2)) {
  for (k in 1:ncol(max_esg)) {
    if (nomes2[i] == colnames(max_esg)[k]) {
      
      socc[i] <- pesos2[i] * max_esg[2, k]
      
    }
  }
}

soc_max <- sum(data.frame(socc))

envv <- vector()

for (i in 1:length(nomes3)) {
  for (k in 1:ncol(max_esg)) {
    if (nomes3[i] == colnames(max_esg)[k]) {
      
      envv[i] <- pesos3[i] * max_esg[2, k]
      
    }
  }
}

env_max <- sum(data.frame(envv))

esg_max <- gov_max*variance_exp[1,1] +
            soc_max*variance_exp[2,1] +
            env_max*variance_exp[3,1]

print(esg_max)





