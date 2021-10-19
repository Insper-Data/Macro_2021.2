setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")

rm(list=ls())

library(tidyverse)
library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(readr)
library(tidyr)
library(readxl)
library(plm)

# Combinando as bases de dados

ESG_PCA_indexes <- read.csv("Bases/ESG_PCA_indexes.csv")

spreads <- read_excel("Bases/yield_bonds.xlsx")

spreads$X <- paste(spreads$...1, "-", spreads$Ano)

spreads <- spreads[order(spreads$X),]

ESG_PCA_indexes$spreads <- spreads$`United States`

panel_past <- pdata.frame(read.csv("Bases/Datasets/dataset_total.csv"), c("country", "year"))

panel_past$X <- rownames(panel_past)

rownames(ESG_PCA_indexes) <- ESG_PCA_indexes$X

data <- left_join(ESG_PCA_indexes, panel_past, by = c("X" = "X"))

#write.csv(data, "Bases/merged.csv")

data <- data[, c("country", "year", "ESGIp", "Ep", "Gp", "spreads", "develop")]

# Spread ao longo dos anos

spread <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(Spread = mean(spreads)) %>% 
  ggplot(aes(x = year, y = Spread, color=Development, group = Development)) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Spread (USA)", title = "", subtitle = "") +
  theme_bw()

# ESG ao longo do tempo

ESG <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(ESG = mean(ESGIp)) %>% 
  ggplot(aes(x = year, y = ESG, color=Development, group = Development)) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

# Environmental ao longo do tempo

Env <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(Environmental = mean(Ep)) %>% 
  ggplot(aes(x = year, y = Environmental, color=Development, group = Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

#Social ao longo do tempo

Soc <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(Social = mean(Sp)) %>% 
  ggplot(aes(x = year, y = Social, color=Development, group = Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

# Governance ao longo do tempo

Gov <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(Governance = mean(Gp)) %>% 
  ggplot(aes(x = year, y = Governance, color=Development, group = Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

# Spread x ESG

graph1 = data %>%
  ggplot(aes(x=ESGIp, y=spreads, color=develop)) +
  geom_point(alpha = 0.7, stroke = 0, size = 2.66) +
  scale_size(range=c(2,12), guide="none") +
  labs(title = "Spread x ESG",
       x = "ESG",
       y = "Spread (USA)",
       color = "Development") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_manual(values = c("EM" = "#B80000", "AM" = "#0887C0")) +
  ylim(-4,15) +
  geom_smooth(method = lm, se = TRUE)
  
graph1

graph1.animation = graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_null()

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("graph5.gif")

# Spread x Environmental

graph2 = data %>%
  ggplot(aes(x=Ep, y=spreads, color=develop)) +
  geom_point(alpha = 0.7, stroke = 0, size = 2.66) +
  scale_size(range=c(2,12), guide="none") +
  labs(title = "Spread x Environmental",
       x = "Environmental",
       y = "Spread (USA)",
       color = "Development") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_manual(values = c("EM" = "#B80000", "AM" = "#0887C0")) +
  ylim(-4,15) +
  geom_smooth(method = lm, se = TRUE)

graph2

graph2.animation = graph2 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_null()

animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("graph6.gif")

# Spread x Social

graph3 = data %>%
  ggplot(aes(x=Sp, y=spreads, color=develop)) +
  geom_point(alpha = 0.7, stroke = 0, size = 2.66) +
  scale_size(range=c(2,12), guide="none") +
  labs(title = "Spread x Social",
       x = "Social",
       y = "Spread (USA)",
       color = "Development") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_manual(values = c("EM" = "#B80000", "AM" = "#0887C0")) +
  ylim(-4,15) +
  geom_smooth(method = lm, se = TRUE)

graph3

graph3.animation = graph3 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_null()

animate(graph3.animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("graph7.gif")

# Spread x Governance

graph4 = data %>%
  ggplot(aes(x=Gp, y=spreads, color=develop)) +
  geom_point(alpha = 0.7, stroke = 0, size = 2.66) +
  scale_size(range=c(2,12), guide="none") +
  labs(title = "Spread x Governance",
       x = "Governance",
       y = "Spread (USA)",
       color = "Development") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_manual(values = c("EM" = "#B80000", "AM" = "#0887C0")) +
  ylim(-4,15) +
  geom_smooth(method = lm, se = TRUE)

graph4

graph4.animation = graph4 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_null()

animate(graph4.animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)

anim_save("graph8.gif")





