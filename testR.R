rm(list=ls())

library(tidyverse)

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")
data <- read.csv("test4.csv")

teste <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(ESG = mean(ESG_Index)) %>% 
  ggplot(aes(x = Time, y = ESG, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

teste

library(readxl)
data2 <- read_excel("Bases/ESG_database - Copy.xlsx")

teste2 <- data2 %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(ESG = mean(`Access to electricity (% of population)`)) %>% 
  ggplot(aes(x = Time, y = ESG, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "ESG", title = "", subtitle = "") +
  theme_bw()

teste2

par(mfrow = c(1,2))
teste
teste2
