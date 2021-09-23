rm(list=ls())

library(tidyverse)


setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")
data <- read.csv("ESG_scale.csv")

ESG <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(ESG = mean(ESGIp)) %>% 
  ggplot(aes(x = Time, y = ESG, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

ESG

Env <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(Environmental = mean(Ep)) %>% 
  ggplot(aes(x = Time, y = Environmental, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

Env

Soc <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(Social = mean(Sp)) %>% 
  ggplot(aes(x = Time, y = Social, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

Soc

Gov <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(Governance = mean(Gp)) %>% 
  ggplot(aes(x = Time, y = Governance, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

Soc

Gov <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(Governance = mean(Gp)) %>% 
  ggplot(aes(x = Time, y = Governance, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Index", title = "", subtitle = "") +
  theme_bw()

###

library(gganimate)
library(ggplot2)
library(dplyr)
library(gapminder)
library(ggthemes)
library(gifski)
library(readr)
library(tidyr)

graph1 = data %>%
  ggplot(aes(x=ESGIp, y=Spread, color=develop)) +
  geom_point(alpha = 0.7, stroke = 0, size = 2.66) +
  scale_size(range=c(2,12), guide="none") +
  labs(title = "Spread x ESG",
       x = "ESG",
       y = "Spread (USA)",
       color = "Development") +
  theme(axis.title = element_text(),
        text = element_text(family = "Rubik"),
        legend.text=element_text(size=10)) +
  scale_color_manual(values = c("EM" = "#B80000", "AV" = "#0887C0")) +
  ylim(-4,15) +
  geom_smooth(method = lm, se = FALSE)
  
graph1

graph1.animation = graph1 +
  transition_time(Time) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_null()

animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 14,
        end_pause = 60, res = 100)
anim_save("graph4.gif")



