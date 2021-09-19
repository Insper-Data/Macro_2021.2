rm(list=ls())

setwd("~/Insper/Data/Projeto Macro/Data_Macro_ESG/Macro_2021.2")
data <- read.csv("test.csv")

teste <- data %>% 
  rename(Development = develop) %>% 
  group_by(Development, Time) %>% 
  summarise(ESG = mean(ESGI)) %>% 
  ggplot(aes(x = Time, y = ESG, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AV" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "ESG", title = "", subtitle = "") +
  ylim(-2, -1) +
  theme_bw()

teste

