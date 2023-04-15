library(ggplot2)
library(tidyverse)
library(ggthemes)
library(MetBrewer)
library(ggplot2)
library(googlesheets4)

#### Quaest ####

quaest_2 <- read_sheet("https://docs.google.com/spreadsheets/d/1ma1BonjBPOs5dvNgdNgk5JNf_FdCSheelnezjTZEt3g/edit#gid=859142516")


quaest_2 %>%
  ggplot()+
  aes(x = data, y = amostra, group = renda, col = renda)+
  geom_line(size = 1.2)+
  geom_point(size = 2.8)+
  theme_fivethirtyeight()+
  scale_color_met_d(name ="Demuth")+
  labs(col = "",
       title = "Composicão da amostra das pesquisas Quaest por renda")

#### Ipec ####

ipec <- read_sheet("https://docs.google.com/spreadsheets/d/1ma1BonjBPOs5dvNgdNgk5JNf_FdCSheelnezjTZEt3g/edit#gid=859142516", sheet = 2)

ipec %>%
  ggplot()+
  aes(x = data, y = amostra, group = renda, col = renda)+
  geom_line(size = 1.2)+
  geom_point(size = 2.8)+
  theme_fivethirtyeight()+
  scale_color_met_d(name ="Demuth")+
  labs(col = "",
       title = "Composicão da amostra das pesquisas Ipec por renda")

#### pesquisas pré eleições do datafolha e resultados no 1º turno ####


datafolha <- read_sheet("https://docs.google.com/spreadsheets/d/1m0D8YJokXbuCeEWbz5GZ0t8MSFmrdG2T1f8iD-xOrQ8/edit#gid=1024581996",
                        sheet = "dados_organizados")



datafolha %>%
  ggplot() +
  aes(x = Tipo, y = Percentuais, col = Colocacao, group = Colocacao)+
  geom_line(stat = "identity", size = 1.5)+
  geom_point(size = 3.5 )+
  facet_grid(~Ano)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  scale_color_met_d(name = "Degas")+
  labs(x = "",
       col = "",
       y = "",
       caption = "Núcleo de Estudos em Representação e Democracia - UENF",
       title = "Pesquisas pré eleições do DataFolha e Resultados no 1º Turno")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))



lapop_total <- bind_rows(lapop_2007,
                         lapop_2008,
                         lapop_2010,
                         lapop_2012,
                         lapop_2014,
                         lapop_2017,
                         lapop_2019,
                         lapop_2021)



lapop_2007 <- rename(lapop_2007, idio2 = IDIO2)


cores <- c("#E69F00", "#56B4E9", "#009E73")

ggplot(data = lapop_total, aes(x = ano, y = percentual, color = as.factor(idio2), group = idio2)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_manual(values = cores) +
  labs(title = "Variação das respostas ao longo dos anos",
       x = "Ano", y = "Percentual", color = "Resposta")+
  theme_minimal()
