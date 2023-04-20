##### pacotes #####
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(MetBrewer)
library(googlesheets4)
library(labelled)
library(dplyr)

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

##### LAPOP: bancos, comecando pela IDIO2 ####

library(haven)
X2138048899brazil_lapop_dims_final_2007_v5 <- read_dta("bancos/2138048899brazil_lapop_dims final 2007 v5.dta")


lapop_2006 <- X2138048899brazil_lapop_dims_final_2007_v5  %>% rename(idio2=IDIO2) %>%
  select(idio2) %>%  mutate(ano = "2006") %>%
  group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>%
  na.omit()

X30541815brazil_lapop_dims_2008_final_data_set_v10 <- read_dta("bancos/30541815brazil_lapop_dims_2008_final_data_set_v10.dta")
View(X30541815brazil_lapop_dims_2008_final_data_set_v10)

lapop_2008 <- X30541815brazil_lapop_dims_2008_final_data_set_v10 %>%
  select(idio2) %>% mutate(ano = "2008") %>% group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>%
  na.omit()

X7948266051039660950Brazil_LAPOP_AmericasBarometer_2010_data_set_approved_v4 <- read_dta("bancos/7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set approved v4.dta")

lapop_2010 <- X7948266051039660950Brazil_LAPOP_AmericasBarometer_2010_data_set_approved_v4 %>%
  select(idio2) %>% mutate(ano = "2010") %>% group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

X54861031Brazil_LAPOP_AmericasBarometer_2012_Rev1_W <- read_dta("bancos/54861031Brazil LAPOP AmericasBarometer 2012 Rev1_W.dta")

lapop_2012 <- X54861031Brazil_LAPOP_AmericasBarometer_2012_Rev1_W %>%
  select(idio2) %>% mutate(ano = "2012") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W <- read_dta("bancos/636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")

lapop_2014 <- X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W %>%
  select(idio2) %>% mutate(ano = "2014") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

X780314464Brazil_LAPOP_AmericasBarometer_2017_V1_0_W <- read_dta("bancos/780314464Brazil LAPOP AmericasBarometer 2017 V1.0_W.dta")

lapop_2016 <- X780314464Brazil_LAPOP_AmericasBarometer_2017_V1_0_W %>%
  select(idio2) %>% mutate(ano = "2016") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

Brazil_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("bancos/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")

lapop_2018 <- Brazil_LAPOP_AmericasBarometer_2019_v1_0_W %>%
  select(idio2) %>% mutate(ano = "2018") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

BRA_2021_LAPOP_AmericasBarometer_v1_2_w <- read_dta("bancos/BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

lapop_2021 <- BRA_2021_LAPOP_AmericasBarometer_v1_2_w %>%
  select(idio2) %>% mutate(ano = "2021") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

##### salvar bancos ####

save(lapop_2007,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2007.Rda")
save(lapop_2008,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2008.Rda")
save(lapop_2010,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2010.Rda")
save(lapop_2012,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2012.Rda")
save(lapop_2014,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2014.Rda")
save(lapop_2017,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2017.Rda")
save(lapop_2019,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2019.Rda")
save(lapop_2021,
     file = "~/R/relatorio_davi/bancos/idio2_lapop2021.Rda")

###### bancos unificados #####

lapop_total <- bind_rows(lapop_2006,
                         lapop_2008,
                         lapop_2010,
                         lapop_2012,
                         lapop_2014,
                         lapop_2016,
                         lapop_2018,
                         lapop_2021)

##### gráfico variação idio2 #####

cores <- c("blue4", "aquamarine4", "darkgoldenrod")

ggplot(data = lapop_total, aes(x = ano, y = percentual, color = as.factor(idio2), group = idio2)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_manual(values = cores) +
  labs(title = "Variação das respostas ao longo dos anos",
       x = "Ano", y = "Percentual", color = "Resposta")+
  theme_bw()+
  theme(legend.spacing.y = unit(0.5, "cm"))



####### variável jc13 #####

lapop_jc13_2006 <- X2138048899brazil_lapop_dims_final_2007_v5 %>%
  mutate(ano="2006") %>%
  group_by(ano, Q1, JC13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, Q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, Q1, resp_genero, JC13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2006 <- lapop_jc13_2006[-2,]
lapop_jc13_2006 <- lapop_jc13_2006[-3,]

lapop_jc13_2006 <- rename(lapop_jc13_2006, q1 = Q1)
lapop_jc13_2006 <- rename(lapop_jc13_2006, jc13 = JC13)

save(lapop_jc13_2006,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2006.Rda")

lapop_jc13_2008 <- X30541815brazil_lapop_dims_2008_final_data_set_v10 %>%
  mutate(ano="2008") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2008 <- lapop_jc13_2008[-2,]
lapop_jc13_2008 <- lapop_jc13_2008[-3,]

save(lapop_jc13_2008,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2008.Rda")

lapop_jc13_2010 <- X7948266051039660950Brazil_LAPOP_AmericasBarometer_2010_data_set_approved_v4 %>%
  mutate(ano="2010") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2010 <- lapop_jc13_2010[-2,]
lapop_jc13_2010 <- lapop_jc13_2010[-3,]

save(lapop_jc13_2010,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2010.Rda")

lapop_jc13_2012 <- X54861031Brazil_LAPOP_AmericasBarometer_2012_Rev1_W %>%
  mutate(ano="2012") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2012 <- lapop_jc13_2012[-2,]
lapop_jc13_2012 <- lapop_jc13_2012[-3,]

save(lapop_jc13_2012,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2012.Rda")

lapop_jc13_2014 <- X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W %>%
  mutate(ano="2014") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2014 <- lapop_jc13_2014[-2,]
lapop_jc13_2014 <- lapop_jc13_2014[-3,]

save(lapop_jc13_2014,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2014.Rda")

lapop_jc13_2016 <- X780314464Brazil_LAPOP_AmericasBarometer_2017_V1_0_W %>%
  mutate(ano="2016") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2016 <- lapop_jc13_2016[-2,]
lapop_jc13_2016 <- lapop_jc13_2016[-3,]

save(lapop_jc13_2016,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2016.Rda")

lapop_jc13_2018 <- Brazil_LAPOP_AmericasBarometer_2019_v1_0_W %>%
  mutate(ano="2018") %>%
  group_by(ano, q1, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2018 <- lapop_jc13_2018[-2,]
lapop_jc13_2018 <- lapop_jc13_2018[-3,]


save(lapop_jc13_2018,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2018.Rda")

lapop_jc13_2021 <- BRA_2021_LAPOP_AmericasBarometer_v1_2_w %>%
  mutate(ano="2021") %>%
  group_by(ano, q1tb, jc13) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1tb) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1tb, resp_genero, jc13, percentual_resp_genero) %>%
  remove_labels()
lapop_jc13_2021 <- lapop_jc13_2021[-5, ]
lapop_jc13_2021 <- rename(lapop_jc13_2021, q1 = q1tb)
lapop_jc13_2021 <- lapop_jc13_2021[-2,]
lapop_jc13_2021 <- lapop_jc13_2021[-3,]


save(lapop_jc13_2021,
     file = "~/R/relatorio_davi/bancos/lapop_jc13_2021.Rda")

###### unificar bancos ######

lapop_jc13 <- bind_rows(lapop_jc13_2006,
                        lapop_jc13_2008,
                        lapop_jc13_2010,
                        lapop_jc13_2012,
                        lapop_jc13_2014,
                        lapop_jc13_2016,
                        lapop_jc13_2018,
                        lapop_jc13_2021)

save(lapop_jc13,
     file = "~/R/relatorio_davi/bancos/lapop_jc13.Rda")
####### gráfico ######

  ggplot(data = lapop_jc13, aes(x = ano, y = percentual_resp_genero, color =q1, group = q1)) +
    geom_line(aes(linetype = factor(q1)), size = 1.1)+
    labs(title = "Variação de respaldo à golpe militar ao longo dos anos.",
         x = "Ano", y = "Percentual", color = "Opções",)+
    theme_bw() +
    theme(legend.spacing.y = unit(0.5, "cm"))

