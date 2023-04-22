##### importar bancos #####
library(haven)
X7948266051039660950Brazil_LAPOP_AmericasBarometer_2010_data_set_approved_v4 <- read_dta("bancos/7948266051039660950Brazil_LAPOP_AmericasBarometer 2010 data set approved v4.dta")
X54861031Brazil_LAPOP_AmericasBarometer_2012_Rev1_W <- read_dta("bancos/54861031Brazil LAPOP AmericasBarometer 2012 Rev1_W.dta")
X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W <- read_dta("bancos/636339374Brazil LAPOP AmericasBarometer 2014 v3.0_W.dta")
X780314464Brazil_LAPOP_AmericasBarometer_2017_V1_0_W <- read_dta("bancos/780314464Brazil LAPOP AmericasBarometer 2017 V1.0_W.dta")
Brazil_LAPOP_AmericasBarometer_2019_v1_0_W <- read_dta("bancos/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
BRA_2021_LAPOP_AmericasBarometer_v1_2_w <- read_dta("bancos/BRA_2021_LAPOP_AmericasBarometer_v1.2_w.dta")

##### Lapop - variável jc14a ####

lapop_jc15a_2010 <- X7948266051039660950Brazil_LAPOP_AmericasBarometer_2010_data_set_approved_v4 %>%
  mutate(ano="2010") %>%
  group_by(ano, q1, jc15a) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc15a, percentual_resp_genero) %>%
  remove_labels()
lapop_jc15a_2010 <- lapop_jc15a_2010[-2,]
    lapop_jc15a_2010 <- lapop_jc15a_2010[-3,]
save(lapop_jc15a_2010,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2010.Rda")


lapop_jc15a_2012 <- X54861031Brazil_LAPOP_AmericasBarometer_2012_Rev1_W %>%
      mutate(ano="2012") %>%
      group_by(ano, q1, jc15a) %>%
      summarise(resp_genero = n()) %>%
      group_by(ano, q1) %>% na.omit() %>%
      mutate(total_Q1 = sum(resp_genero),
             percentual_resp_genero = resp_genero/total_Q1*100) %>%
      select(ano, q1, resp_genero, jc15a, percentual_resp_genero) %>%
      remove_labels()
lapop_jc15a_2012 <- lapop_jc15a_2012[-2,]
lapop_jc15a_2012 <- lapop_jc15a_2012[-3,]
save(lapop_jc15a_2012,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2012.Rda")


lapop_jc15a_2014 <- X636339374Brazil_LAPOP_AmericasBarometer_2014_v3_0_W %>%
  mutate(ano="2014") %>%
  group_by(ano, q1, jc15a) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc15a, percentual_resp_genero) %>%
  remove_labels()
lapop_jc15a_2014 <- lapop_jc15a_2014[-2,]
lapop_jc15a_2014 <- lapop_jc15a_2014[-3,]
save(lapop_jc15a_2014,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2014.Rda")



lapop_jc15a_2016 <- X780314464Brazil_LAPOP_AmericasBarometer_2017_V1_0_W %>%
  mutate(ano="2016") %>%
  group_by(ano, q1, jc15a) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc15a, percentual_resp_genero) %>%
  remove_labels()
lapop_jc15a_2016 <- lapop_jc15a_2016[-2,]
lapop_jc15a_2016 <- lapop_jc15a_2016[-3,]
save(lapop_jc15a_2016,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2016.Rda")


lapop_jc15a_2018 <- Brazil_LAPOP_AmericasBarometer_2019_v1_0_W %>%
  mutate(ano="2018") %>%
  group_by(ano, q1, jc15a) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1, resp_genero, jc15a, percentual_resp_genero) %>%
  remove_labels()
lapop_jc15a_2018 <- lapop_jc15a_2018[-2,]
lapop_jc15a_2018 <- lapop_jc15a_2018[-3,]
save(lapop_jc15a_2018,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2018.Rda")


lapop_jc15a_2021 <- BRA_2021_LAPOP_AmericasBarometer_v1_2_w %>%
  mutate(ano="2021") %>%
  group_by(ano, q1tb, jc15a) %>%
  summarise(resp_genero = n()) %>%
  group_by(ano, q1tb) %>% na.omit() %>%
  mutate(total_Q1 = sum(resp_genero),
         percentual_resp_genero = resp_genero/total_Q1*100) %>%
  select(ano, q1tb, resp_genero, jc15a, percentual_resp_genero) %>%
  remove_labels()

lapop_jc15a_2021 <- rename(lapop_jc15a_2021, q1 = q1tb)
lapop_jc15a_2021 <- lapop_jc15a_2021[-5,]
lapop_jc15a_2021 <- lapop_jc15a_2021[-2,]
lapop_jc15a_2021 <- lapop_jc15a_2021[-3,]

save(lapop_jc15a_2021,
     file = "C:/Users/davia/Desktop/relatorio_davi/bancos_lapop/variaveis_lapop/lapop_jc15a_2021.Rda")



##### unificar bancos #####

lapop_jc15a <- bind_rows(lapop_jc15a_2010,
                        lapop_jc15a_2012,
                        lapop_jc15a_2014,
                        lapop_jc15a_2016,
                        lapop_jc15a_2018,
                        lapop_jc15a_2021)

##### gráfico jc15a #####

ggplot(data = lapop_jc15a, aes(x = ano, y = percentual_resp_genero, group = q1)) +
  geom_line(aes(linetype = factor(q1)), size = 1.1)+
  scale_linetype_manual(values = c("solid", "dotted"),
                        name = "Linhas",
                        labels = c("Homens", "Mulheres"))+
  labs(x = "Ano", y = "Percentual", color = "Opções",)+
  theme_bw()+
  theme(plot.background = element_rect(fill = "white"),  # cor de fundo
        axis.line = element_line(color = "black"),  # cor dos eixos
        axis.text = element_text(size = 12),  # tamanho da fonte dos rótulos dos eixos
        axis.title = element_text(size = 14),  # tamanho da fonte dos títulos dos eixos
        plot.title = element_text(size = 16),  # tamanho da fonte do título do gráfico
        legend.position = "bottom"  # posição da legenda
  )
X1_01838 <- read_sav("bancos_eseb/1_01838.sav")
View(X1_01838)

