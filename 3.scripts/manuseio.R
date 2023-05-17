# 1. Carregar pacotes -----
pacman::p_load(tidyverse, haven, ggplot2, ggthemes, MetBrewer, googlesheets4, labelled, dplyr)

# 2. Composicao da amostra de pesquisa, comparacao por mes ----
## 2.1. Pesquisa Quaest ----

quaest_2 <- read_sheet("https://docs.google.com/spreadsheets/d/1ma1BonjBPOs5dvNgdNgk5JNf_FdCSheelnezjTZEt3g/edit#gid=859142516")

quaest_2 %>%
  ggplot()+
  aes(x = data, y = amostra, group = renda, col = renda)+
  geom_line(size = 1.2)+
  geom_point(size = 2.8)+
  theme_bw()+
  scale_color_met_d(name ="Demuth")+
  labs(col = "",
       title = "Composicão da amostra das pesquisas Quaest por renda")

## 2.2. Pesquisa Ipec ----

ipec <- read_sheet("https://docs.google.com/spreadsheets/d/1ma1BonjBPOs5dvNgdNgk5JNf_FdCSheelnezjTZEt3g/edit#gid=859142516", sheet = 2)

ipec %>%
  ggplot()+
  aes(x = data, y = amostra, group = renda, col = renda)+
  geom_line(size = 1.2)+
  geom_point(size = 2.8)+
  theme_bw()+
  scale_color_met_d(name ="Demuth")+
  labs(col = "",
       title = "Composicão da amostra das pesquisas Ipec por renda")

# 3. pesquisas pré eleições e resultados no 1º turno ----
## 3.1. Datafolha ----

datafolha <- read_sheet("https://docs.google.com/spreadsheets/d/1m0D8YJokXbuCeEWbz5GZ0t8MSFmrdG2T1f8iD-xOrQ8/edit#gid=1024581996",
                        sheet = "dados_organizados_datafolha")

datafolha %>%
  ggplot() +
  aes(x = Tipo, y = Percentuais, col = Colocacao, group = Colocacao)+
  geom_line(stat = "identity", size = 1.5)+
  geom_point(size = 3.5 )+
  facet_grid(~Ano)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_met_d(name = "Degas")+
  labs(x = "",
       col = "",
       y = "",
       caption = "Núcleo de Estudos em Representação e Democracia - UENF",
       title = "Pesquisas pré eleições do DataFolha e Resultados no 1º Turno")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))
2
## 3.2. Ibope -----

ibope <- read_sheet("https://docs.google.com/spreadsheets/d/1m0D8YJokXbuCeEWbz5GZ0t8MSFmrdG2T1f8iD-xOrQ8/edit#gid=1024581996",
                        sheet = "dados_organizados_ibope")

ibope %>%
  ggplot() +
  aes(x = Tipo, y = Percentuais, col = Colocacao, group = Colocacao)+
  geom_line(stat = "identity", size = 1.5)+
  geom_point(size = 3.5 )+
  facet_grid(~Ano)+
  theme_bw()+
  theme(legend.position = "bottom")+
  scale_color_met_d(name = "Degas")+
  labs(x = "",
       col = "",
       y = "",
       caption = "Núcleo de Estudos em Representação e Democracia - UENF",
       title = "Pesquisas pré eleições do Ibope e Resultados no 1º Turno")+
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# 4. Abrir bancos LAPOP ----

lapop_2006 <- read_dta("1.bancos/bancos_lapop/lapop_2006.dta")
lapop_2008 <- read_dta("1.bancos/bancos_lapop/lapop_2008.dta")
lapop_2010 <- read_dta("1.bancos/bancos_lapop/lapop_2010.dta")
lapop_2012 <- read_dta("1.bancos/bancos_lapop/lapop_2012.dta")
lapop_2014 <- read_dta("1.bancos/bancos_lapop/lapop_2014.dta")
lapop_2016 <- read_dta("1.bancos/bancos_lapop/lapop_2016.dta")
lapop_2018 <- read_dta("1.bancos/bancos_lapop/lapop_2018.dta")
lapop_2021 <- read_dta("1.bancos/bancos_lapop/lapop_2021.dta")

# 5. Filtrar bancos, selecionando as variáveis ----
## 5.1. Variável IDIO2 ----
### 5.1.1 Filtrar nos bancos ----

lapop_2006_idio2 <- lapop_2006  %>%
  rename(idio2=IDIO2) %>%
  select(idio2) %>%  mutate(ano = "2006") %>%
  group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>%
  na.omit()

lapop_2008_idio2 <- lapop_2008 %>%
  select(idio2) %>% mutate(ano = "2008") %>%
  group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>%
  na.omit()

lapop_2010_idio2 <- lapop_2010 %>%
  select(idio2) %>% mutate(ano = "2010") %>% group_by(ano, idio2) %>%
  summarise(total_idio2 = n()) %>%
  mutate(total_respostas = sum(total_idio2), percentual = (total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>%
  select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

lapop_2012_idio2 <- lapop_2012 %>%
  select(idio2) %>% mutate(ano = "2012") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

lapop_2014_idio2 <- lapop_2014 %>%
  select(idio2) %>% mutate(ano = "2014") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

lapop_2016_idio2 <- lapop_2016 %>%
  select(idio2) %>% mutate(ano = "2016") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

lapop_2018_idio2 <- lapop_2018 %>%
  select(idio2) %>% mutate(ano = "2018") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()

lapop_2021_idio2 <- lapop_2021 %>%
  select(idio2) %>% mutate(ano = "2021") %>% group_by(ano, idio2) %>%
  summarise(total_idio2=n()) %>%
  mutate(total_respostas=sum(total_idio2), percentual=(total_idio2/total_respostas)*100) %>%
  select(!total_respostas) %>% select(ano, idio2, total_idio2, percentual) %>%
  remove_labels %>% na.omit()


### 5.1.2 Idio2 unificado - 2006 - 2022 ----

lapop_idio2_total <- bind_rows(lapop_2006_idio2,
                         lapop_2008_idio2,
                         lapop_2010_idio2,
                         lapop_2012_idio2,
                         lapop_2014_idio2,
                         lapop_2016_idio2,
                         lapop_2018_idio2,
                         lapop_2021_idio2)

### 5.1.3. Gráfico de variação - Idio2 ----

cores <- c("blue4", "aquamarine4", "darkgoldenrod")

ggplot(data = lapop_idio2_total, aes(x = ano, y = percentual, color = as.factor(idio2), group = idio2)) +
  geom_line(size = 1) +
  geom_point() +
  scale_colour_manual(values = cores) +
  labs(title = "Variação das respostas ao longo dos anos",
       x = "Ano", y = "Percentual", color = "Resposta")+
  theme_bw()+
  theme(legend.spacing.y = unit(0.5, "cm"))

## 5.2. Variável jc13 ----
### 5.2.1 Filtrar nos bancos ----
lapop_jc13_2006 <- lapop_2006  %>%
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

lapop_jc13_2008 <- lapop_2008 %>%
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

lapop_jc13_2010 <- lapop_2010 %>%
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

lapop_jc13_2012 <- lapop_2012 %>%
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

lapop_jc13_2014 <- lapop_2014 %>%
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

lapop_jc13_2016 <- lapop_2016 %>%
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

lapop_jc13_2018 <- lapop_2018 %>%
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

lapop_jc13_2021 <- lapop_2021 %>%
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

### 5.2.2 jc13 unificado - 2006 - 2022 ----

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

### 5.2.3 Gráfico de variacao (por genero) - jc13 ----

  ggplot(data = lapop_jc13, aes(x = ano, y = percentual_resp_genero, group = q1)) +
    geom_line(aes(linetype = factor(q1)), size = 1.1)+
  scale_linetype_manual(values = c("solid", "dotted"),
                        name = "Linhas",
                        labels = c("Homens", "Mulheres"))+
    labs(
         x = "Ano", y = "Percentual", color = "Opções",)+
    theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),  # cor de fundo
        axis.line = element_line(color = "black"),  # cor dos eixos
        axis.text = element_text(size = 15),  # tamanho da fonte dos rótulos dos eixos
        axis.title = element_text(size = 16),  # tamanho da fonte dos títulos dos eixos
        plot.title = element_text(size = 16),  # tamanho da fonte do título do gráfico
        legend.position = "bottom"  # posição da legenda
  )+
    theme(legend.spacing.y = unit(0.5, "cm"))

## 5.3 Variável jc15a ----
### 5.3.1 Filtrar nos bancos ----

lapop_jc15a_2010 <- lapop_2010 %>%
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

lapop_jc15a_2012 <- lapop_2012 %>%
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

lapop_jc15a_2014 <- lapop_2014 %>%
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

lapop_jc15a_2016 <- lapop_2016 %>%
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

lapop_jc15a_2018 <- lapop_2018 %>%
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

lapop_jc15a_2021 <- lapop_2021 %>%
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

### 5.3.2 Unificar bancos ----

lapop_jc15a <- bind_rows(lapop_jc15a_2010,
                         lapop_jc15a_2012,
                         lapop_jc15a_2014,
                         lapop_jc15a_2016,
                         lapop_jc15a_2018,
                         lapop_jc15a_2021)

### 5.3.3 Gráfico de variacao (por genero) - jc15a ----

ggplot(data = lapop_jc15a, aes(x = ano, y = percentual_resp_genero, group = q1)) +
  geom_line(aes(linetype = factor(q1)), size = 2)+
  scale_linetype_manual(values = c("solid", "dotted"),
                        name = "Linhas:",
                        labels = c("Homens", "Mulheres"))+
  labs(x = "Ano", y = "Percentual", color = "Opções",)+
  theme_bw()+
  theme(plot.background = element_rect(fill = "white"),  # cor de fundo
        axis.line = element_line(color = "black"),  # cor dos eixos
        axis.title.y = element_text(size = 25, family = "Times"),
        axis.text.y = element_text(size = 20, family = "Times"),
        axis.text.x = element_text(size = 20, family = "Times"),  # tamanho da fonte dos rótulos dos eixos
        axis.title.x = element_text(size = 25, family = "Times"),  # tamanho da fonte dos títulos dos eixos
        legend.title = element_text(size = 27, family = "Times"), # tamanho do título da legenda
        legend.text = element_text(size = 25, family = "Times"),
        legend.position = "bottom"  # posição da legenda
  )
X1_01838 <- read_sav("bancos_eseb/1_01838.sav")
View(X1_01838)

