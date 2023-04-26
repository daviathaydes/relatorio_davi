#

# 1. Carregar pacotes
library(ggplot2)
library(dplyr)
library(pacman)
pacman::p_load(tidyverse, haven)

# 2. Abrir bancos ----

#lapop_2006 <- read_dta("bancos_lapop/lapop_2006.dta")
#lapop_2008 <- read_dta("bancos_lapop/lapop_2008.dta")
#lapop_2010 <- read_dta("bancos_lapop/lapop_2010.dta")
#lapop_2012 <- read_dta("bancos_lapop/lapop_2012.dta")
#lapop_2014 <- read_dta("bancos_lapop/lapop_2014.dta")
#lapop_2016 <- read_dta("bancos_lapop/lapop_2016.dta")
lapop_2018 <- read_dta("1.bancos/bancos_lapop/lapop_2018.dta")
lapop_2021 <- read_dta("1.bancos/bancos_lapop/lapop_2021.dta")


###

lapop_2018_filtrado <- lapop_2018 %>%
  select(ed, q1, wave, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18) %>%
  na.omit() # 1342 obs

# criar categorias de classes de acordo com o criterio renda brasil

lapop_2018_teste <- lapop_2018_filtrado %>%
  mutate(lapop_2018_filtrado, classe = if_else(criterio_brasil %in% c(0:16), "D",
                                               ifelse(criterio_brasil %in% c(17:28), "C",
                                                      ifelse(criterio_brasil %in% c(29:44), "B",
                                                             ifelse(criterio_brasil %in% c(45:100), "A", NA)))))


# Criar categorias de escolaridade de acordo com o criterio renda brasil

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(renda_brasil = (r1*2) + (r3*2) + r4 + r4a + (r5*4) + (r6*3) +
                           (r7*2) + r8 + (r12*4) + (r14*3) + (r15*3) + (r16*3) +
                           (r18*2),
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 7),
         criterio_brasil = renda_brasil + pontos_escolaridade)

# boxplot

lapop_2018_teste %>%
  ggplot()+
  aes(x = as_factor(q1), y = classe)+
  geom_jitter(alpha = 0.7, size = 2)+
  geom_boxplot(alpha= 0.6, size = 2)+
   stat_summary(fun=mean, geom="point", shape=20, size=8, color="red",
                position = position_dodge2(0.75), show.legend = FALSE)+
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb'))+
   labs(title = "Classe de consumo e Escolaridade por Gênero",
        caption = "Elaborado pelos autores com base nos dados LAPOP 2018",
        y = "Renda Brasil",
        fill = "")


lapop_2018_filtrado %>%
  ggplot()+
  aes(x = as_factor(q1), y = criterio_brasil)+
  geom_density()

# histogram

lapop_2018_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

# dotplot

ggplot(lapop_2018_teste, aes(x = classe, fill = q1)) +
  geom_dotplot( stackdir = "center", dotsize = 0.1, binwidth = 0.3) +
  labs(x = "Classe", y = "Contagem", fill = "Gênero") +
  theme_classic()
