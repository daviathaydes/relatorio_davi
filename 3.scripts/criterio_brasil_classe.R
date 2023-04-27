#

# 1. Carregar pacotes ----
pacman::p_load(tidyverse, haven, labelled)

## 1.2. Abrir bancos ----

#lapop_2006 <- read_dta("bancos_lapop/lapop_2006.dta")
#lapop_2008 <- read_dta("bancos_lapop/lapop_2008.dta")
#lapop_2010 <- read_dta("bancos_lapop/lapop_2010.dta")
#lapop_2012 <- read_dta("bancos_lapop/lapop_2012.dta")
#lapop_2014 <- read_dta("bancos_lapop/lapop_2014.dta")
#lapop_2016 <- read_dta("bancos_lapop/lapop_2016.dta")
lapop_2018 <- read_dta("1.bancos/bancos_lapop/lapop_2018.dta")
lapop_2021 <- read_dta("1.bancos/bancos_lapop/lapop_2021.dta")



# 2.0 Filtrar e selecionar banco ----

lapop_2018_filtrado <- lapop_2018 %>%
  select(ed, q1, wave, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18, pol1)


# 3.0 criar categorias de classes de acordo com o criterio renda brasil ----

## 3.1 Recategorizar variável escolaridade ----

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))

## 3.2 Atribuir pontos ao Critério Brasil ----

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

## 3.3 Criar classes (A, B, C e D) ----
# Vamos deixar até a classe D ou apenas até a C?

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(lapop_2018_filtrado, classe = if_else(criterio_brasil %in% c(0:16), "D",
                                               ifelse(criterio_brasil %in% c(17:28), "C",
                                                      ifelse(criterio_brasil %in% c(29:44), "B",
                                                             ifelse(criterio_brasil %in% c(45:100), "A", NA)))))


### 3.3.1 Histograma da variável Renda Brasil

lapop_2018_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

## 3.3.2 Boxplot - variável critério brasil ----

lapop_2018_filtrado %>%
  filter(!is.na(classe)) %>%
  ggplot()+
  aes(x = criterio_brasil, y = classe, fill = as_factor(q1))+
  geom_jitter(alpha = 0.3, size = 2)+
  geom_boxplot(alpha= 1.0, size = 1)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red",
               position = position_dodge2(0.75), show.legend = FALSE)+
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb'))+
  labs(title = "Capacidade de consumo por Gênero e Classe - 2018",
       caption = "Elaborado pelos autores com base nos dados LAPOP 2018",
       y = "Renda Brasil",
       fill = "")+
  theme_bw()

## 3.4 Gráfico de barras - interesse por política, classe e genero ----

lapop_2018_pol1_teste <- lapop_2018_filtrado %>%
  filter_at(vars(pol1, classe),all_vars(!is.na(.))) %>% # var pol1 tem muitos NAs
  group_by(q1, pol1, classe) %>%
  summarise(total_genero_pol1 = n()) %>%
  group_by(q1) %>%
  mutate(total_genero = sum(total_genero_pol1)) %>%
  ungroup() %>%
  mutate(total = sum(total_genero_pol1),
         percentual_genero = total_genero/total*100) #%>%
  select(q1, resp_genero, pol1, percentual_genero, classe) %>%
  remove_labels()



# porcentagem de interesse por politica por genero

lapop_2018_pol1 <- lapop_2018_filtrado_1 %>%
  mutate(ano="2018") %>%
  group_by(ano, q1, pol1) %>%
  summarise(resp_genero = n())%>%
  group_by(ano, q1) %>% na.omit() %>%
  mutate(total_q1 = sum(resp_genero),
         percentual_genero = resp_genero/total_q1*100) %>%
  select(ano, q1, resp_genero, pol1, percentual_genero) %>%
  remove_labels()


# Criar categorias de escolaridade de acordo com o criterio renda brasil







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



## facets

lapop_2018_pol1_teste %>%
  ggplot()+
  aes(x = (pol1), y = (percentual_genero),  fill = q1)+
geom_bar(stat = "identity", position = "dodge")+
facet_wrap(~ classe, ncol = 4)+
  theme_bw()


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



# dotplot

ggplot(lapop_2018_teste, aes(x = classe, fill = q1)) +
  geom_dotplot( stackdir = "center", dotsize = 0.1, binwidth = 0.3) +
  labs(x = "Classe", y = "Contagem", fill = "Gênero") +
  theme_classic()
