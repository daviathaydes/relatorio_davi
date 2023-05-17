# 1. Carregar pacotes ----
pacman::p_load(tidyverse, haven, ggplot2, ggthemes, MetBrewer, googlesheets4, labelled, dplyr)

## 1.2. Abrir bancos ----

#lapop_2006 <- read_dta("bancos_lapop/lapop_2006.dta")
#lapop_2008 <- read_dta("bancos_lapop/lapop_2008.dta")
#lapop_2010 <- read_dta("bancos_lapop/lapop_2010.dta")
#lapop_2012 <- read_dta("bancos_lapop/lapop_2012.dta")
#lapop_2014 <- read_dta("bancos_lapop/lapop_2014.dta")
#lapop_2016 <- read_dta("bancos_lapop/lapop_2016.dta")
lapop_2018 <- read_dta("1.bancos/bancos_lapop/lapop_2018.dta")
#lapop_2021 <- read_dta("1.bancos/bancos_lapop/lapop_2021.dta")

# 2.0 Filtrar e selecionar banco ----

lapop_2018_filtrado <- lapop_2018 %>%
  select(ed, q1, wave, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18, pol1, jc10, jc16a )

recategorizacao <- c("Homem", "Mulher")
lapop_2018_filtrado$q1 <- factor(lapop_2018_filtrado$q1, levels = c(1, 2), labels = recategorizacao)

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
  mutate(lapop_2018_filtrado, classe = if_else(criterio_brasil %in% c(0:16), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(17:28), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(29:44), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(45:100), "CLASSE A", NA)))))


### 3.3.1 Histograma da variável Renda Brasil ----

lapop_2018_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

### 3.3.2 Boxplot - variável critério brasil ----

lapop_2018_filtrado %>%
  filter(!is.na(classe)) %>%
  ggplot()+
  aes(x = criterio_brasil, y = classe, fill = as_factor(q1))+
  geom_jitter(alpha = 0.4, size = 2)+
  geom_boxplot(alpha= 1.0, size = 1)+
  stat_summary(fun=mean, geom="point", shape=20, size=4, color="red",
               position = position_dodge2(0.75), show.legend = FALSE)+
  scale_fill_manual(values = c('#66c2a5','#fc8d62','#8da0cb'))+
  labs(
       y = "Classe",
       fill = "")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 10),  # tamanho da fonte dos rótulos dos eixos
    axis.title = element_text(size = 17),  # tamanho da fonte dos títulos dos eixos
    legend.title = element_text(size = 14), # tamanho do título da legenda
    legend.text = element_text(size = 14), # tamanho do texto da legenda
    #   plot.caption = element_text(size = 12) # tamanho do texto de rodapé
  )



## 3.4 Gráfico de barras - interesse por política, classe e genero ----

lapop_2018_pol1_teste <- lapop_2018_filtrado %>%
  filter_at(vars(pol1, classe),all_vars(!is.na(.))) %>% # var pol1 tem muitos NAs
  group_by(q1, pol1, classe) %>%
  summarise(total_genero_pol1 = n()) %>%
  mutate(total_genero = sum(total_genero_pol1),
         percentual_genero = total_genero_pol1/total_genero*100)



### 3.4.1 criar grafico ----

# opcao de grafico sem recategorizar o interesse por poltiica
lapop_2018_pol1_teste %>%
  filter(pol1 >=3) %>%
  ggplot()+
  aes(x = as_factor(q1), y = percentual_genero, fill = as_factor(pol1))+
 geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c('#66c2a5','#fc8d62'))+
 facet_wrap(~ classe, ncol = 4)+
  theme_bw()


# opcao de grafico com recategorias de interesse por poltiica
# ficou melhor

lapop_2018_pol1_teste %>%
  mutate(pol1_binario = case_when(pol1 == 1 ~ 1, # tem interesse por politica
                                  pol1 == 2 ~ 1, # tem interesse por politica
                                  pol1 == 3 ~ 0, # nao tem interesse por politica
                                  pol1 == 4 ~ 0,)) %>% # nao tem interesse por politica
  ggplot()+
  aes(x = as_factor(q1), y = percentual_genero, fill = as_factor(pol1_binario))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c('#8e9b79','#fc8d62'),
                    name = "Interesse por política:",
                    labels = c("Nada ou pouco.", "Algo ou muito."))+
  facet_wrap(~ classe, ncol = 4)+
  labs(
       y = "Percentual", x = "",
       fill = "")+
  theme_bw()+
  theme(
    axis.text = element_text(size = 10),  # tamanho da fonte dos rótulos dos eixos
    axis.title = element_text(size = 14),  # tamanho da fonte dos títulos dos eixos
    legend.title = element_text(size = 12), # tamanho do título da legenda
    legend.text = element_text(size = 11), # tamanho do texto da legenda
    plot.caption = element_text(size = 12),
    legend.position = "bottom"
  )


## 3.5 atitude/apoio a democracia - presidente governar sem o STF ----

lapop_2018_jc16a_teste <- lapop_2018_filtrado %>%
  filter_at(vars(jc16a, classe),all_vars(!is.na(.))) %>% #
  group_by(q1, jc16a, classe) %>%
  summarise(total_genero_jc16a = n()) %>%
  mutate(total_genero = sum(total_genero_jc16a),
         percentual_genero = total_genero_jc16a/total_genero*100)

### 3.5.1 criar gráfico ----

lapop_2018_jc16a_teste %>% ggplot()+
  aes(x = as_factor(q1), y = percentual_genero, fill = as_factor(jc16a))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c('#8e9b79','#fc8d62'),
                    name = "Dissolução do STF:",
                    labels = c("Sim, justifica-se.", "Nao, nao se justifica."))+
  facet_wrap(~ classe, ncol = 4)+
  labs(
      # caption = "Elaborado pelos autores com base nos dados LAPOP 2018",
       y = "Percentual", x = "",
       fill = "")+
  theme_minimal()+
  theme(
        axis.title.y = element_text(size = 25, family = "Times"),
        axis.text.y = element_text(size = 20, family = "Times"),
        axis.text.x = element_text(size = 20, family = "Times"),  # tamanho da fonte dos rótulos dos eixos
        axis.title.x = element_text(size = 25, family = "Times"),  # tamanho da fonte dos títulos dos eixos
        strip.text = element_text(size = 20, family = "Times"),
        legend.title = element_text(size = 27, family = "Times"), # tamanho do título da legenda
        legend.text = element_text(size = 25, family = "Times"),
        legend.position = "bottom"
     #   plot.caption = element_text(size = 12) # tamanho do texto de rodapé
  )

## 3.6 Atitude/apoio a democracia - se um golpe militar se justifica em caso de muito crime ----

lapop_2018_jc10_teste <- lapop_2018_filtrado %>%
  filter_at(vars(jc10, classe),all_vars(!is.na(.))) %>% #
  group_by(q1, jc10, classe) %>%
  summarise(total_genero_jc10 = n()) %>%
  mutate(total_genero = sum(total_genero_jc10),
         percentual_genero = total_genero_jc10/total_genero*100)

### 3.6.1 Criar gráfico ----

lapop_2018_jc10_teste %>% ggplot()+
  aes(x = as_factor(q1), y = percentual_genero, fill = as_factor(jc10))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_manual(values = c('#8e9b79','darkslategray4'),
                    name = "Golpe militar",
                    labels = c("Sim, justifica-se", "Nao, nao se justifica"))+
  facet_wrap(~ classe, ncol = 4)+
  labs(
    # caption = "Elaborado pelos autores com base nos dados LAPOP 2018",
    y = "Percentual", x = "",
    fill = "")+
  theme_fivethirtyeight()+
  theme(
    axis.text = element_text(size = 10),  # tamanho da fonte dos rótulos dos eixos
    axis.title = element_text(size = 17),  # tamanho da fonte dos títulos dos eixos
    legend.title = element_text(size = 14), # tamanho do título da legenda
    legend.text = element_text(size = 14), # tamanho do texto da legenda
    #   plot.caption = element_text(size = 12) # tamanho do texto de rodapé
  )









#lapop_2018_filtrado %>%
  ggplot()+
  aes(x = as_factor(q1), y = criterio_brasil)+
  geom_density()

# histogram



# dotplot

ggplot(lapop_2018_teste, aes(x = classe, fill = q1)) +
  geom_dotplot( stackdir = "center", dotsize = 0.1, binwidth = 0.3) +
  labs(x = "Classe", y = "Contagem", fill = "Gênero") +
  theme_classic()
