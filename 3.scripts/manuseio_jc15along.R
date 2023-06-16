# 1. abrir pacotes ----
pacman::p_load(tidyverse, haven, ggplot2, ggthemes, MetBrewer, googlesheets4, labelled, dplyr)

## 1.2 abrir bancos lapop que contenham a variável jc15a ----

lapop_2010 <- read_dta("1.bancos/bancos_lapop/lapop_2010.dta")
lapop_2012 <- read_dta("1.bancos/bancos_lapop/lapop_2012.dta")
lapop_2014 <- read_dta("1.bancos/bancos_lapop/lapop_2014.dta")
lapop_2016 <- read_dta("1.bancos/bancos_lapop/lapop_2016.dta")
lapop_2018 <- read_dta("1.bancos/bancos_lapop/lapop_2018.dta")
lapop_2021 <- read_dta("1.bancos/bancos_lapop/lapop_2021.dta")

# 2. filtrar bancos ----
##2.1 2010. ----

lapop_2010_filtrado <- lapop_2010 %>%
  select(ed, q1, q10, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18,jc15a )

### 2.1.1 recategorizar variavel de escolaridade ----

lapop_2010_filtrado <- lapop_2010_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))

### 2.1.2 atribuir pontos ao critério brasil ----

lapop_2010_filtrado <- lapop_2010_filtrado %>%
  mutate(renda_brasil = (r1*1) + (r3*4) + r4 + r4a + (r5*4) + (r6*2) +
           r7 + r8 + r12 + (r14*4) + r15 + r16 +
           r18,
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 8),
         criterio_brasil = renda_brasil + pontos_escolaridade)

### 2.1.3 criar classes (A, B, C e D) ----

lapop_2010_filtrado <- lapop_2010_filtrado %>%
  mutate(lapop_2010_filtrado, classe = if_else(criterio_brasil %in% c(0:13), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(14:22), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(23:34), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(35:100), "CLASSE A", NA)))))
### 2.1.4 histograma e boxplot - renda brasil e criterio brasil de 2010 ----
lapop_2010_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

#

lapop_2010_filtrado %>%
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

## 2.2 2012. ----

lapop_2012_filtrado <- lapop_2012 %>%
  select(ed, q1, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18,jc15a ) # grande quantidade de NAs na r18.


### 2.2.1 recategorizar variavel de escolaridade ----
lapop_2012_filtrado <- lapop_2012_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))
### 2.2.2 atribuir pontos ao critério brasil ----

lapop_2012_filtrado <- lapop_2012_filtrado %>%
  mutate(renda_brasil = (r1*1) + (r3*4) + r4 + r4a + (r5*4) + (r6*2) +
           r7 + r8 + r12 + (r14*4) + r15 + r16,
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 8),
         criterio_brasil = renda_brasil + pontos_escolaridade)

### 2.2.3 criar classes (A, B, C e D) ----

lapop_2012_filtrado <- lapop_2012_filtrado %>%
  mutate(lapop_2012_filtrado, classe = if_else(criterio_brasil %in% c(0:13), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(14:22), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(23:34), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(35:100), "CLASSE A", NA)))))
### 2.2.4 histograma e boxplot - renda brasil e criterio brasil de 2012 ----
lapop_2012_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

#

lapop_2012_filtrado %>%
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

## 2.3 2014. ----

lapop_2014_filtrado <- lapop_2014 %>%
  select(ed, q1, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18,jc15a )
### 2.3.1 recategorizar variavel de escolaridade ----

lapop_2014_filtrado <- lapop_2014_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))
### 2.3.2 atribuir pontos ao critério brasil ----

lapop_2014_filtrado <- lapop_2014_filtrado %>%
  mutate(renda_brasil = (r1*2) + (r3*4) + r4 + r4a + (r5*4) + (r6*2) +
           r7 + r8 + r12 + (r14*4) + r15 + r16 +
           r18,
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 8),
         criterio_brasil = renda_brasil + pontos_escolaridade)

### 2.3.3 criar classes (A, B, C e D) ----

lapop_2014_filtrado <- lapop_2014_filtrado %>%
  mutate(lapop_2014_filtrado, classe = if_else(criterio_brasil %in% c(0:13), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(14:22), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(23:34), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(35:100), "CLASSE A", NA)))))
### 2.3.4 histograma e boxplot - renda brasil e criterio brasil de 2014 ----

lapop_2014_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

#

lapop_2014_filtrado %>%
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

## 2.4 2016. ----
lapop_2016_filtrado <- lapop_2016 %>%
  select(ed, q1, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18,jc15a )

### 2.4.1 recategorizar variavel de escolaridade ----

lapop_2016_filtrado <- lapop_2016_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))
### 2.4.2 atribuir pontos ao critério brasil ----

lapop_2016_filtrado <- lapop_2016_filtrado %>%
  mutate(renda_brasil = r1 + (r3*2) + r4 + r4a + (r5*3) + (r6*2) +
           (r7*2) + r8 + (r12*2) + (r14*3) + (r15*3) + r16 +
           r18,
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 7),
         criterio_brasil = renda_brasil + pontos_escolaridade)

### 2.4.3 criar classes (A, B, C e D) ----

lapop_2016_filtrado <- lapop_2016_filtrado %>%
  mutate(lapop_2016_filtrado, classe = if_else(criterio_brasil %in% c(0:16), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(17:28), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(29:44), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(45:100), "CLASSE A", NA)))))

### 2.4.4 histograma e boxplot - renda brasil e criterio brasil de 2016 ----

lapop_2016_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()

#

lapop_2016_filtrado %>%
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

## 2.5 2018. ----
lapop_2018_filtrado <- lapop_2018 %>%
  select(ed, q1, wave, q10new, r1, r3, r4, r4a, r5, r6,
         r7, r8, r12, r14, r15, r16, r18, jc15a)
### 2.5.1 recategorizar variavel de escolaridade ----

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))
### 2.5.2 atribuir pontos ao critério brasil ----

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(renda_brasil = r1 + (r3*2) + r4 + r4a + (r5*3) + (r6*2) +
           (r7*2) + r8 + (r12*4) + (r14*3) + (r15*3) + r16 +
           r18,
         pontos_escolaridade = case_when(escolaridade == "Fundamental I incompleto" ~ 0,
                                         escolaridade == "Fundametal II incompleto" ~ 1,
                                         escolaridade == "Ensino Médio incompleto" ~ 2,
                                         escolaridade == "Superior incompleto" ~ 4,
                                         escolaridade == "Superior completo" ~ 7),
         criterio_brasil = renda_brasil + pontos_escolaridade)
### 2.5.3 criar classes (A, B, C e D) ----
lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(lapop_2018_filtrado, classe = if_else(criterio_brasil %in% c(0:16), "CLASSE D",
                                               ifelse(criterio_brasil %in% c(17:28), "CLASSE C",
                                                      ifelse(criterio_brasil %in% c(29:44), "CLASSE B",
                                                             ifelse(criterio_brasil %in% c(45:100), "CLASSE A", NA)))))
### 2.5.4 histograma e boxplot - renda brasil e criterio brasil de 2018 ----

lapop_2018_filtrado %>%
  ggplot()+
  aes(x= renda_brasil)+
  geom_histogram(bins = 30,  fill = "gray", color = "black")+
  labs(x = "Renda Brasil", y = "Contagem")+
  theme_bw()
#

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

## 2.6 2021. ----
lapop_2021_filtrado <- lapop_2021 %>%
  select(edr, q1tb, wave, q10newt, r3, r4, r6,
         r7, r15, r16, r18, r18n, jc15a)

### 2.6.1 recategorizar variavel de escolaridade ----

lapop_2018_filtrado <- lapop_2018_filtrado %>%
  mutate(escolaridade = ifelse(ed %in% c(0, 1, 2, 3), "Fundamental I incompleto",
                               ifelse(ed %in% c(4, 5, 6), "Fundametal II incompleto",
                                      ifelse(ed %in% c(7, 8, 9, 10), "Ensino Médio incompleto",
                                             ifelse(ed %in% c(11, 12, 13, 14, 15), "Superior incompleto",
                                                    ifelse(ed %in% c(16, 17), "Superior completo", NA))))))





# 3. filtrar variavel e unificar bancos ----
## 3.1 2010 - total de respostas positivas, por genero e classe. ----

lapop_2010_jc15a <- lapop_2010_filtrado %>%
  filter_at(vars(jc15a, classe),all_vars(!is.na(.))) %>%
  group_by(q1, jc15a, classe) %>%
  summarise(total_genero_jc15a = n()) %>%
  mutate(total_genero = sum(total_genero_jc15a),
         percentual_genero = total_genero_jc15a/total_genero*100) %>%
  mutate(wave = 2010)
lapop_2010_jc15a_m <- lapop_2010_jc15a[1:3,]
lapop_2010_jc15a_f <- lapop_2010_jc15a[8:10,]
lapop_2010_jc15a <- bind_rows(lapop_2010_jc15a_m,
                              lapop_2010_jc15a_f)

## 3.2 2012 - total de respostas positivas, por genero e classe. ----
lapop_2012_jc15a <- lapop_2012_filtrado %>%
  filter_at(vars(jc15a, classe),all_vars(!is.na(.))) %>%
  group_by(q1, jc15a, classe) %>%
  summarise(total_genero_jc15a = n()) %>%
  mutate(total_genero = sum(total_genero_jc15a),
         percentual_genero = total_genero_jc15a/total_genero*100) %>%
  mutate(wave = 2012)

lapop_2012_jc15a_m <- lapop_2012_jc15a[1:3,]
lapop_2012_jc15a_f <- lapop_2012_jc15a[7:9,]
lapop_2012_jc15a <- bind_rows(lapop_2012_jc15a_m,
                              lapop_2012_jc15a_f)

## 3.3 2014 - total de respostas positivas, por genero e classe. ----

lapop_2014_jc15a <- lapop_2014_filtrado %>%
  filter_at(vars(jc15a, classe),all_vars(!is.na(.))) %>%
  group_by(q1, jc15a, classe) %>%
  summarise(total_genero_jc15a = n()) %>%
  mutate(total_genero = sum(total_genero_jc15a),
         percentual_genero = total_genero_jc15a/total_genero*100)%>%
  mutate(wave = 2014)
lapop_2014_jc15a_m <- lapop_2014_jc15a[1:4,]
lapop_2014_jc15a_f <- lapop_2014_jc15a[9:12,]
lapop_2014_jc15a <- bind_rows(lapop_2014_jc15a_m,
                              lapop_2014_jc15a_f)

## 3.4 2016  - total de respostas positivas, por genero e classe. ----
lapop_2016_jc15a <- lapop_2016_filtrado %>%
  filter_at(vars(jc15a, classe),all_vars(!is.na(.))) %>%
  group_by(q1, jc15a, classe) %>%
  summarise(total_genero_jc15a = n()) %>%
  mutate(total_genero = sum(total_genero_jc15a),
         percentual_genero = total_genero_jc15a/total_genero*100)%>%
  mutate(wave = 2016)
lapop_2016_jc15a_m <- lapop_2016_jc15a[1:3,]
lapop_2016_jc15a_f <- lapop_2016_jc15a[7:9,]
lapop_2016_jc15a <- bind_rows(lapop_2016_jc15a_m,
                              lapop_2016_jc15a_f)

## 3.5 2018 - total de respostas positivas, por genero e classe. ----
lapop_2018_jc15a <- lapop_2018_filtrado %>%
  filter_at(vars(jc15a, classe),all_vars(!is.na(.))) %>%
  group_by(q1, jc15a, classe) %>%
  summarise(total_genero_jc15a = n()) %>%
  mutate(total_genero = sum(total_genero_jc15a),
         percentual_genero = total_genero_jc15a/total_genero*100)%>%
  mutate(wave = 2018)

lapop_2018_jc15a_m <- lapop_2018_jc15a[1:4,]
lapop_2018_jc15a_f <- lapop_2018_jc15a[9:11,]
lapop_2018_jc15a <- bind_rows(lapop_2018_jc15a_m,
                              lapop_2018_jc15a_f)


# 3.6 unificar bancos. ----
lapop_total_jc15a <- bind_rows(lapop_2010_jc15a,
                               lapop_2012_jc15a,
                               lapop_2014_jc15a,
                               lapop_2016_jc15a,
                               lapop_2018_jc15a)
lapop_total_jc15aA <- lapop_total_jc15a[1:12,]
lapop_total_jc15aB <- lapop_total_jc15a[14:16,]
lapop_total_jc15aC <- lapop_total_jc15a[18:26,]
lapop_total_jc15aD <- lapop_total_jc15a[28:33,]

lapop_total_jc15a <- bind_rows(lapop_total_jc15aA,
                               lapop_total_jc15aB,
                               lapop_total_jc15aC,
                               lapop_total_jc15aD)

# 4. Gráfico: percentuais de respaldo ao fechamento do congresso pelo presidentes, por genero e classe. ----

ggplot(lapop_total_jc15a, aes(x = wave, y = percentual_genero, group = q1, color = factor(q1))) +
  geom_line(size = 1.8) +
  geom_point() +
  facet_grid(classe ~ ., labeller = labeller(classe_social = c("A" = "Classe A", "B" = "Classe B", "C" = "Classe C", "D" = "Classe D"))) +
  labs(#title = "Percentuais de concordância ao fechamento do congresso pelo presidente de acordo com gênero e classe, 2010 - 2018.",
         x = "Ano", y = "Percentual de Respostas Positivas", color = "Gênero", caption = "Nota: Classe A nao consta na análise por insuficiencia de dados.") +
  scale_color_manual(values = c("#8e9b79", "#fc8d62"), labels = c("Homem", "Mulher")) +
  theme_bw()+
  ylim(0, 70)+
  theme(plot.background = element_rect(fill = "white"),  # cor de fundo
                    axis.line = element_line(color = "black"),  # cor dos eixos
                    axis.text = element_text(size = 15, family = "Times"),  # tamanho da fonte dos rótulos dos eixos
                    axis.title = element_text(size = 16, family = "Times"),  # tamanho da fonte dos títulos dos eixos
                    plot.title = element_text(size = 16, family = "Times"),  # tamanho da fonte do título do gráfico
                    plot.caption = element_text(size = 17, family = "Times"),
                    strip.text = element_text(size = 14, family = "Times"),
        legend.title = element_text(size = 25, family = "Times"), # tamanho do título da legenda
        legend.text = element_text(size = 22, family = "Times"),
                    legend.position = "bottom"  # posição da legenda
  )+
  theme(legend.spacing.y = unit(0.5, "cm"))


##
ggplot(lapop_total_jc15a, aes(x = wave, y = percentual_genero, fill = factor(q1))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ classe, labeller = labeller(classe_social = c("A" = "Classe A", "B" = "Classe B", "C" = "Classe C", "D" = "Classe D"))) +
  labs(x = "Ano", y = "Percentual de Respostas Positivas", fill = "Gênero") +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Homem", "Mulher")) +
  theme_minimal()
