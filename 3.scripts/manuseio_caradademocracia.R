# 1. abrir pacotes. ----
pacman::p_load(tidyverse, haven, ggplot2, ggthemes, MetBrewer, googlesheets4, labelled, dplyr)

# 2. abrir bancos: cara da democracia. ----
cara_2018_ondaa <- read_sav("1.bancos/bancos_caradademocracia/cara_2018_ondaa.sav")
cara_2018_ondab <- read_sav("1.bancos/bancos_caradademocracia/cara_2018_ondab.sav")
cara_2018_ondac <- read_sav("1.bancos/bancos_caradademocracia/cara_2018_ondac.sav")
cara_2018_ondad <- read_sav("1.bancos/bancos_caradademocracia/cara_2018_ondad.sav")
cara_2019 <- read_sav("1.bancos/bancos_caradademocracia/cara_2019.sav")
cara_2020 <- read_sav("1.bancos/bancos_caradademocracia/cara_2020.sav")
