# Dados de comercio brasileiro usam o pacote {comerciobr}. Atualizar o pacote uma vez por mês
# usando o remotes::install_github("fernandobastosneto/comerciobr")

library(comerciobr)
library(tidyverse)

# Dados de janeiro até o último mês

ultimo_mes <- sh4_df %>% 
  filter(co_ano == max(co_ano)) %>% 
  filter(co_mes == max(co_mes)) %>% 
  distinct(co_mes) %>% 
  pull()

dados_comercio <- comerciobr::sh4_df %>% 
  filter(co_mes <= ultimo_mes) %>% 
  group_by(path, co_ano) %>% 
  summarise(value = sum(value)) %>% 
  pivot_wider(names_from = path, values_from = value) %>% 
  mutate(Corrente = EXP + IMP, 
         Saldo = EXP - IMP) %>%
  rename(Exportações = EXP,
         Importações = IMP) %>% 
  pivot_longer(cols = Exportações:Saldo, names_to = "trade_flow", values_to = "value")

# Índice Intraindustria

dados_intraindustria <- comerciobr::sh4_df %>% 
  mutate(co_sh2 = str_sub(co_sh4, 1, 2)) %>% 
  mutate(co_sh2 = as.numeric(co_sh2)) %>% 
  mutate(fator = case_when(co_sh2 > 15 & co_sh2 < 68 ~ "Produto Industrializado",
                           co_sh2 > 71 & co_sh2 < 90 ~ "Produto Industrializado",
                         TRUE ~ "Outro")) %>% 
  filter(fator == "Produto Industrializado") %>% 
  group_by(co_ano, path, co_sh2) %>% 
  summarise(value = sum(value)) %>%
  ungroup() %>% 
  pivot_wider(names_from = path, values_from = value, values_fill = 0) %>% 
  mutate(soma = EXP + IMP,
         modulo = abs(EXP - IMP)) %>% 
  group_by(co_ano) %>%
  summarise(indiceGL = 1 - (sum(modulo)/sum(soma)))

# Índice HH - índice de concentração de comércio

dados_hh <- comerciobr::sh4_df %>%
    group_by(co_ano, no_pais) %>%
    summarise(value = sum(value)) %>% 
    group_by(co_ano) %>%
    mutate(total_ano = sum(value)) %>% 
    ungroup() %>% 
    mutate(porcentagem = value/total_ano) %>% 
    mutate(HH = porcentagem^2) %>% 
    group_by(co_ano) %>%
    summarise(HH = sum(HH))

#H abaixo de 0.01 - competitivo
#H abaixo de 0.15 - não concentrada
#H de 0.15 a 0.25 - concentração moderada
#H acima de 0.25 - alta concentração
  
