# devtools::install_github('wilsonfreitas/rbcb')

library(tidyverse)
library(rbcb)

# Código de indicadores no Banco Central: https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries

# Inflação

ipca <- rbcb::get_series(c(IPCA = 433))

ipca  %>% 
  View()

# Índice de Atividade Econômica (IBC-Br)

ibc_br <- rbcb::get_series(c(ibc_br = 24363))

# ibc_br %>%
#   mutate(var = (ibc_br/lag(ibc_br))-1) %>% 
#   ggplot() +
#   geom_col(aes(date, var)) +
#   geom_line(aes(date, ibc_br))


# Taxa de Desocupação

desemprego <- rbcb::get_series(c(desemprego = 24369))

# Dólar

dolar <- rbcb::get_series(c(dolar = 1), last = 1000)

dolar %>% 
  ggplot() +
  geom_line(aes(date, dolar))

# Expectativas
# Usando API não documentada do Banco Central


# Possíveis indicadores:
# Grupo: "Índices de Preços", Indicadores: IPCA
# Grupo: "Taxas", Indicadores: Selic e Câmbio
# Grupo: "Fiscal", Indicadores: Resultado Primário, Dívida Líquida do Setor Público
# Grupo: "PIB", Indicadores: PIB total, PIB agropecuária, PIB indústria, PIB serviços
# Grupo: "Externo", Indicadores: Balança Comercial, Conta Corrente, Investimento Direto no País
# Grupo: "Produção Industrial", Indicadores: Produção Industrial


indicadores_expectativas <- c("ipca", "idp", "resultado_primario",
                              "divida_liquida", "selic", "cambio",
                              "pib_total", "pib_agro", "pib_industria",
                              "pib_servicos")

get_expectativa <- function(indicador) {
  
  # Adaptar as data em formato aceito pela API do Banco Central
  
  ano_inicial <- lubridate::year(Sys.Date()-30)
  mes_inicial <- str_remove(Sys.Date()-30, "^\\d{4}-") %>% 
    str_remove("-\\d{2}$")
  dia_inicial <- str_extract(Sys.Date()-30, "\\d{2}$")
  
  data_inicial <- paste0(dia_inicial, "/", mes_inicial, "/", ano_inicial)

  ano_final <- lubridate::year(Sys.Date())
  mes_final <- str_remove(Sys.Date(), "^\\d{4}-") %>% 
    str_remove("-\\d{2}$")
  dia_final <- str_extract(Sys.Date(), "\\d{2}$")
  
  data_final <- paste0(dia_final, "/", mes_final, "/", ano_final)
  
  url <- "https://www3.bcb.gov.br/expectativas2/rest/publico/consultarSeriesDeEstatisticas"
  
  if (indicador == "idp") {
    indicador_cod <- "INVESTIMENTO_DIRETO"
    grupo_indicador <- "EXTERNO_GRUPO"
  }
  
  else if (indicador == "ipca") {
    indicador_cod <- "IPCA"
    grupo_indicador <- "INDICE_PRECOS_GRUPO"
  }
  
  else if (indicador == "resultado_primario") {
    indicador_cod <- "RESULTADO_PRIMARIO"
    grupo_indicador <- "FISCAL_GRUPO"
  }
  
  else if (indicador == "divida_liquida") {
    indicador_cod <- "DIVIDA_LIQUIDA"
    grupo_indicador <- "FISCAL_GRUPO"
  }
  
  else if (indicador == "selic") {
    indicador_cod <- "META_TAXA_SELIC"
    grupo_indicador <- "TAXAS_GRUPO"
  }
  
  else if (indicador == "cambio") {
    indicador_cod <- "TAXA_CAMBIO"
    grupo_indicador <- "TAXAS_GRUPO"
  }
  
  else if (indicador == "pib_total") {
    indicador_cod <- "PIB_TOTAL"
    grupo_indicador <- "PIB_GRUPO"
  }
  
  else if (indicador == "pib_agro") {
    indicador_cod <- "PIB_AGROPECUARIA"
    grupo_indicador <- "PIB_GRUPO"
  }
  
  else if (indicador == "pib_industria") {
    indicador_cod <- "PIB_INDUSTRIA"
    grupo_indicador <- "PIB_GRUPO"
  }
  
  else if (indicador == "pib_servicos") {
    indicador_cod <- "PIB_SERVICOS"
    grupo_indicador <- "PIB_GRUPO"
  }
  
  # Atenção à sintaxe diferente usada na função glue. Usei essa maracutaia
  # porque estava tendo dificuldade de incluir uma lista dentro de outra lista
  # A solução abaixo foi a primeira solução que encontrei - o uso de 
  # parênteses no lugar de chaves como indicador de variáveis.
  
  body <- glue::glue('{"dataInicio":"(data_inicial)",
  "dataFim":"(data_final)",
  "periodo":"ANUAL",
  "consultaIndicadoresDescontinuados":false,
  "grupoIndicador":"(grupo_indicador)",
  "codigosIndicadores":["(indicador_cod)"],
  "tipoEstatistica":"MEDIANA",
  "baseCalculoEstatistica":"TRINTA_DIAS"}', 
                     .open = "(",
                     .close = ")")
  
  
  req <- httr2::request(url) %>% 
    httr2::req_body_raw(body, type = "application/json") %>%
    httr2::req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/92.0.4515.107 Safari/537.36") %>% 
    httr2::req_headers(Referer = "https://www3.bcb.gov.br/expectativas2/",
                       `Content-Type` = 'application/json',
                       `Origin` = 'https://www3.bcb.gov.br')
  resp <- req %>% 
    httr2::req_perform()
  
  df <- resp %>% 
    httr2::resp_body_json()
  
  resultado <- df[[1]]$posicoes[[1]]$detalhes[[1]] %>% 
    tibble::enframe() %>% 
    filter(name == "estatisticas") %>%
    unnest_longer(col = value) %>% 
    unnest_wider(value) %>% 
    unnest(periodoReferencia) %>% 
    filter(map_lgl(periodoReferencia, is.integer)) %>% 
    unnest(periodoReferencia) %>% 
    mutate(indicador = indicador)

  Sys.sleep(.5)
  
  resultado

}

expectativas <- map_df(indicadores_expectativas, get_expectativa)

expectativas$periodoReferencia

expectativas %>% 
  filter(indicador == "cambio") %>% 
  mutate(periodoReferencia = as.character(periodoReferencia)) %>% 
  ggplot() +
  geom_point(aes(data, valorMediana, color = periodoReferencia))
