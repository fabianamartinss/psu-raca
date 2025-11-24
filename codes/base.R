
# Prêmio salarial urbano no estado de São Paulo

#--------------------------------------------------------------------------------
## Carregando os pacotes necessários
#--------------------------------------------------------------------------------

# install.packages("pacman")
# install.packages("radiant.data")
# install.packages("plotly")
# install.packages("kdensity")
# install.packages("forestmangr")
# install.packages("plm")
# install.packages("stargazer")
# install.packages("AER")
# install.packages("survey")
# install.packages("plyr")

library(pacman)
pacman::p_load(lifecycle, tidyverse, dplyr, radiant.data, ggplot2, plotly, 
               kdensity, forestmangr, plm, stargazer, AER, survey)


# 1. Tratamento da base

## Carregar e limpar base de dados
### Aqui estou utilizando a dados_sp, base que gerei no SQL por meio da PNADC.
### Esse foi o melhor meio que encontrei até agora p/ obter os dados de uma forma
### mais simples e rápida.

#--------------------------------------------------------------------------------
## 1.1 Renomeando as colunas
#--------------------------------------------------------------------------------
dados_sp_renamed <- dados_sp %>% 
                dplyr::rename("entrevista" = "V1016",
                             "area" = "V1022",
                             "tipo_area" = "V1023",
                             "peso_domicilio" = "V1028",
                             "qtd_pessoas" = "V2001",
                             "condicao" = "V2005",
                             "sexo" = "V2007",
                             "dia" = "V2008",
                             "mes" = "V20081",
                             "ano_nasc" = "V20082",
                             "idade" = "V2009",
                             "cor" = "V2010",
                             "alfabetizacao" = "V3001",
                             "trabalhou_dinheiro" = "V4001",
                             "trabalhou_mercadoria" = "V4002",
                             "afastado" = "V4005",
                             "afastado_remunerado" = "V4007",
                             "qtd_trabalhos" = "V4009",
                             "funcao" = "V4012",
                             "n_remunerado" = "V40121",
                             "tam_empresa" = "V4018",
                             "va_tam_empresa_1" = "V40181",
                             "va_tam_empresa_2" = "V40182",
                             "va_tam_empresa_3" = "V40183",
                             "domestico_mais_de_um_dom" = "V4024",
                             "formal" = "V4029",
                             "va_rendimento_bruto_mensal" = "V4033",
                             "va_rendimento_bruto_mensal_dinheiro" = "V40331",
                             "rendimento_bruto_mensal" = "V403312",
                             "va_rendimento_bruto_mensal_mercadoria" = "V40332",
                             "rendimento_bruto_mensal_mercadoria" = "V403322",
                             "horas_trabalhadas" = "V4039",
                             "tempo_trabalho" = "V4040",
                             "va_tempo_trabalho_1" = "V40401",
                             "va_tempo_trabalho_2" = "V40402",
                             "va_tempo_trabalho_3" = "V40403",
                             "ocupacao_secundaria" = "V4043",
                             "ocupacao_secundaria_formal" = "V4048",
                             "va_rendimento_bruto_mensal_secundario" = "V4050",
                             "va_rendimento_bruto_mensal_secundario_dinheiro" = "V40501",
                             "rendimento_bruto_mensal_secundario" = "V405012",
                             "va_rendimento_bruto_mensal_secundario_mercadoria" = "V40502",
                             "rendimento_bruto_mensal_secundario_mercadoria" = "V405022",
                             "horas_trabalhadas_secundario" = "V4056",
                             "va_rendimento_bruto_mensal_outros_trabalhos" = "V4058",
                             "va_rendimento_bruto_mensal_outros_trabalhos_dinheiro" = "V40581",
                             "rendimento_bruto_mensal_outros_trabalhos" = "V405812",
                             "va_rendimento_bruto_mensal_outros_trabalhos_mercadoria" = "V40582",
                             "rendimento_bruto_mensal_outros_trabalhos_mercadoria" = "V405822",
                             "horas_trabalhadas_outros_trabalhos" = "V4062",
                             "anos_estudo" = "VD3005",
                             "forca_trabalho" = "VD4001",
                             "ocupados" = "VD4002",
                             "forca_potencial" = "VD4003",
                             "posicao" = "VD4009",
                             "atividade" = "VD4010",
                             "ocupacao" = "VD4011",
                             "previdencia" = "VD4012",
                             "renda_familiar_efetiva" = "VD4020")

#--------------------------------------------------------------------------------
## 1.2 Flags
#--------------------------------------------------------------------------------

### 1.2.1 Excluindo nascimento 99/99/9999
dados_sp_renamed_2 <- dados_sp_renamed %>%
  filter(dia < 32,
         mes < 13,
         ano < 3000)

### 1.2.2  Excluindo gêmeos da base

twins_info <- dados_sp_renamed_2 %>%
  mutate(cod_nasc = paste(id_domicilio, ano, trimestre, entrevista, ano_nasc, mes, dia, sep = "_"))

n_occur <- data.frame(table(twins_info$cod_nasc))

n_occur[n_occur$Freq >1,]

mask <- n_occur[n_occur$Freq >1,]

vetor_1 <- mask$Var1[mask$Freq >1]

'%notin%' <- Negate('%in%')

twins_info_filt <- twins_info %>%
  filter(cod_nasc %notin% vetor_1)

dados_sp_renamed_3 <- twins_info_filt

### 1.2.3  Filhos até 10 e até 17 anos
  
children_info <- dados_sp_renamed_3 %>%
  mutate(filho_10y = case_when(
          condicao %in% c(4,5,6) & idade %in% c(0:10) ~ 1,
          .default = 0),
        filho_17y = case_when(
          condicao %in% c(4,5,6) & idade %in% c(11:17) ~ 1,
          .default = 0),
        h_filho_6y = case_when(
          condicao %in% c(4,5,6) & idade %in% c(0:6) ~ 1,
          .default = 0),
        h_filho_14y = case_when(
          condicao %in% c(4,5,6) & idade %in% c(7:14) ~ 1,      #vars para heckman
          .default = 0),
        h_filhos_ativos = case_when(
          condicao %in% c(4,5,6) & idade %in% c(18:65) ~ 1,
          .default = 0),
  ) %>%
  group_by(ano, id_domicilio, entrevista, trimestre) %>%
  summarise(qtd_filho_10y = sum(filho_10y),
            qtd_filho_17y = sum(filho_17y),
            h_qtd_filho_6y = sum(h_filho_6y),
            h_qtd_filho_14y = sum(h_filho_14y),      #vars para heckman
            h_qtd_filhos_ativos = sum(h_filhos_ativos)
            )

### 1.2.3  Cônjuges

married_info <- dados_sp_renamed_3 %>%
  mutate(pres_conj = case_when(
                  condicao %in% c(2,3) ~ 1,
                  .default = 0)
         ) %>%
  group_by(ano, id_domicilio, entrevista, trimestre) %>%
  reframe(qtd_conj = sum(pres_conj))

### 1.2.4  Domicílios com mais de um chefe de família

dom_info <- dados_sp_renamed_3 %>%
  mutate(chefe = case_when(condicao == 1 ~1,
                           .default = 0)) %>%
  group_by(ano, id_domicilio, entrevista, trimestre) %>%
  summarise(qtd_chefe = sum(chefe))


### 1.2.5  Recontando número de pessoas no domicílio

recount <- dados_sp_renamed_3 %>%
  group_by(ano, id_domicilio, entrevista, trimestre) %>%
  summarise(qtd_pessoas_recount = n())

#--------------------------------------------------------------------------------
## 1.3 Acrescentando informações de raça, área etc
#--------------------------------------------------------------------------------

dados_sp_adjusted <- dados_sp_renamed_3 %>%
  mutate(
    #dummy urbano
  urbano = case_when(
    tipo_area %in% c(1,2) ~ 1,
    .default = 0
  ),
    #dummy negros
  negros = case_when(
    cor %in% c(2,4) ~ 1,
    .default = 0
  ),
    #dummy brancos
  brancos = case_when(
    !(cor %in% c(2,4)) ~ 1,
    .default = 0
  ),
    #dummy não-urbano
  n_urbano = case_when(
    !(tipo_area %in% c(1,2)) ~ 1,
    .default = 0
  ),
    #salário bruto
  salario_bruto = rowSums(across(c(rendimento_bruto_mensal,
                                   rendimento_bruto_mensal_mercadoria,
                                   rendimento_bruto_mensal_secundario,
                                   rendimento_bruto_mensal_secundario_mercadoria,
                                   rendimento_bruto_mensal_outros_trabalhos,
                                   rendimento_bruto_mensal_outros_trabalhos_mercadoria)), na.rm = TRUE),
    #horas trabalhadas semanalmente 
  horas_trabalhadas_semanal = rowSums(across(c(horas_trabalhadas,
                                               horas_trabalhadas_secundario,
                                               horas_trabalhadas_outros_trabalhos)), na.rm = TRUE),
    #salário/hora
  salario_hora = salario_bruto/(horas_trabalhadas*4),
  formal = case_when(
    formal == 1 ~ 1,
    .default = 0
  ),
    #informalidade
  informal = case_when(ocupacao %in% c(2,4) ~ 1,
                       ocupacao %in% c(8,9) & previdencia ==2 ~ 1,
                       .default = 0),
    #tempo de trabalho
  tempo_trabalho_cont = case_when(
    tempo_trabalho == 1 ~ 1,
    tempo_trabalho == 2 ~ va_tempo_trabalho_1,
    tempo_trabalho == 3 ~ va_tempo_trabalho_2+12,
    tempo_trabalho == 4 ~ va_tempo_trabalho_3*12
  )
  )
#--------------------------------------------------------------------------------
## 1.4 Acrescentando informação sobre filhos e qtd pessoas por estrato a base
#--------------------------------------------------------------------------------
dados_sp_joined <- dados_sp_adjusted %>%
                      left_join(children_info,
                                by = c('ano', 'trimestre', 'entrevista', 'id_domicilio')) %>%
                      mutate(
                        filho_10y = case_when(
                        qtd_filho_10y > 0 ~ 1,
                        .default = 0
                      ),
                        filho_17y = case_when(
                          qtd_filho_17y > 0 ~ 1,
                          .default = 0
                        ),
                        filho_6y = case_when(
                          h_qtd_filho_6y > 0 ~ 1,
                          .default = 0
                        ),
                      filho_14y = case_when(
                        h_qtd_filho_14y > 0 ~ 1,
                        .default = 0
                      ),
                      filhos_ativos = case_when(
                        h_qtd_filhos_ativos > 0 ~ 1,
                        .default = 0
                      )
                      ) %>%
                      left_join(married_info,
                                by = c('ano', 'trimestre', 'entrevista', 'id_domicilio')) %>%
                      mutate(
                        casado = case_when(
                          qtd_conj > 0 & condicao %in% c(1,2,3) ~ 1,
                          .default = 0
                        )
                      ) %>%
                      left_join(dom_info,
                                by = c('ano', 'trimestre', 'entrevista', 'id_domicilio')) %>%
                      filter(qtd_chefe<2) %>%
                      left_join(recount,
                                by = c('ano', 'trimestre', 'entrevista', 'id_domicilio'))

#--------------------------------------------------------------------------------
## 1.5 Variáves p/ heckman
#--------------------------------------------------------------------------------

dados_sp_heckman <- dados_sp_joined %>%
                      mutate(qtd_filhos = qtd_filho_10y+qtd_filho_17y,
                             filhos = case_when(
                               filho_10y == 1 |
                                 filho_17y == 1 ~ 1,
                               .default = 0
                             ),
                             sexo = case_when(
                               sexo == 1 ~ "M",
                               sexo == 2 ~ "F"
                             ),
                             chefe = case_when(
                               condicao == 1 ~ 1,
                               .default = 0
                             ),
                             h_chefe_ou_casado = case_when(
                               chefe == 1 |
                                 casado == 1 ~ 1,
                               .default = 0
                             ),
                             chefe_ocupado = case_when(
                               h_chefe_ou_casado == 1 &
                                 ocupados == 1 ~ 1,
                               .default = 0
                             ),
                             
                      )

#--------------------------------------------------------------------------------
## 1.5 Filtrando amostra de acordo com paper
#--------------------------------------------------------------------------------

dados_sp_filtered <- dados_sp_joined %>%
                      filter(ano %in% (2012:2019),
                             idade %in% (18:55),
                             posicao %notin% c(3,4,5,6,7,10), #excluindo militares, servidores, trab doméstico e auxiliar familiar
                             !(ocupacao == 10), 
                             atividade %notin% c(8,11),
                             !is.na(tam_empresa),
                             va_rendimento_bruto_mensal == 1) %>%
                      filter(trabalhou_dinheiro == 1 |
                               trabalhou_mercadoria == 1 |
                               afastado_remunerado == 1) 

summary(dados_sp_filtered)
