
# 2.  Análise exploratória

#--------------------------------------------------------------------------------
## 2.1  Análise descritiva dos dados
#--------------------------------------------------------------------------------
### 2.1.1  Cor, MA/NMA

analysis_1 <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
    group_by(negros, urbano) %>%
  summarise(qtd_f = sum(sexo=="F"),
            qtd_m = sum(sexo=="M"),
            female = weighted.mean(sexo == "F", peso_domicilio),
            male = weighted.mean(sexo == "M", peso_domicilio),
            casados_perc = weighted.mean(casado == 1, peso_domicilio),
            idade_media = weighted.mean(idade, peso_domicilio),
            idade_sd = weighted.sd(idade, peso_domicilio),
            escolaridade = weighted.mean(anos_estudo, peso_domicilio),
            qtd_pessoas_media = weighted.mean(qtd_pessoas, peso_domicilio),
            qtd_pessoas_sd = weighted.sd(qtd_pessoas, peso_domicilio),
            qtd_pessoas_recount_media = weighted.mean(qtd_pessoas_recount, peso_domicilio),
            qtd_pessoas_recount_sd = weighted.sd(qtd_pessoas_recount, peso_domicilio),
            perc_filhos_10y = weighted.mean(filho_10y == 1, peso_domicilio),
            perc_filhos_17y = weighted.mean(filho_17y == 1, peso_domicilio),
            salario_hora_media = weighted.mean(salario_hora, peso_domicilio),
            salario_hora_sd = weighted.sd(salario_hora, peso_domicilio),
            renda_familiar_media = weighted.mean(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            renda_familiar_sd = weighted.sd(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            formalidade = weighted.mean(formal == 1, peso_domicilio, na.rm = T),
            horas_trabalhadas_semanal_media = weighted.mean(horas_trabalhadas_semanal, peso_domicilio),
            horas_trabalhadas_semanal_sd = weighted.sd(horas_trabalhadas_semanal, peso_domicilio))

### 2.1.2  Cor, MA/NMA e sexo

analysis_2 <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
  group_by(negros, urbano, sexo) %>%
  summarise(qtd = n(),
            casados_perc = weighted.mean(casado == 1, peso_domicilio),
            idade_media = weighted.mean(idade, peso_domicilio),
            idade_sd = weighted.sd(idade, peso_domicilio),
            escolaridade = weighted.mean(anos_estudo, peso_domicilio),
            qtd_pessoas_recount_media = weighted.mean(qtd_pessoas_recount, peso_domicilio),
            qtd_pessoas_recount_sd = weighted.sd(qtd_pessoas_recount, peso_domicilio),
            perc_filhos_10y = weighted.mean(filho_10y == 1, peso_domicilio),
            perc_filhos_17y = weighted.mean(filho_17y == 1, peso_domicilio),
            salario_hora_media = weighted.mean(salario_hora, peso_domicilio),
            salario_hora_sd = weighted.sd(salario_hora, peso_domicilio),
            renda_familiar_media = weighted.mean(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            renda_familiar_sd = weighted.sd(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            formalidade = weighted.mean(formal == 1, peso_domicilio, na.rm = T),
            horas_trabalhadas_semanal_media = weighted.mean(horas_trabalhadas_semanal, peso_domicilio),
            horas_trabalhadas_semanal_sd = weighted.sd(horas_trabalhadas_semanal, peso_domicilio))

### 2.1.3  Pop. completa

total_analysis <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
  summarise(qtd_f = sum(sexo=="F"),
            qtd_m = sum(sexo=="M"),
            female = weighted.mean(sexo == "F", peso_domicilio),
            male = weighted.mean(sexo == "M", peso_domicilio),
            casados_perc = weighted.mean(casado == 1, peso_domicilio),
            idade_media = weighted.mean(idade, peso_domicilio),
            idade_sd = weighted.sd(idade, peso_domicilio),
            escolaridade = weighted.mean(anos_estudo, peso_domicilio),
            qtd_pessoas_media = weighted.mean(qtd_pessoas, peso_domicilio),
            qtd_pessoas_sd = weighted.sd(qtd_pessoas, peso_domicilio),
            qtd_pessoas_sd = weighted.sd(qtd_pessoas, peso_domicilio),
            qtd_pessoas_recount_media = weighted.mean(qtd_pessoas_recount, peso_domicilio),
            qtd_pessoas_recount_sd = weighted.sd(qtd_pessoas_recount, peso_domicilio),
            perc_filhos_10y = weighted.mean(filho_10y == 1, peso_domicilio),
            perc_filhos_17y = weighted.mean(filho_17y == 1, peso_domicilio),
            salario_hora_media = weighted.mean(salario_hora, peso_domicilio),
            salario_hora_sd = weighted.sd(salario_hora, peso_domicilio),
            renda_familiar_media = weighted.mean(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            renda_familiar_sd = weighted.sd(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            formalidade = weighted.mean(formal == 1, peso_domicilio, na.rm = T),
            horas_trabalhadas_semanal_media = weighted.mean(horas_trabalhadas_semanal, peso_domicilio),
            horas_trabalhadas_semanal_sd = weighted.sd(horas_trabalhadas_semanal, peso_domicilio))

### 2.1.4  Pop. por sexo

total_analysis_by_sex <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
  group_by(sexo) %>%
  summarise(qtd = n(),
            casados_perc = weighted.mean(casado == 1, peso_domicilio),
            idade_media = weighted.mean(idade, peso_domicilio),
            idade_sd = weighted.sd(idade, peso_domicilio),
            escolaridade = weighted.mean(anos_estudo, peso_domicilio),
            qtd_pessoas_media = weighted.mean(qtd_pessoas, peso_domicilio),
            qtd_pessoas_sd = weighted.sd(qtd_pessoas, peso_domicilio),
            qtd_pessoas_recount_media = weighted.mean(qtd_pessoas_recount, peso_domicilio),
            qtd_pessoas_recount_sd = weighted.sd(qtd_pessoas_recount, peso_domicilio),
            perc_filhos_10y = weighted.mean(filho_10y == 1, peso_domicilio),
            perc_filhos_17y = weighted.mean(filho_17y == 1, peso_domicilio),
            salario_hora_media = weighted.mean(salario_hora, peso_domicilio),
            salario_hora_sd = weighted.sd(salario_hora, peso_domicilio),
            renda_familiar_media = weighted.mean(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            renda_familiar_sd = weighted.sd(renda_familiar_efetiva, peso_domicilio, na.rm = T),
            formalidade = weighted.mean(formal == 1, peso_domicilio, na.rm = T),
            horas_trabalhadas_semanal_media = weighted.mean(horas_trabalhadas_semanal, peso_domicilio),
            horas_trabalhadas_semanal_sd = weighted.sd(horas_trabalhadas_semanal, peso_domicilio))
#--------------------------------------------------------------------------------
## 3.1  Distribuição da população
#--------------------------------------------------------------------------------
### 3.1.1 Adicionando dummy por grupo 

base_dummies <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
  mutate(Grupo = case_when(
    negros == 0 & urbano == 0 ~ 'NN_NMA',
    negros == 0 & urbano == 1 ~ 'NN_MA',
    negros == 1 & urbano == 0 ~ 'N_NMA',
    negros == 1 & urbano == 1 ~ 'N_MA',
  ),
  grupo_sex = case_when(
    negros == 0 & urbano == 0 & sexo == 'F' ~ 'NN_NMA_F',
    negros == 0 & urbano == 1 & sexo == 'F' ~ 'NN_MA_F',
    negros == 1 & urbano == 0 & sexo == 'F' ~ 'N_NMA_F',
    negros == 1 & urbano == 1 & sexo == 'F' ~ 'N_MA_F',
    negros == 0 & urbano == 0 & sexo == 'M' ~ 'NN_NMA_M',
    negros == 0 & urbano == 1 & sexo == 'M' ~ 'NN_MA_M',
    negros == 1 & urbano == 0 & sexo == 'M' ~ 'N_NMA_M',
    negros == 1 & urbano == 1 & sexo == 'M' ~ 'N_MA_M'
  ))

mulheres <- base_dummies %>%
  filter(grupo_sex %in% c('N_MA_F', 'NN_MA_F', 'NN_NMA_F', 'N_NMA_F'))

homens <- base_dummies %>%
  filter(grupo_sex %in% c('N_MA_M', 'NN_MA_M', 'NN_NMA_M', 'N_NMA_M'))

### 3.1.2 Gráfico k-density

## Por cor/raça
graf_5 <- ggplot(data = base_dummies, aes(x = ln(salario_hora), fill = Grupo)) +
  geom_density(fill = NA,
               color = 'gray30',
               aes(linetype = Grupo)) +
  #xlab("Não-negros (NN) vs. Negros (N)") + 
  xlab("Ln(hourly wage)") +
  theme_classic()
ggplotly(graf_5)

# ## Por sexo e cor/raça
# 
# ## Female
# graf_f <- ggplot(data = mulheres, aes(x = ln(salario_hora), fill = mulheres)) +
#   geom_density(fill = NA,
#                color = 'gray30',
#                aes(linetype = mulheres)) +
#   #xlab("Não-negros (NN) vs. Negros (N)") + 
#   xlab("ln(Salário/Hora)") +
#   theme_classic()
# ggplotly(graf_f)
# 
# ## Male
# graf_m <- ggplot(data = homens, aes(x = ln(salario_hora), fill = grupo_sex)) +
#   geom_density(fill = NA,
#                color = 'gray30',
#                aes(linetype = grupo_sex)) +
#   #xlab("Não-negros (NN) vs. Negros (N)") + 
#   xlab("ln(Salário/Hora)") +
#   theme_classic()
# ggplotly(graf_m)
