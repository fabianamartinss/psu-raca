## Regressão linear
base_regressao <- dados_sp_filtered %>%
  filter(ano == 2019,
         trimestre == 4,
         entrevista == 1,
         salario_hora <= (mean(salario_hora) + 3*sd(salario_hora)),
         !is.na(salario_hora),
         !is.na(negros),
         !is.na(urbano),
         !is.na(brancos),
         !is.na(n_urbano)) %>%
  mutate(sexo = case_when(sexo == 1 ~ 1,
                          .default = 0))

## adicionando dummy

base_dummies <- dados_sp_filtered %>%
  filter(entrevista == 1) %>%
  mutate(Grupo = case_when(
    negros == 0 & urbano == 0 ~ 'NN_NMA',
    negros == 0 & urbano == 1 ~ 'NN_MA',
    negros == 1 & urbano == 0 ~ 'N_NMA',
    negros == 1 & urbano == 1 ~ 'N_MA',
  ),
  grupo_sex = case_when(
    negros == 0 & urbano == 0 & sexo == 0 ~ 'NN_NMA_F',
    negros == 0 & urbano == 1 & sexo == 0 ~ 'NN_MA_F',
    negros == 1 & urbano == 0 & sexo == 0 ~ 'N_NMA_F',
    negros == 1 & urbano == 1 & sexo == 0 ~ 'N_MA_F',
    negros == 0 & urbano == 0 & sexo == 1 ~ 'NN_NMA_M',
    negros == 0 & urbano == 1 & sexo == 1 ~ 'NN_MA_M',
    negros == 1 & urbano == 0 & sexo == 1 ~ 'N_NMA_M',
    negros == 1 & urbano == 1 & sexo == 1 ~ 'N_MA_M'
  ))

mulheres <- base_dummies %>%
  filter(grupo_sex %in% c('N_MA_F', 'NN_MA_F', 'NN_NMA_F', 'N_NMA_F'))

homens <- base_dummies %>%
  filter(grupo_sex %in% c('N_MA_M', 'NN_MA_M', 'NN_NMA_M', 'N_NMA_M'))

## gráfico densidade

graf_5 <- ggplot(data = base_dummies, aes(x = ln(salario_hora), fill = Grupo)) +
  geom_density(fill = NA,
               color = 'gray30',
               aes(linetype = Grupo)) +
  #xlab("Não-negros (NN) vs. Negros (N)") + 
  xlab("ln(Salário/Hora)") +
  theme_classic()
ggplotly(graf_5)
