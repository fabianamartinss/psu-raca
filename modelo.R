
# 3.  Resultados

# The variables are age and age squared, marital status -> se tem conjuge no domicilio, -> household position (chefe ou não),
# schooling level (anos de estudo), the presence of a child, the number of children up to six or between
# seven and fourteen years old, region, quarter, and year.
# like //industry --> setor de atividade, occupation, firm size --> dummy, //tenure -> tempo no emprego, and formality status.

#--------------------------------------------------------------------------------
## 3.1  Modelos por raça
#--------------------------------------------------------------------------------

### 3.1.1  Dividindo a base entre negros e n-negros

## Negros
negros <- dados_sp_filtered %>%
  filter(negros == 1,
         salario_hora > 0,
         formal == 1 | informal == 1) %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T))

negros[is.na(negros) | negros=="Inf"] <- NA

## N-Negros
n_negros <- dados_sp_filtered %>%
  filter(negros == 0,
         salario_hora > 0,
         formal == 1 | informal == 1) %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T))

n_negros[is.na(n_negros) | n_negros=="Inf"] <- NA

### 3.1.2 Negros

## Transformando dados p/ formato painel
panel_negros <- pdata.frame(negros, index = c("cod_nasc", "entrevista"), row.names = F)

levels(panel_negros$ocupacao)
ax <- c("5","6","7","8","9","3","4","11","1","2")

## Modelo (negros)
plm_negros <- plm(ln_salario_hora ~ idade+
                                    I(idade^2)+
                                    factor(sexo)+
                                    factor(casado)+
                                    anos_estudo+
                                    qtd_pessoas_recount+
                                    qtd_filhos+
                                    factor(trimestre)+
                                    factor(ano)+
                                    factor(ocupacao, levels = ax)+
                                    factor(tam_empresa)+
                                    factor(formal)+
                                    factor(informal)+ 
                                    factor(chefe)+ 
                                    factor(atividade)+
                                    tempo_trabalho_cont+ 
                                    factor(urbano), 
                  data = panel_negros, weights = peso_domicilio, model = "pooling")
summary(plm_negros)

### 3.1.3 N-Negros

## Transformando dados p/ formato painel
panel_n_negros <- pdata.frame(n_negros, index = c("cod_nasc", "entrevista"), row.names = FALSE)

## Modelo (n-negros)
plm_n_negros <- plm(ln_salario_hora ~ idade+
                                      I(idade^2)+
                                      factor(casado)+
                                      factor(sexo)+
                                      anos_estudo+
                                      qtd_pessoas_recount+
                                      qtd_filhos+
                                      factor(trimestre)+
                                      factor(ano)+
                                      factor(ocupacao, levels = ax)+
                                      factor(tam_empresa)+
                                      factor(formal)+ 
                                      factor(informal)+ 
                                      factor(chefe)+ 
                                      factor(atividade)+
                                      tempo_trabalho_cont+ 
                                      factor(urbano),
                    data = panel_n_negros, weights = peso_domicilio, model = "pooling")
summary(plm_n_negros)
plmtest(plm_negros, effect = 'twoways', type = 'bp')

#--------------------------------------------------------------------------------
## 3.2 Modelos por raça e sexo
#--------------------------------------------------------------------------------

## 3.2.1  Dividindo a base entre negros, n-negros e sexo

## Mulheres negras
f_negros <- dados_sp_filtered %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T)) %>%
  filter(negros == 1,
         sexo == 'F',
         salario_hora > 0)

f_negros[is.na(f_negros) | f_negros=="Inf"] <- NA

## Homens negros
m_negros <- dados_sp_filtered %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T)) %>%
  filter(negros == 1,
         sexo == 'M',
         salario_hora > 0)

m_negros[is.na(m_negros) | m_negros=="Inf"] <- NA

## Mulheres n-negras
f_n_negros <- dados_sp_filtered %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T)) %>%
  filter(negros == 0,
         sexo == 'F',
         salario_hora > 0)

f_n_negros[is.na(f_n_negros) | f_n_negros=="Inf"] <- NA

## Homens n-negros
m_n_negros <- dados_sp_filtered %>%
  mutate(ln_salario_hora = ln(salario_hora, na.rm = T)) %>%
  filter(negros == 0,
         sexo == 'M',
         salario_hora > 0)

m_n_negros[is.na(m_n_negros) | m_n_negros=="Inf"] <- NA

#### 3.2.2 Mulheres negras

## Transformando dados p/ formato painel
panel_f_negros <- pdata.frame(f_negros, index = c("cod_nasc", "entrevista"), row.names = FALSE)

## Modelo (mulheres negras)
plm_f_negros <- plm(ln_salario_hora ~ idade+
                                      I(idade^2)+
                                      factor(casado)+
                                      anos_estudo+
                                      qtd_pessoas_recount+
                                      qtd_filhos+
                                      factor(trimestre)+
                                      factor(ano)+
                                      factor(ocupacao, levels = ax)+
                                      factor(tam_empresa)+
                                      factor(formal)+
                                      factor(informal)+
                                      factor(chefe)+
                                      factor(atividade)+
                                      tempo_trabalho_cont+
                                      factor(urbano),
                    data = panel_f_negros, weights = peso_domicilio, model = "pooling")
summary(plm_f_negros)

#### 3.2.3 Homens negros

## Transformando dados p/ formato painel
panel_m_negros <- pdata.frame(m_negros, index = c("cod_nasc", "entrevista"), row.names = FALSE)

## Modelo (Homens negros)
plm_m_negros <- plm(ln_salario_hora ~ idade+
                                      I(idade^2)+
                                      factor(casado)+
                                      anos_estudo+
                                      qtd_pessoas_recount+
                                      qtd_filhos+
                                      factor(trimestre)+
                                      factor(ano)+
                                      factor(ocupacao, levels = ax)+
                                      factor(tam_empresa)+
                                      factor(formal)+
                                      factor(informal)+
                                      factor(chefe)+
                                      factor(atividade)+
                                      tempo_trabalho_cont+
                                      factor(urbano), 
                    data = panel_m_negros, weights = peso_domicilio, model = "pooling")
summary(plm_m_negros)

#### 3.2.4 Mulheres n-negras

## Transformando dados p/ formato painel
panel_f_n_negros <- pdata.frame(f_n_negros, index = c("cod_nasc", "entrevista"), row.names = FALSE)

## Modelo (Mulheres n-negras)
plm_f_n_negros <- plm(ln_salario_hora ~ idade+
                                        I(idade^2)+
                                        factor(casado)+
                                        anos_estudo+
                                        qtd_pessoas_recount+
                                        qtd_filhos+
                                        factor(trimestre)+
                                        factor(ano)+
                                        factor(ocupacao, levels = ax)+
                                        factor(tam_empresa)+
                                        factor(formal)+
                                        factor(informal)+
                                        factor(chefe)+
                                        factor(atividade)+
                                        tempo_trabalho_cont+
                                        factor(urbano), 
                      data = panel_f_n_negros, weights = peso_domicilio, model = "pooling")
summary(plm_f_n_negros)

#### 3.2.5 Homens n-negros

## Transformando dados p/ formato painel
panel_m_n_negros <- pdata.frame(m_n_negros, index = c("cod_nasc", "entrevista"), row.names = FALSE)

## Modelo (Homens n-negros)
plm_m_n_negros <- plm(ln_salario_hora ~ idade+
                                        I(idade^2)+
                                        factor(casado)+
                                        anos_estudo+
                                        qtd_pessoas_recount+
                                        qtd_filhos+
                                        factor(trimestre)+
                                        factor(ano)+
                                        factor(ocupacao, levels = ax)+
                                        factor(tam_empresa)+
                                        factor(formal)+
                                        factor(informal)+
                                        factor(chefe)+
                                        factor(atividade)+
                                        tempo_trabalho_cont+
                                        factor(urbano), 
                      data = panel_m_n_negros, weights = peso_domicilio, model = "pooling")
summary(plm_m_n_negros)

