setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/TCC - ESTAT/Base de dados e códigos")
RE <- read_xlsx("Receita.xlsx")
VO <- read_xlsx("Volume.xlsx")
PM <- read_xlsx("Preço Médio.xlsx")

######################################## Análise Exploratória ###########################################

## Arrumando a coluna Mês/ano

VO$`Mês/Ano` = as.Date(VO$`Mês/Ano`)
PM$`Mês/Ano` = as.Date(PM$`Mês/Ano`)
RE$`Mês/Ano`= as.Date(RE$`Mês/Ano`)



############################################### MODELOS #################################################

espectro = spectrum(PM$Conillon)
abline(v=seq(0,0.5,0.01), lty="dotted")
indice_max <- which.max(espectro$spec)
frequencia_max <- espectro$freq[indice_max]

1 / frequencia_max

adf.test(PM$Conillon) # É Estacionária

VO

model = lm(PM$Conillon ~ PM$`Mês/Ano`) # Aqui está nos afirmando que é estacionária
summary(model)

# Número de harmônicas
K <- 6

# Transformar a série em ts
PM_CO <- PM %>% select(Conillon)
PM_CO <- ts(PM_CO, start = c(2015, 1), frequency = 96)

# Gerar as harmônicas
harmonicas <- fourier(PM_CO, K = K)

# Ajustar o modelo inicial
modelo_inicial <- tslm(PM_CO ~ I(trend^5) + harmonicas)

# Resumo do modelo
summary(modelo_inicial)

# Identificar variáveis significativas (p < 0.05)
coeficientes <- summary(modelo_inicial)$coefficients
# Obter os nomes das variáveis significativas
variaveis_significativas <- rownames(coeficientes)[coeficientes[, "Pr(>|t|)"] < 0.05]

# Filtrar as variáveis relacionadas às harmônicas
variaveis_harmonicas_significativas <- variaveis_significativas[grep("harmonicas", variaveis_significativas)]

# Ajustar os nomes para corresponder às colunas reais (mantendo o sufixo "-12")
colunas_harmonicas <- sub("harmonicas", "", variaveis_harmonicas_significativas)

colunas_harmonicas_reais <- colnames(harmonicas)
colunas_harmonicas_reais

# Filtrar diretamente as colunas significativas com sufixo "-12"
colunas_harmonicas_final <- colunas_harmonicas_reais[
  colunas_harmonicas_reais %in% colunas_harmonicas
]

# Validar as colunas selecionadas
print(colunas_harmonicas_final)

# Filtrar as harmônicas significativas
harmonicas_significativas <- harmonicas[, colunas_harmonicas_final, drop = FALSE]

# Ajustar o modelo com as harmônicas significativas
modelo_final_harmonica <- tslm(PM_CO ~ I(trend^5) + harmonicas_significativas)


# Resumo do modelo final
summary(modelo_final_harmonica)

# Prever os valores ajustados pelo modelo
valores_ajustados_harmonica <- fitted(modelo_final_harmonica)

# Criar um data frame com os dados
dados_harmonica <- data.frame(
  Tempo = PM$`Mês/Ano`, 
  Observado = PM$Conillon,
  Ajustado = valores_ajustados_harmonica
)

# Transformar os dados em formato longo para o ggplot
dados_long_harmonica <- dados_harmonica %>%
  pivot_longer(cols = c("Observado", "Ajustado"), 
               names_to = "Série", 
               values_to = "Valor")


ggplot(dados_long_harmonica, aes(x = Tempo, y = Valor, color = Série)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("red", "black")) +
  labs(
    title = "Série Temporal: Dados Observados vs. Modelo Ajustado",
    x = "Tempo",
    y = "Valor",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.9), # Ajuste para simular "topleft"
    legend.background = element_rect(color = "black", size = 0.5)
  )



checkresiduals(modelo_final_harmonica, lag = 36)

qqnorm(modelo_final_harmonica$residuals, main="Q-Q Plot dos Resíduos (Harmonica)")
qqline(modelo_final_harmonica$residuals, col="red")

shapiro.test(modelo_final_harmonica$residuals)

dwtest(modelo_final_harmonica)

# Transformação Orkutt
mod_co_PMCO <- cochrane.orcutt(modelo_final_harmonica)

resumo <- data.frame(
  Coeficiente = mod_co_PMCO$coefficients,
  `Erro Padrão` = mod_co_PMCO$std.error,
  `t valor` = mod_co_PMCO$t.value,
  `p valor` = mod_co_PMCO$p.value
)

print(round(resumo, 4))

mod_co_PMCO$r.squared

res <- mod_co_PMCO$residuals
plot(res[-1], res[-length(res)], main = "Resíduo vs. Resíduo defasado",
     xlab = expression(e[t-1]), ylab = expression(e[t]))
abline(lm(res[-1] ~ res[-length(res)]), col = "red")

valores_ajustados_harmonica_co = mod_co_PMCO$fitted.values

# Criar um data frame com os dados
dados_harmonica <- data.frame(
  Tempo = PM$`Mês/Ano`, 
  Observado = PM$Conillon,
  Ajustado = valores_ajustados_harmonica_co
)

# Transformar os dados em formato longo para o ggplot
dados_long_harmonica <- dados_harmonica %>%
  pivot_longer(cols = c("Observado", "Ajustado"), 
               names_to = "Série", 
               values_to = "Valor")


ggplot(dados_long_harmonica, aes(x = Tempo, y = Valor, color = Série)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("red", "black")) +
  labs(
    title = "Série Temporal: Dados Observados vs. Modelo Ajustado (Conillon)",
    x = "Tempo",
    y = "Valor",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.9), # Ajuste para simular "topleft"
    legend.background = element_rect(color = "black", size = 0.5)
  )


#########modelo_final_harmonica############################### MODELO DUMMIES
# Criar variáveis dummy para os meses
dummies_mes <- seasonaldummy(PM_CO)  # Cria dummies sazonais (1 para cada mês)

# Ajustar o modelo inicial com todas as dummies
modelo_inicial_dummies <- tslm(PM_CO ~ trend + dummies_mes)

# Resumo do modelo inicial
summary(modelo_inicial_dummies)

# Identificar variáveis significativas (p < 0.05)
coeficientes <- summary(modelo_inicial_dummies)$coefficients
variaveis_significativas <- rownames(coeficientes)[coeficientes[, "Pr(>|t|)"] < 0.05]

# Filtrar as variáveis relacionadas às dummies
variaveis_dummies_significativas <- variaveis_significativas[grep("dummies_mes", variaveis_significativas)]

# Ajustar os nomes para corresponder às colunas reais (mantendo o sufixo "-12")
colunas_dummies <- sub("dummies_mes", "", variaveis_dummies_significativas)

colunas_dummies_reais <- colnames(dummies_mes)
colunas_dummies_reais

# Filtrar diretamente as colunas significativas com sufixo "-12"
colunas_dummies_final <- colunas_dummies_reais[
  colunas_dummies_reais %in% colunas_dummies
]

# Validar as colunas selecionadas
print(colunas_dummies_final)

# Filtrar as harmônicas significativas
dummies_significativas <- dummies_mes[, colunas_dummies_final, drop = FALSE]

# Ajustar o modelo com as harmônicas significativas
modelo_final_dummies <- tslm(VO_SO ~ trend + dummies_significativas)

# Resumo do modelo final
summary(modelo_final_dummies)

# Prever os valores ajustados pelo modelo
valores_ajustados_dummies <- fitted(modelo_inicial_dummies)


# Criar um data frame com os dados
dados_dummies <- data.frame(
  Tempo = VO$`Mês/Ano`, 
  Observado = VO$Solúvel,
  Ajustado = valores_ajustados_dummies
)

# Transformar os dados em formato longo para o ggplot
dados_long_dummies <- dados_dummies %>%
  pivot_longer(cols = c("Observado", "Ajustado"), 
               names_to = "Série", 
               values_to = "Valor")

# Gráfico do modelo x observado
ggplot(dados_long_dummies, aes(x = Tempo, y = Valor, color = Série)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("red", "black")) +
  labs(
    title = "Série Temporal: Dados Observados vs. Modelo Ajustado",
    x = "Tempo",
    y = "Valor",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.1, 0.9), # Ajuste para simular "topleft"
    legend.background = element_rect(color = "black", size = 0.5)
  )


# Checando os residuos
checkresiduals(modelo_final_dummies, lag = 36)

qqnorm(modelo_final_dummies$residuals, main="Q-Q Plot dos Resíduos (Dummies)")
qqline(modelo_final_dummies$residuals, col="red")

shapiro.test(modelo_final_dummies$residuals)

dwtest(modelo_final_dummies)

# Calcular RMSE
rmse_value <- rmse(VO$Arábica, valores_ajustados_dummies)

# Calcular MAE
mae_value <- mae(VO$Arábica, valores_ajustados_dummies)

# Calcular RMSE
rmse_value <- rmse(VO$Arábica, valores_ajustados_harmonica)

# Calcular MAE
mae_value <- mae(VO$Arábica, valores_ajustados_harmonica)
