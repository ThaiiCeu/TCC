
#Bibliotecas
library(lmtest) # Durbin Watson test
library(ggplot2) # Visuaizaçao de dados
library(tseries) # adf test
library(gridExtra) # Visualização de gráficos
library(dplyr) # Manipulação
library(readxl) # Abrir as bases
library(tidyr) # Manipulação
library(forecast) # Modelos
library(Metrics) # RMSE, MSE

#Chamando as bases de dados

setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/TCC - CAFÉ/Base de dados e códigos")
RE <- read_xlsx("Receita.xlsx")
VO <- read_xlsx("Volume.xlsx")
PM <- read_xlsx("Preço Médio.xlsx")

######################################## Análise Exploratória ###########################################

## Arrumando a coluna Mês/ano

VO$`Mês/Ano` = as.Date(VO$`Mês/Ano`)
PM$`Mês/Ano` = as.Date(PM$`Mês/Ano`)
RE$`Mês/Ano`= as.Date(RE$`Mês/Ano`)

#Gráfico das séries
plot_time_series <- function(data, columns, title = "Preço Médio dos Tipos de Café") {
  plots <- list()
  
  for (col in columns) {
    p <- ggplot(data, aes_string(x = "`Mês/Ano` ", y = col)) +
      geom_line() +
      labs(title = paste("Série Temporal:", col), x = "Data", y = "Volume") +
      theme_minimal()
    plots[[col]] <- p
  }
  
  grid.arrange(grobs = plots, ncol = 2, top = title)
} # Lembrar de mudar o title da imagem e o title da variável y!

plot_time_series(RE, c("`Conillon`", "`Arábica`", "`Solúvel`", "`Torrado`"))
plot_time_series(VO, c("`Conillon`", "`Arábica`", "`Solúvel`", "`Torrado`"))
plot_time_series(PM, c("`Conillon`", "`Arábica`", "`Solúvel`", "`Torrado`"))

# Criando os Boxplots
generate_boxplots <- function(data) {
  # Verificar se a entrada é um dataframe
  if (!is.data.frame(data)) {
    stop("O argumento fornecido não é um dataframe.")
  }
  
  # Lista para armazenar os boxplots
  box_plots <- list()
  
  # Percorrer cada coluna do dataframe
  for (colname in names(data)) {
    column <- data[[colname]]
    
    # Verificar se a coluna é numérica
    if (is.numeric(column)) {
      mean_value <- mean(column, na.rm = TRUE)  # Calcular a média
      
      # Criar boxplot
      box_plot <- ggplot(data, aes_string(y = colname)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(title = paste("Boxplot da coluna:", colname), y = colname) +
        theme_minimal() +
        geom_hline(yintercept = mean_value, color = "red", linetype = "dashed", size = 1) +  # Linha da média
        annotate("text", x = 0.0, y = mean_value + 0.1 * diff(range(column, na.rm = TRUE)), 
                 label = paste("Média:", round(mean_value, 2)), 
                 vjust = 0, color = "black")  # Anotação da média acima da linha
      
      # Adicionar o boxplot à lista
      box_plots[[colname]] <- box_plot
    }
  }
  
  # Exibir os boxplots em um grid
  grid.arrange(grobs = box_plots, ncol = 2)
}

generate_boxplots(VO)
generate_boxplots(PM)
generate_boxplots(RE)

#Criando os Histogramas
generate_histograms <- function(data) {
  # Verificar se a entrada é um dataframe
  if (!is.data.frame(data)) {
    stop("O argumento fornecido não é um dataframe.")
  }
  
  # Lista para armazenar os histogramas
  hist_plots <- list()
  
  # Percorrer cada coluna do dataframe
  for (colname in names(data)) {
    column <- data[[colname]]
    
    # Verificar se a coluna é numérica
    if (is.numeric(column)) {
      mean_value <- mean(column, na.rm = TRUE)  # Calcular a média
      
      # Criar histograma
      hist_plot <- ggplot(data, aes_string(x = colname)) +
        geom_histogram(binwidth = diff(range(column, na.rm = TRUE)) / 30, fill = "blue", color = "black", alpha = 0.7) +
        geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1) +  # Marcação da média
        labs(title = paste("Histograma da coluna:", colname), x = colname, y = "Frequência") +
        theme_minimal()
      
      # Calcular a altura máxima do histograma
      max_y <- max(ggplot_build(hist_plot)$data[[1]]$count, na.rm = TRUE) * 1.05  # Um pouco acima da altura máxima
      
      # Adicionar texto da média, ajustando a posição
      hist_plot <- hist_plot + 
        annotate("text", x = mean_value, y = 5, label = paste("Média:", round(mean_value, 2)), 
                 vjust = -0.5, color = "black")  # Adiciona texto da média acima da linha
      
      # Adicionar o histograma à lista
      hist_plots[[colname]] <- hist_plot
    }
  }
  
  # Exibir os histogramas em um grid
  grid.arrange(grobs = hist_plots, ncol = 2)
}

generate_histograms(VO)
generate_histograms(PM)
generate_histograms(RE)


# Criando os Boxplots de cada ano
criar_boxplots_cafe <- function(dados, tipos_cafe) {
  # Converter a coluna 'Mês/Ano' para o tipo Date e extrair o ano
  dados$`Mês/Ano` <- as.Date(dados$`Mês/Ano`)
  dados$Ano <- format(dados$`Mês/Ano`, "%Y")
  
  # Transformar os dados em formato longo
  dados_long <- dados %>%
    pivot_longer(cols = all_of(tipos_cafe), 
                 names_to = "Tipo", 
                 values_to = "Volume")
  
  # Lista para armazenar os gráficos
  graficos <- list()
  
  # Criar gráficos de boxplot para cada tipo de café
  for (tipo in tipos_cafe) {
    p <- ggplot(subset(dados_long, Tipo == tipo), aes(x = Ano, y = Volume, fill = Tipo)) +
      geom_boxplot() +
      labs(title = tipo, x = "Ano", y = "Preço Médio") +
      theme_minimal() +
      scale_fill_brewer(palette = "Set3")
    
    graficos[[tipo]] <- p
  }
  
  # Organizar todos os gráficos em uma única página
  do.call(grid.arrange, c(graficos, ncol = 2))  # Ajuste o número de colunas conforme necessário
}

criar_boxplots_cafe(VO, c("Conillon", "Arábica", "Solúvel", "Torrado"))
criar_boxplots_cafe(RE, c("Conillon", "Arábica", "Solúvel", "Torrado"))
criar_boxplots_cafe(PM, c("Conillon", "Arábica", "Solúvel", "Torrado"))


VO_data <- VO %>%
  mutate(mes = format(`Mês/Ano`, "%m")) %>%
  pivot_longer(cols = -c(`Mês/Ano`, mes), names_to = "Tipo_Cafe", values_to = "Quantidade")

RE_data <- RE %>%
  mutate(mes = format(`Mês/Ano`, "%m")) %>%
  pivot_longer(cols = -c(`Mês/Ano`, mes), names_to = "Tipo_Cafe", values_to = "Quantidade")

PM_data <- PM %>%
  mutate(mes = format(`Mês/Ano`, "%m")) %>%
  pivot_longer(cols = -c(`Mês/Ano`, mes), names_to = "Tipo_Cafe", values_to = "Quantidade")


# Função para agrupar e somar por mês, e gerar o gráfico de linhas
plot_cafe_linhas <- function(data, tipo_cafe) {
  # Agrupar os dados por mês e tipo de café, e calcular a soma
  dados_agrupados <- data %>%
    filter(Tipo_Cafe %in% tipo_cafe) %>%
    group_by(mes, Tipo_Cafe) %>%
    summarise(Quantidade = sum(Quantidade, na.rm = TRUE), .groups = "drop")
  
  # Gerar o gráfico de linhas
  ggplot(dados_agrupados, aes(x = mes, y = Quantidade, color = Tipo_Cafe, group = Tipo_Cafe)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    labs(
      title = paste("Preço Médio dos tipos de café (mensalmente):", paste(tipo_cafe, collapse = ", ")),
      x = "Mês",
      y = "Preço Médio",
      color = "Tipo de Café"
    ) +
    scale_x_discrete(labels = month.name) + # Nome completo dos meses
    theme_minimal()
} # Lembrar de trocar o titulo do gráfico e do eixo Y


# Fazendo o gráfico de linhas mensal
VO_CS = plot_cafe_linhas(VO_data, tipo_cafe = c("Conillon", "Solúvel")) # Arábica, Conillon e Solúvel juntos
VO_A = plot_cafe_linhas(VO_data, tipo_cafe = "Arábica")
VO_T = plot_cafe_linhas(VO_data, tipo_cafe = "Torrado") 

RE_CS = plot_cafe_linhas(RE_data, tipo_cafe = c("Conillon", "Solúvel")) # Arábica, Conillon e Solúvel juntos
RE_A = plot_cafe_linhas(RE_data, tipo_cafe = "Arábica")
RE_T = plot_cafe_linhas(RE_data, tipo_cafe = "Torrado") 

PM_CSA = plot_cafe_linhas(PM_data, tipo_cafe = c("Conillon", "Solúvel", "Arábica")) # Arábica, Conillon e Solúvel juntos
PM_T = plot_cafe_linhas(PM_data, tipo_cafe = "Torrado") 

plot_cafe_linhas(PM_data, tipo_cafe = c("Conillon", "Solúvel", "Arábica"))


grid.arrange(VO_A,VO_CS,VO_T, ncol = 1)
grid.arrange(RE_A,RE_CS,RE_T, ncol = 1)
grid.arrange(PM_T,PM_CSA, ncol = 1)


# Função para gerar o gráfico de boxplot mensal
plot_cafe_boxplot <- function(data, tipo_cafe) {
  # Filtrar os dados para o tipo de café desejado
  dados_filtrados <- data %>%
    filter(Tipo_Cafe %in% tipo_cafe)
  
  # Criar o boxplot
  ggplot(dados_filtrados, aes(x = mes, y = Quantidade, fill = Tipo_Cafe)) +
    geom_boxplot(outlier.size = 2) +
    labs(
      title = paste("Distribuição Mensal de Café:", paste(tipo_cafe, collapse = ", ")),
      x = "Mês",
      y = "Preço Médio",
      fill = "Tipo de Café"
    ) +
    scale_x_discrete(labels = month.name) + # Nome completo dos meses
    theme_minimal()
}

#Fazendo os gráficos de Boxplot mensal
VO_CS = plot_cafe_boxplot(VO_data, tipo_cafe = c("Conillon", "Solúvel")) # Arábica, Conillon e Solúvel juntos
VO_A = plot_cafe_boxplot(VO_data, tipo_cafe = "Arábica")
VO_T = plot_cafe_boxplot(VO_data, tipo_cafe = "Torrado") 

RE_CS = plot_cafe_boxplot(RE_data, tipo_cafe = c("Conillon", "Solúvel")) # Arábica, Conillon e Solúvel juntos
RE_A = plot_cafe_boxplot(RE_data, tipo_cafe = "Arábica")
RE_T = plot_cafe_boxplot(RE_data, tipo_cafe = "Torrado") 

PM_CSA = plot_cafe_boxplot(PM_data, tipo_cafe = c("Conillon", "Solúvel", "Arábica")) # Arábica, Conillon e Solúvel juntos
PM_T = plot_cafe_boxplot(PM_data, tipo_cafe = "Torrado") 

plot_cafe_boxplot(PM_data, tipo_cafe = c("Conillon", "Solúvel", "Arábica","Torrado"))


grid.arrange(VO_A,VO_CS,VO_T, ncol = 1)
grid.arrange(RE_A,RE_CS,RE_T, ncol = 1)
grid.arrange(PM_T,PM_CSA, ncol = 1)

############################################### MODELOS #################################################

adf.test(VO$Arábica) # É Estacionária

model = lm(VO$Arábica ~ VO$`Mês/Ano`) # Aqui está nos afirmando que não é estacionária
summary(model)

# Número de harmônicas
K <- 6  

# Transformar a série em ts
VO_AR <- VO %>% select(Arábica)
VO_AR <- ts(VO_AR, start = c(2015, 1), frequency = 12)

################################################## MODELO HARMÔNICA
harmonicas <- fourier(VO_AR, K = K)

# Ajustar o modelo inicial
modelo_inicial <- tslm(VO_AR ~ harmonicas)

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
modelo_final_harmonica <- tslm(VO_AR ~ trend + harmonicas_significativas)


# Resumo do modelo final
summary(modelo_final_harmonica)

# Prever os valores ajustados pelo modelo
valores_ajustados_harmonica <- fitted(modelo_final_harmonica)

# Criar um data frame com os dados
dados_harmonica <- data.frame(
  Tempo = VO$`Mês/Ano`, 
  Observado = VO$Arábica,
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

# Checando os residuos
checkresiduals(modelo_final_harmonica, lag = 36)

qqnorm(modelo_final_harmonica$residuals, main="Q-Q Plot dos Resíduos (Harmonica)")
qqline(modelo_final_harmonica$residuals, col="red")

shapiro.test(modelo_final_harmonica$residuals)

dwtest(modelo_final_harmonica)

######################################## MODELO DECOMPOSIÇÃO TEMPORAL
# Criar variáveis dummy para os meses
dummies_mes <- seasonaldummy(VO_AR)  # Cria dummies sazonais (1 para cada mês)

# Ajustar o modelo inicial com todas as dummies
modelo_inicial_dummies <- tslm(VO_AR ~ trend + dummies_mes)

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
modelo_final_dummies <- tslm(VO_AR ~ trend + dummies_significativas)

# Resumo do modelo final
summary(modelo_final_dummies)

# Prever os valores ajustados pelo modelo
valores_ajustados_dummies <- fitted(modelo_inicial_dummies)


# Criar um data frame com os dados
dados_dummies <- data.frame(
  Tempo = VO$`Mês/Ano`, 
  Observado = VO$Arábica,
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

####################################### MÉTRICAS PARA AVALIAR OS MODELOS
# Calcular RMSE
rmse_value <- rmse(VO$Arábica, valores_ajustados_dummies)

# Calcular MAE
mae_value <- mae(VO$Arábica, valores_ajustados_dummies)

# Calcular RMSE
rmse_value <- rmse(VO$Arábica, valores_ajustados_harmonica)

# Calcular MAE
mae_value <- mae(VO$Arábica, valores_ajustados_harmonica)
