
#Bibliotecas

library(ggplot2)
library(tseries)
library(gridExtra)
library(dplyr)

#Chamando as bases de dados

setwd("C:/Users/luiz1/OneDrive/Área de Trabalho/TCC - CAFÉ/Base de dados e códigos")
RE <- read_xlsx("Receita.xlsx")
VO <- read_xlsx("Volume.xlsx")
PM <- read_xlsx("Preço Médio.xlsx")

#Análise Exploratória

generate_statistics_and_plots <- function(data) {
  # Verificar se a entrada é um dataframe
  if (!is.data.frame(data)) {
    stop("O argumento fornecido não é um dataframe.")
  }
  
  # Lista para armazenar gráficos
  plots <- list()
  
  # Percorrer cada coluna do dataframe
  for (colname in names(data)) {
    column <- data[[colname]]
    
    # Verificar se a coluna é numérica
    if (is.numeric(column)) {
      cat("Estatísticas para a coluna:", colname, "\n")
      
      # Calcular estatísticas descritivas
      summary_stats <- summary(column)
      sd_value <- sd(column, na.rm = TRUE)
      cv <- sd_value / mean(column, na.rm = TRUE)
      p99 <- quantile(column, 0.99, na.rm = TRUE)
      p2 <- quantile(column, 0.02, na.rm = TRUE)
      
      # Imprimir estatísticas descritivas
      print(summary_stats)
      cat("Desvio Padrão (SD):", sd_value, "\n")
      cat("Coeficiente de variação (CV):", cv, "\n")
      cat("Percentil 99:", p99, "\n")
      cat("Percentil 2:", p2, "\n")
      
      # Criar histograma
      hist_plot <- ggplot(data, aes_string(x = colname)) +
        geom_histogram(binwidth = diff(range(column, na.rm = TRUE))/30, fill = "blue", color = "black", alpha = 0.7) +
        labs(title = paste("Histograma da coluna:", colname), x = colname, y = "Frequência") +
        theme_minimal()
      
      # Criar boxplot
      box_plot <- ggplot(data, aes_string(y = colname)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(title = paste("Boxplot da coluna:", colname), y = colname) +
        theme_minimal()
      
      # Adicionar gráficos à lista
      plots[[paste(colname, "hist", sep = "_")]] <- hist_plot
      plots[[paste(colname, "box", sep = "_")]] <- box_plot
      
      cat("\n\n")  # Adicionar separador entre as colunas
    }
  }
  
  # Exibir os gráficos em um grid
  grid.arrange(grobs = plots, ncol = 2)
}

generate_statistics_and_plots(RE)
generate_statistics_and_plots(VO)
generate_statistics_and_plots(PM)


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
