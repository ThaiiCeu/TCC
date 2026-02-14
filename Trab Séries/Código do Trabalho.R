
# Trabalho de Séries Temporais
# Thaii Céu

#Bibliotecas usadas

library(ggplot2)
library(tseries)
library(forecast)
library(lmtest)

#Séries de Exportação de café (Arábica) : Variável Volume

head(Volume)

Volume$`Mês/Ano` = as.Date(Volume$`Mês/Ano`)

VA = ggplot(Volume, aes(x = `Mês/Ano`, y = `Arábica`)) +
  geom_line() + 
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  labs(title = "Série Temporal do Volume (Arábica)", x = "Ano", y = "Volume (Arábica)") + 
  theme_minimal() 
VA

#Testando se é estacionária pelo teste e também pelo modelo

adf.test(Volume$Arábica) # É Estacionária

model = lm(Volume$Arábica ~ Volume$`Mês/Ano`)
summary(model) # Tambem deu estacionária

#Arrumando a série para jan2019 A jun2023

Vol <- subset(Volume, `Mês/Ano` >= as.Date("2019-01-01") & `Mês/Ano` <= as.Date("2023-07-01"))

# Criando o gráfico da série de jan2019 até jun2023
VA1 = ggplot(Vol, aes(x = `Mês/Ano`, y = Arábica)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  labs(title = "Série Temporal de jan2019 a jun2023 (Volume Arábica)", x = "Data", y = "Volume(Arábica)") +
  theme_minimal() 

VA1

#Testando se a série continua estácionaria (Por modelo e teste)

adf.test(Vol$Arábica) #Não é estacionária por 0,00552

model1 = lm(Vol$Arábica ~ Vol$`Mês/Ano`)

summary(model1) # Deu estácionaria!! 

#Identificando modelos

acf(Vol$Arábica)
pacf(Vol$Arábica)

model_arima_100 = auto.arima(Vol$Arábica)

model_arima_100 # auto.arima
model_arima_201 <- Arima(Vol$Arábica, order=c(2,0,1))

#Estimativas dos parâmetros dos modelos e Ruidos Brancos

coef100 = model_arima_100$coef
coef201 = model_arima_201$coef

coeftest(model_arima_100)
coeftest(model_arima_201)

# Testes Residuais

checkresiduals(model_arima_100, lag = 36)
checkresiduals(model_arima_201, lag = 36)

qqnorm(res100, main="Q-Q Plot dos Resíduos ARIMA(1,0,0)")
qqline(res100, col="red")

qqnorm(res201, main="Q-Q Plot dos Resíduos ARIMA(2,0,1)")
qqline(res201, col="red")

shapiro.test(res100)
shapiro.test(res201)

jarque.bera.test(res100)
jarque.bera.test(res201)

#Teste do modelo

Box.test(res100, lag = 36, type = "Ljung-Box")
Box.test(res201, lag = 36, type = "Ljung-Box")

tsdiag(model_arima_100, gof.lag = 36)
tsdiag(model_arima_201, gof.lag = 36)


# Previsões

prev100 <- forecast(model_arima_100, h=6)
plot(prev100, main="Previsão com o Melhor Modelo ARIMA para os Próximos 6 Meses")

prev201 <- forecast(model_arima_201, h=6)
plot(prev201, main="Previsão com o Melhor Modelo ARIMA para os Próximos 6 Meses")

# Criando um gráfico para juntar as previsões e comparar com os dados reais

VA_6valors = tail(Volume$Arábica, 6)
VA_6valors

prev_valor100 = prev100$mean
prev_valor201 = prev201$mean

data_ts <- ts(Vol$Arábica, frequency=12)

forecast_df <- data.frame(
  Time = seq(length(data_ts) + 1, length(data_ts) + 6),
  Modelo = rep(c("Modelo 1", "Modelo 2", "Dados"), each=6),
  Previsao = c(prev_valor100, prev_valor201, VA_6valors)
)

ggplot(forecast_df, aes(x=Time, y=Previsao, color=Modelo)) +
  geom_line() +
  geom_point() +
  labs(title="Comparação das Previsões dos Modelos", x="Tempo", y="Previsões") +
  theme_minimal()

# Intervalo de Confiança

LI1 = prev100$lower
LS1 = prev100$upper

LI2 = prev201$lower 
LS2 = prev201$upper

#EQMMP, AIC e BIC

erro100 = VA_6valors - prev_valor100
erro201 = VA_6valors - prev_valor201

EQMMP100 = (sum((erro100)^2)) / 6
EQMMP201 = (sum((erro201)^2)) / 6

aic_values <- c(AIC(model_arima_100), AIC(model_arima_201))
bic_values <- c(BIC(model_arima_100), BIC(model_arima_201))

