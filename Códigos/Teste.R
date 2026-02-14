library(forecast)

# 2. Criar a variável dummy COVID
meses <- time(VO_AR)
dummy_COVID <- ifelse(meses >= 2020 + 2/12, 1, -1)

# 3. Ajustar o modelo com dummy
modelo <- tslm(VO_AR ~ trend + dummies_significativas + dummy_COVID)

# 4. Resumo do modelo
summary(modelo)

# Prever os valores ajustados pelo modelo
valores_ajustados <- fitted(modelo)

# Criar o gráfico
plot(VO_AR, col = "blue", lwd = 2, ylab = "Valor", xlab = "Tempo", 
     main = "Série Temporal: Dados Observados vs. Modelo Ajustado")
lines(valores_ajustados, col = "red", lwd = 2)

# Adicionar legenda
legend("topleft", legend = c("Dados Observados", "Modelo Ajustado"),
       col = c("blue", "red"), lwd = 2, bty = "n")


# 5. Prever os valores ajustados
valores_ajustados <- fitted(modelo)



# 4. Combinar as dummies sazonais e a dummy COVID
dummies_combinadas <- cbind(dummies_mes, dummy_COVID = dummy_COVID)

# Visualizar as dummies combinadas
head(dummies_combinadas)


modelo <- tslm(VO_AR ~ trend + dummies_combinadas)




VO_A$t = seq_len(nrow(VO_A))
VO_A$Z = ifelse(VO_A$`Mês/Ano` >= 2020 + 2/12, 1, -1)

VO_AR$``



VO_A <- VO %>% select(Arábica,`Mês/Ano`)


