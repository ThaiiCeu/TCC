cochrane.orcutt <- function(reg, convergence = 8, max.iter=100){ 
  # Verifica se é tslm e converte para lm se for necessário
  if (inherits(reg, "tslm")) {
    f <- formula(reg)
    d <- model.frame(reg)
    reg <- lm(f, data = d)
  }
  
  # Início da função original
  X <- model.matrix(reg) 
  Y <- model.response(model.frame(reg)) 
  n <- length(Y) 
  e <- reg$residuals   
  e2 <- e[-1] 
  e3 <- e[-n] 
  regP <- lm(e2 ~ e3 - 1) 
  rho <- summary(regP)$coeff[1] 
  rho2 <- c(rho) 
  
  XB <- X[-1,] - rho * X[-n,] 
  YB <- Y[-1] - rho * Y[-n] 
  regCO <- lm(YB ~ XB - 1) 
  
  ypCO <- regCO$coeff[1] + as.matrix(X[,-1]) %*% regCO$coeff[-1]   
  e1 <- ypCO - Y
  e2 <- e1[-1] 
  e3 <- e1[-n] 
  regP <- lm(e2 ~ e3 - 1)  
  rho <- summary(regP)$coeff[1] 
  rho2[2] <- rho 
  i <- 2 
  
  while (round(rho2[i-1], convergence) != round(rho2[i], convergence) && (i <= max.iter)) { 
    XB <- X[-1,] - rho * X[-n,] 
    YB <- Y[-1] - rho * Y[-n] 
    regCO <- lm(YB ~ XB - 1) 
    ypCO <- regCO$coeff[1] + as.matrix(X[,-1]) %*% regCO$coeff[-1]   
    e1 <- ypCO - Y 
    e2 <- e1[-1] 
    e3 <- e1[-n] 
    regP <- lm(e2 ~ e3 - 1) 
    rho <- summary(regP)$coeff[1]   
    i <- i + 1 
    rho2[i] <- rho 
  } 
  
  regCO$number.interaction <- i - 1 
  regCO$rho <- rho2[i - 1]
  
  if ((i - 1) >= max.iter) {
    regCO$coefficients <- NA
    regCO$DW <- c(lmtest::dwtest(reg)$statistic, lmtest::dwtest(reg)$p.value, NA, NA)
    class(regCO) <- "orcutt"
    regCO$call <- reg$call
    warning("Did not converge")
  } else {    
    regCO$DW <- c(lmtest::dwtest(reg)$statistic, lmtest::dwtest(reg)$p.value,
                  lmtest::dwtest(regCO)$statistic, lmtest::dwtest(regCO)$p.value)
    
    regF <- lm(YB ~ 1)
    tF <- anova(regCO, regF)
    regCO$Fs <- c(tF$F[2], tF$`Pr(>F)`[2])
    
    regCO$fitted.values <- model.matrix(reg) %*% (as.matrix(regCO$coeff))
    names(regCO$coefficients) <- colnames(X)
    
    regCO$std.error <- summary(regCO)$coeff[,2]
    regCO$t.value <- summary(regCO)$coeff[,3]
    regCO$p.value <- summary(regCO)$coeff[,4]       
    
    class(regCO) <- "orcutt"
    regCO$call <- reg$call    
    
    df1 <- dim(model.frame(reg))[2] - 1
    df2 <- length(regCO$residuals) - df1 - 1
    
    RSS <- sum((regCO$residuals)^2)  
    TSS <- sum((regCO$model[1] - mean(regCO$model[,1]))^2)
    
    regCO$rse <- sqrt(RSS / df2)
    regCO$r.squared <- 1 - (RSS / TSS)
    regCO$adj.r.squared <-  1 - ((RSS / df2)/(TSS / (df1 + df2)))
    
    regCO$gdl <- c(df1, df2)
    regCO$rank <- df1
    regCO$df.residual <- df2    
    
    regCO$residuals <- Y - regCO$fitted.values
  }
  regCO
}

# aplicar Cochrane-Orcutt
mod_co <- cochrane.orcutt(modelo_final_harmonica)

summary(mod_co)
resumo <- data.frame(
  Coeficiente = mod_co$coefficients,
  `Erro Padrão` = mod_co$std.error,
  `t valor` = mod_co$t.value,
  `p valor` = mod_co$p.value
)

print(round(resumo, 4))


