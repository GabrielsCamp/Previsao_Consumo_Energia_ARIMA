############## Previsão Consumo de Energia na Região Sul - ARIMA ##############

# 1. Pacotes --------------------------------------------------------------

# 1.1 Carregando pacotes 
library(magrittr)        
library(lubridate)       
library(dplyr)          
library(rbcb)           
library(tsibble)         
library(tidyr)          
library(ggplot2)         
library(forecast)       
library(tsDyn)           
library(tseries)
library(urca)

# 2. Download e Leitura dos Dados -----------------------------------------

# 2.1 Importando Consumo de Energia Residencial
energia_sul_residencial = rbcb::get_series(code = 1418, 
                                           start_date = "2015-01-01", 
                                           end_date = Sys.Date()) %>%
  dplyr::select("data" = "date", 
                "energia" = "1418") %>%
  dplyr::as_tibble()

# 2.2 Importando Consumo de Energia Industrial
energia_sul_industrial = rbcb::get_series(code = 1419, 
                                          start_date = "2015-01-01", 
                                          end_date = Sys.Date()) %>%
  dplyr::select("data" = "date", 
                "energia" = "1419") %>%
  dplyr::as_tibble()

# 2.3 Transformando Dados em Séries Temporais - Residencial
energia_sul_residencial_ts = ts(energia_sul_residencial$energia, 
                                start = c(2015,1), 
                                end = c(2022,11), 
                                frequency = 12)

# 2.4 Transformando Dados em Séries Temporais - Industrial
energia_sul_industrial_ts = ts(energia_sul_industrial$energia, 
                               start = c(2015,1), 
                               end = c(2022,11), 
                               frequency = 12)

# 3. Visualização dos Dados -----------------------------------------------

# 3.1 Plotando Séries em nível
par(mfrow = c(1,1))
par(cex.main = 0.75)

# 3.1.1 Plotando Série Consumo Residencial 
plot(energia_sul_residencial, type ="l", main = "Consumo Residencial de Energia 
na Região Sul", xlab = "anos", ylab = "GWh", col = "darkblue")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

# 3.1.2 Plotando Série Consumo Industrial 
plot(energia_sul_industrial, type ="l", 
     main = "Consumo Industrial de Energia na Região Sul",
     xlab = "anos", ylab = "GWh", col = "darkgreen")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

# 3.2 Plotando das Séries em Diferença
par(mfrow = c(1,2))
par(cex.main = 0.75)

# 3.2.1 Plotando Série em diferença - Residencial 
diff(energia_sul_residencial_ts) %>%
  plot(type ="l", main = "Primeira Diferença do Consumo Residencial",
       xlab = "anos", ylab = "GWh", col = "darkblue")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)

# 3.2.2 Plotando Série em diferença - Industrial
diff(energia_sul_industrial_ts) %>%
  plot(type ="l", main = "Primeira Diferença do Consumoo Industrial",
       xlab = "anos", ylab = "GWh", col = "darkgreen")
grid(nx = NULL, ny = NULL, lty = 2, col = "gray", lwd = 0.5)


# 4. Modelagem ------------------------------------------------------------

# 4.1 Estimando Auto ARIMA
modelo_residencial = forecast::auto.arima(energia_sul_residencial_ts)
modelo_industrial = forecast::auto.arima(energia_sul_industrial_ts)

# 4.2 Diagnóstico dos Resíduos

# 4.2.1 Resíduos ARIMA Consumo Residencial
par(mfrow = c(2,2))
plot(modelo_residencial$residuals, type = "l" , ylab = "Resíduos")
qqnorm(modelo_residencial$residuals)
qqline(modelo_residencial$residuals)
acf(modelo_residencial$residuals, main = "ACF")
pacf(modelo_residencial$residuals, main = "PACF")

# 4.2.2 Resíduos ARIMA Consumo Residencial
par(mfrow = c(2,2))
plot(modelo_industrial$residuals, type = "l" , ylab = "Resíduos")
qqnorm(modelo_industrial$residuals)
qqline(modelo_industrial$residuals)
acf(modelo_industrial$residuals, main = "ACF")
pacf(modelo_industrial$residuals, main = "PACF")

# 5. Previsão -------------------------------------------------------------

# 5.1 Previsão
pred_residencial = forecast(modelo_residencial, h = 6)
pred_industrial = forecast(modelo_industrial, h = 6)

# 5.2 Plot da Previsão
par(cex.main = 0.75)
par(mfrow = c(1,2))
plot(pred_residencial, main = "previsão consumo residencial", col ="darkblue",
     xlab = "Anos" , ylab = "GWh")
plot(pred_industrial, main = "previsão consumo industrial", col = "darkgreen",
     xlab = "Anos" , ylab = "GWh")

