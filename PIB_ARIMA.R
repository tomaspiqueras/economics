#código para predicción en economia -PIB bajo modelo ARIMA
# importamos y especificamos los datos como serie temporal
library(readxl)
library(ggplot2)
library(lubridate)
library(tseries)
library(forecast)
library(writexl)
library(dplyr)
df <- read_excel("Documents/Prediccion economica/df.xlsx")

df$periodo <- yq(paste0(substr(df$periodo, 1, 4), "Q", substr(df$periodo, 6, 6)))
df$periodo <- as.Date(df$periodo)




ggplot(df, aes(x = periodo, y = pib)) +
  geom_line() +
  labs(x = "Año", y = "PIB", title = "Evolución PIB") +
  theme_minimal()

#realizamos el test de dickey-fuller

dickey<-  adf.test(df$pib)
print(dickey)
seriediferencias <- diff(df$pib, differences = 2)
print(seriediferencias)
adf.test(seriediferencias, alternative = "stationary")
help(pacf)

autocorre<- acf(seriediferencias, plot = TRUE)
partial_autocor <- pacf(seriediferencias, plot = TRUE)
modelo <- arima(df$pib,order=c(2,2,20))

summary(modelo)
Box.test(residuals(modelo),type="Ljung-Box")
error <- residuals(modelo)
plot(error)
help(residuals)
prediccion <- forecast::forecast(modelo,h=8)
plot(prediccion)


