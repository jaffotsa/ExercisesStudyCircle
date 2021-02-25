library(openxlsx)
library(xts)
library(dynlm)
library(mFilter)


ex = read.xlsx(file.choose(), sheet = 1)

MexicoItsmo = data.frame(Petroleo = ex$Mexico.Itsmo)

MexicoItsmo

Serie = ts(MexicoItsmo$Petroleo, start = c(1983,1),
           end = c(2019,12), frequency = 12)

class(Serie)
Serie

# Boxplot para estudiar la estacionalidad

boxplot(Serie ~ cycle(Serie))


# Autocorrelacion y la autocorrelacion parcial

acf(Serie)

pacf(Serie)

# Graficar

plot(Serie)

# Modelo Aditivo

modeloaditivo = decompose(Serie)
plot(modeloaditivo)

# Modelo mULTIPLICATIVO

modeloaditivo = decompose(Serie, type = "multiplicative" )
plot(modeloaditivo)

# Estimar la tendencia

Tendencia = modeloaditivo$trend
plot(Tendencia)

# Estimar el término estocastico

Estoc = modeloaditivo$random
plot(Estoc)

# Estimar el término Estacional

Estac = modeloaditivo$seasonal
plot(Estac)

# Estimar el valor observado

Observ = modeloaditivo$x
plot(Observ)

# Juntar Estacionalidad y tendencia

ts.plot(cbind(Tendencia, Tendencia+Estac),
        lty = 1:2)

# Estimemos un modelo ARIMA para proyectar
# el precio del petroleo

library(forecast)

arimaPET = auto.arima(Serie, stepwise = F,
                      approximation = F,
                      trace = T)

proy = forecast(arimaPET,d=1,D=1,h=12, level = 95)

checkresiduals(arimaPET)

plot(proy, xlim = c(2010,2021))
autoplot(proy)

print(proy)