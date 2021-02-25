library(dplyr)
library(openxlsx)

x = read.xlsx("C:\\Users\\Ulia\\Desktop\\Bioestadística\\EA\\Datos_U2EA.xlsx", sheet = 2)
x = data.frame(lapply(x, function(x) as.numeric(as.character(x))))

fac =factor(rep(c("mex","esp","som"),c(19,19,19)))
x1 = c(x$México, x$España, x$Somalia)

datos_factor = data.frame(fac = fac, x1 = x1)


library(ggplot2)

som = x$Somalia
esp = x$España
mex = x$México
tiempito = x$year
datitos = data.frame(años = tiempito, somalia = som, españa = esp, mexico = mex)

regsom = lm(som ~ tiempito)
regmex = lm(mex ~ tiempito)
regesp = lm(esp ~ tiempito)

ggplot(datitos,aes(años, somalia)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')

ggplot(datitos,aes(años, españa)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')

ggplot(datitos,aes(años, mexico)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')

## Correlación

library(Hmisc)
library(corrplot)

rcorr(as.matrix(datitos), type = "pearson")



summary(regsom)

summary(regesp)

summary(regmex)