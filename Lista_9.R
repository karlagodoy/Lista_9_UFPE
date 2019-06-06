# Lista 9
 
# ALuna: Karla Godoy


dir("C:\\Users\\karla\\Desktop\\Mestrado\\Análise de Dados")

setwd("C:\\Users\\karla\\Desktop\\Mestrado\\Análise de Dados")

# Pacotes

library(tidyverse)
library(GGally)
library(foreign)
library(texreg)
library(olsrr)


# Questao 4 


#### 4.a Utilizando a base de Dados "Wordrecall" ####
#### abrindo a base de dados: ####

wordrecall <- read.table("wordrecall.txt", header=T)

## conhecendo a base de dados:

head(wordrecall)

summary(wordrecall)

# observando a distribui??o das vari?veis: 

ggplot(wordrecall, aes(time))+geom_density()
ggplot(wordrecall, aes(prop))+geom_density()

# correla??o e valor de t:

cor.test(wordrecall$time, wordrecall$prop)

# Notamos uma valor de correla??o negativo entre as vari?veis, por?m esse valor ? estatisticamente significante, 

# gr?fico de dispers?o: 
ggplot(wordrecall, aes(time, prop))+geom_point()

#### gerando o modelo de regress?o: ####

modelo_41 <- lm(prop ~ time, data = wordrecall)

summary(modelo_41)

screenreg(modelo_41)

confint(modelo_41)

#### gerando o modelo com a transforma??o log ####

logtime <- log(wordrecall$time)

ggplot(wordrecall, aes(logtime, prop))+geom_point()

cor.test(wordrecall$prop, logtime)

modelo_41_log <- lm(prop ~ logtime, data = wordrecall)

summary(modelo_41_log)

confint(modelo_41_log)

screenreg(list(modelo_41, modelo_41_log))

## observando o distribui??o de normalidade, erros e res?duos: 

ols_plot_resid_qq(modelo_41)
ols_plot_resid_fit(modelo_41)

# o modelo alterado:

ols_plot_resid_qq(modelo_41_log)
ols_plot_resid_fit(modelo_41_log)

ggplot(wordrecall, aes(logtime, prop))+geom_point()+geom_smooth(method = "lm")
ggplot(wordrecall, aes(time, prop))+geom_point()+geom_smooth(method = "lm")







#### 4.b Utilizando a base de Dados "Shortleaf" ####
#### abrindo a base de dados:

getwd()

dir()

shortleaf <- read.table("shortleaf.txt", header=T)

dim(shortleaf)

head(shortleaf)

summary(shortleaf)

sd(shortleaf$Diam)
sd(shortleaf$Vol)

ggplot(shortleaf, aes(Diam))+geom_density()
ggplot(shortleaf, aes(Vol))+geom_density()

cor.test(shortleaf$Diam,shortleaf$Vol)

ggplot(shortleaf, aes(Diam, Vol))+geom_point()

#### modelo 42 ####

modelo_42 <- lm(Vol ~ Diam, data = shortleaf)

summary(modelo_42)

confint(modelo_42)

ggplot(shortleaf, aes(Diam, Vol))+geom_point()+geom_smooth(method = "lm")

## a an?lise dos componentes estoc?tico:

ols_plot_resid_qq(modelo_42)

ols_plot_resid_fit(modelo_42)

#### modelo42 transformando o X em log ####

logDiam <- log(shortleaf$Diam)

modelo_42logX <- lm(Vol ~ logDiam, data = shortleaf)

summary(modelo_42logX)

confint(modelo_42logX)

## gr?fico da regress?o:

ggplot(shortleaf, aes(Vol, logDiam))+geom_point()+geom_smooth(method = "lm")

## an?lise gr?fica dos componentes estoc?ticos:

ols_plot_resid_qq(modelo_42logX)

ols_plot_resid_fit(modelo_42logX)


### Transforma??o log-log

logVol <- log(shortleaf$Vol)

ggplot(shortleaf, aes(logVol))+geom_density()
ggplot(shortleaf, aes(logDiam))+geom_density()

cor.test(logVol, logDiam)

ggplot(shortleaf, aes(logVol, logDiam))+geom_point()

modelo_42logXY <- lm(logVol ~ logDiam, data = shortleaf)

summary(modelo_42logXY)

# an?lise gr?fica da regress?o: 

ggplot(shortleaf, aes(logVol, logDiam))+geom_point()+geom_smooth(method = "lm")

# an?lise dos res?duos: 

ols_plot_resid_qq(modelo_42logXY)

ols_plot_resid_fit(modelo_42logXY)

# Compara??o dos modelos:

screenreg(list(modelo_42, modelo_42logX, modelo_42logXY))

#### 4.c Utilizando a base de dados "Mamagest" ####
#### abrindo base de dados:

getwd()

dir("C:/Users/Bhr/Documents/Lista9")

setwd("C:/Users/Bhr/Documents/Lista9")

getwd()

mammgest <- read.table("mammgest.txt", header=T)

## conhecendo a base:

dim(mammgest)

head(mammgest)

summary(mammgest)

sd(mammgest$Birthwgt)
sd(mammgest$Gestation)

ggplot(mammgest, aes(Birthwgt))+geom_density()
ggplot(mammgest, aes(Gestation))+geom_density()


## correla??o

library(RColorBrewer)

cor.test(mammgest$Birthwgt, mammgest$Gestation)

library(ggthemes)

ggplot(mammgest, aes(Birthwgt,Gestation, color = Gestation))+geom_point()

#### modelo 43 #### 

modelo_43 <- lm(Gestation ~ Birthwgt, data = mammgest)

summary(modelo_43)

confint(modelo_43)

## an?lise gr?fica do modelo

ggplot(mammgest, aes(Birthwgt,Gestation, color = Gestation))+geom_point()+geom_smooth(method = "lm")

## an?lise de res?duos:

ols_plot_resid_qq(modelo_43)

ols_plot_resid_fit(modelo_43)

#### transforma??o do log em Y ####

logGestation <- log(mammgest$Gestation)

# conhecendo a nova vari?vel:

summary(logGestation)

ggplot(mammgest, aes(logGestation))+geom_density()

# correla??o

cor.test(mammgest$Birthwgt, logGestation)

ggplot(mammgest, aes(Birthwgt, logGestation, color = logGestation))+geom_point()

# modelo 43_logY

modelo_43logY <- lm(logGestation ~ Birthwgt, data = mammgest)

summary(modelo_43logY)

# an?lise gr?fica:

ggplot(mammgest, aes(Birthwgt, logGestation, color = logGestation))+geom_point()+geom_smooth(method = "lm")

# an?lise de res?duos:

ols_plot_resid_qq(modelo_43logY)

ols_plot_resid_fit(modelo_43logY)

screenreg(list(modelo_43, modelo_43logY))

#### 4.2 Regress?o Polinomial ####

dir()

bluegills <- read.table("bluegills.txt", header = T)

# conhecendo o banco: 

dim(bluegills)

names(bluegills)

summary(bluegills)

sd(bluegills$age)
sd(bluegills$length)


# correla??o: 

ggplot(bluegills, aes(age, length, color = age))+geom_point()
cor.test(bluegills$age, bluegills$length)

# modelo bivariado: 

modelo_44 <- lm(length ~ age, data = bluegills)

summary(modelo_44)

confint(modelo_44)

ggplot(bluegills, aes(age, length, color = age))+geom_point()+geom_smooth(method = "lm")


ols_plot_resid_qq(modelo_44)

ols_plot_resid_fit(modelo_44)

# transforma??o polinomial:

agepol <- bluegills$age^2

summary(agepol)

sd(agepol)

ggplot(bluegills, aes(agepol))+geom_density()

# CORRELA??O

ggplot(bluegills, aes(agepol, length))+geom_point()

#### novo modelo

modelo_44pol <- lm(length ~ age + agepol, data = bluegills)

summary(modelo_44pol)

ols_plot_resid_qq(modelo_44pol)

ols_plot_resid_fit(modelo_44pol)

screenreg(list(modelo_44, modelo_44pol))



