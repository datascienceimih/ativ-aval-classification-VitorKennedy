install.packages("dplyr")
install.packages("readr")
install.packages("ggplot2")
install.packages("ISLR")
library(dplyr)
library(readr)
library(ggplot2)
library(ISLR)
library(MASS)

### 10)
## A)
summary(Weekly)
cor(Weekly[, -9])
attach(Weekly)
plot(Volume)

# Apenas a relação entre Ano e Volume é significativa.
# Volume está aumentando com o tempo.

## B)
fit.glm = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(fit.glm)

# Lag2 é a única variável significativa devido o seu valor p.

## C)
probs = predict(fit.glm, type = "response")
pred.glm = rep("Down", length(probs))
pred.glm[probs > 0.5] <- "Up"
table(pred.glm, Direction)

# É possível concluir que a % de predições corretas nos dados de treinamento é igual a 56,10.
# A taxa de erro de treinamento é de 43,89%

## D)
train = (Year < 2009)
Weekly.20092010 = Weekly[!train, ]
Direction.20092010 = Direction[!train]
fit.glm2 = glm(Direction ~ Lag2, data = Weekly, family = binomial, subset = train)
summary(fit.glm2)

probs2 = predict(fit.glm2, Weekly.20092010, type = "response")
pred.glm2 = rep("Down", length(probs2))
pred.glm2[probs2 > 0.5] <- "Up"
table(pred.glm2, Direction.20092010)

# 62,5% das previsões foram corretas nos dados de teste.
# A taxa de erro foi de 37,5%.

## E)
fit.lda = lda(Direction ~ Lag2, data = Weekly, subset = train)
fit.lda

pred.lda = predict(fit.lda, Weekly.20092010)
table(pred.lda$class, Direction.20092010)

# Os resultados foram bem parecidos com o do modelo de reg. logística.

## F)
fit.qda = qda(Direction ~ Lag2, data = Weekly, subset = train)
fit.qda

pred.qda = predict(fit.qda, Weekly.20092010)
table(pred.qda$class, Direction.20092010)

# Com QDA, 58,65% das previsões foram corretas nos dados de teste.
#  taxa de erro foi de 41,34%.

## H)
# Reg. logística e LDA.

### 11)
## A)
attach(Auto)
mpg01 = rep(0, length(mpg))
mpg01[mpg > median(mpg)] <- 1
Auto = data.frame(Auto, mpg01)

## B)
cor(Auto[, -9])
pairs(Auto)
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs Mpg01")
boxplot(displacement ~ mpg01, data = Auto, main = "Displacement vs Mpg01")
boxplot(horsepower ~ mpg01, data = Auto, main = "Horsepower vs Mpg01")
boxplot(weight ~ mpg01, data = Auto, main = "Weight vs Mpg01")
boxplot(acceleration ~ mpg01, data = Auto, main = "Acceleration vs Mpg01")
boxplot(year ~ mpg01, data = Auto, main = "Year vs Mpg01")

# As variáveis cylinders, weight, displacement e horsepower 
# possuem uma associação com mpg01.

## C)
train = (year %% 2 == 0)
Auto.train = Auto[train, ]
Auto.test = Auto[!train, ]
mpg01.test = mpg01[!train]

## D)
fit.lda = lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda

pred.lda = predict(fit.lda, Auto.test)
table(pred.lda$class, mpg01.test)

mean(pred.lda$class != mpg01.test)

# A taxa de erro de teste é de 12,63%

## E)
fit.qda = qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda

pred.qda = predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)

mean(pred.qda$class != mpg01.test)

# A taxa de erro de teste é de 13,18%

## F)
fit.glm = glm(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, family = binomial, subset = train)
summary(fit.glm)

probs = predict(fit.glm, Auto.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, mpg01.test)

mean(pred.glm != mpg01.test)

# A taxa de erro de teste é de 12,08%

### 13)
attach(Boston)
crim01 = rep(0, length(crim))
crim01[crim > median(crim)] <- 1
Boston = data.frame(Boston, crim01)

train = 1:(length(crim) / 2)
test = (length(crim) / 2 + 1):length(crim)
Boston.train = Boston[train, ]
Boston.test = Boston[test, ]
crim01.test = crim01[test]

fit.glm = glm(crim01 ~ . - crim01 - crim, data = Boston, family = binomial, subset = train)

probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)

# Nesta reg. logística, a taxa de erro de teste é de 18,18%

fit.glm = glm(crim01 ~ . - crim01 - crim - chas - nox, data = Boston, family = binomial, subset = train)

probs = predict(fit.glm, Boston.test, type = "response")
pred.glm = rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, crim01.test)

mean(pred.glm != crim01.test)

# Nesta reg. logística, a taxa de erro de teste é de 15,81%

fit.lda = lda(crim01 ~ . - crim01 - crim, data = Boston, subset = train)
pred.lda = predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)

mean(pred.lda$class != crim01.test)

# Neste LDA, a taxa de erro de teste é de 13,43%

fit.lda = lda(crim01 ~ . - crim01 - crim - chas - nox, data = Boston, subset = train)
pred.lda = predict(fit.lda, Boston.test)
table(pred.lda$class, crim01.test)

mean(pred.lda$class != crim01.test)

# Neste LDA, a taxa de erro de teste é de 15,02%