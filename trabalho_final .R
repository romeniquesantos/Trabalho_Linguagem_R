##################################################################
## INSTALACAO DE PACOTES
##################################################################
packages <- c("tidyverse", "factorextra", "gridExtra", "cluster", 
              "plotly", "car", "outliers", "lm", "psych")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

##################################################################
## CARREGAMENTO DE PACOTES
##################################################################
library(readr)  #LEITURA DE ARQ.CSV
library(readxl) #LEITURA DE ARQ.EXCELL
library(dplyr)  
library(cluster)
library(tidyverse)
library(ggplot2)
library(factoextra)
library(gridExtra)
library(plotly) 
library(tidyr)
library(scales)
library("ggsci")
library(psych)
library(outliers)
theme_set(theme_pubclean()) #Tema pronto para publica????o

##################################################################
## Quest??o 1 - Importar dados da planilha
##################################################################
setwd("D:/pos unicarioca/6 ?? modulo/1 - LINGUAGEM DE PROGRAMA????O APLICADA R/Trabalho final")
getwd() 
path <- read.csv("bcdata.sgs.24363.csv",  sep = ";")
path <- read.csv("https://api.bcb.gov.br/dados/serie/bcdata.sgs.24363/dados?formato=csv",sep=";")
dataset <- data.frame(path)
dataset$data <- as.Date(dataset$data, "%d/%m/%Y")
dataset$mes <-as.numeric(format(dataset$data, format = "%m"))
dataset$ano <- 0
dataset$ano <- as.numeric(format(dataset$data, format = "%Y"))
dataset$valor <- gsub(",",".",dataset$valor)
dataset$valor <- as.double(dataset$valor, ",")

##################################################################
## Pre - Processamento
##################################################################
data_ano <- dataset[dataset$ano >= 2009 & dataset$ano <= 2011,]

data_ano$ano <- data_ano$ano <- as.numeric(format(data_ano$data, format = "%Y"))
data_ano$valor <- as.numeric(data$valor, 2)

data_ano$valor

variable_names <- list(
  "January" = "Janeiro" ,
  "February" = "Fevereiro",
  "Match" = "Mar??o",
  "April" = "Abril",
  "May"  = "Maio",
  "June"  = "Junho",
  "July"  = "Julho",
  "August"  = "Agosto",
  "Septembe"  = "Setembro",
  "October"  = "Outubro",
  "November"  = "Novembro",
  "December"  = "Dezembro"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}

##################################################################
## Quest??o 2.a
##################################################################
ggplot(data_ano, aes(x=data, y=valor)) +
  geom_line() +
  theme_minimal()

ggplot(data_ano, aes(x=month, y=valor, group=year)) +
  geom_line(aes(color=year))+
  geom_point(aes(color=year))+
  theme_bw() +
  theme_minimal()+
  ylab("Valor") +
  xlab("Mes") +
  labs(color = 'Ano') + 
  ggtitle("Série temporais dos anos de 2009 a 2011")

##################################################################
## Quest??o 2.b - agrupado por anos
##################################################################
data_ano$month <- factor(month.name[data_ano$mes], levels = month.name)
data_ano$year <- as.factor(data_ano$ano)
ggplot(data_ano, aes(year, fill=month)) +
  geom_bar(aes(weight=valor), position="stack") +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  theme_minimal() +
  ylab("Valor") +
  xlab("Ano") +
  labs(fill = "Mes")




##################################################################
## Quest??o 2.b - agrupado por meses
##################################################################
ggplot(data_ano, aes(month, fill=year)) +
  geom_bar(aes(weight=valor), position="stack") +
  scale_fill_brewer(palette="Paired") +
  coord_flip() + 
  theme_bw() +
  theme_minimal() +
  ylab("Ano") +
  xlab("M??s") +
  labs(fill = "Ano")

##################################################################
ggplot(data_ano, aes(year, fill=year)) +
  geom_bar(aes(weight=valor), position="dodge") +
  facet_wrap(~month,  ncol=4, labeller=variable_labeller) +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  theme_minimal() +
  ylab("Valor") +
  xlab("Mes") +
  labs(fill = "Ano")

##################################################################
## Quest??o 2.c - agrupado por meses
##################################################################
ggplot(data_ano, aes(year, fill=month)) +
  geom_bar(aes(weight=valor), position="dodge") +
  geom_text(aes(x = year, y = valor, label = valor, group = month), 
            position =  position_dodge(width = 1), size = 2.5,
            vjust = 0.4, hjust = -0.1, angle = 0) + 
  scale_fill_brewer(palette="Paired") +
  coord_flip() + 
  theme_bw() +
  theme_minimal() +
  ylab("Valor") +
  xlab("Ano") +
  labs(fill = "Mes")

##################################################################
## Quest??o 2.c - agrupado por anos
##################################################################
ggplot(data_ano, aes(year, fill=year)) +
  geom_bar(aes(weight=valor), position="dodge") +
  facet_wrap(~month,  ncol=6, labeller=variable_labeller) +
  scale_fill_brewer(palette="Paired") +
  theme_bw() +
  theme_minimal() +
  ylab("Valor") +
  xlab("Mes") +
  labs(fill = "Ano")

##################################################################
## Quest??o 3
##################################################################
head(data_ano)

str(data_ano)

#limite inferior 
range(data_ano$mes)
range(data_ano$ano)
range(data_ano$valor)

#desvio-padr??o
sd(data_ano$valor)

#mediana
median(data_ano$valor)

#media
mean(data_ano$valor)

#variancia 
var(data_ano$valor)

#quartis
Q1 <- quantile(data_ano$valor, probs = 0.25)
Q1
Q2 <- quantile(data_ano$valor, probs = 0.50)
Q2
Q3 <- quantile(data_ano$valor, probs = 0.75)
Q3

#visualizando os outliers
ano <- data_ano[data_ano$year == 2009, ]
data_summary <- data.frame(unclass(summary(ano$valor)),  # Convert summary to data frame
                           check.names = FALSE)
a <-list(data_summary)

ano <- data_ano[data_ano$year == 2010, ]
data_summary <- data.frame(unclass(summary(ano$valor)),  # Convert summary to data frame
                           check.names = FALSE)
b <- list(data_summary)

ano <- data_ano[data_ano$year == 2011, ]
data_summary <- data.frame(unclass(summary(ano$valor)),  # Convert summary to data frame
                           check.names = FALSE)
c <- list(data_summary)
data_sumario <- data.frame(c(a, b, c))
data_sumario$descrica <- row.names(data_sumario)
row.names(data_sumario) <- 1 : 6
colnames(data_sumario) <- c('2009','2010','2011')

boxplot(valor ~ ano, data = data_ano)

describeBy(data_ano[ , c("valor")], group=data_ano$year, fast=TRUE)
  
##################################################################
## Quest??o 4.a
##################################################################
data_ano2 <- dataset[dataset$ano >= 2010 & dataset$ano <= 2011, ]

#Normalidade dos residuos
shapiro.test(mod$residuals)

#Outiers nos residuos 
rstandard(mod)
summary(rstandard(mod))

ggplot(data = data_ano2, aes(x = mes, y=valor)) +
  geom_point(colour = "black", size = 1) +
  labs(title = 'Regress??o Linear',
     y = 'Valor',
     x = 'M??s')+
  stat_smooth(method = lm)
  theme_classic()

##################################################################
## Quest??o 4.b
##################################################################
#C??lculo da Regress??o Linear
mod <- lm(valor ~ poly(mes, 2, raw = TRUE), data = data_ano2)
mod
plot(mod)  

#Visualizar dados sem a Regress??o Polinomial
ggplot(data_ano2, aes(mes, valor)) +
  labs(title = 'Exemplo de Visualiza????o dos Dados sem RP',
       y = 'Valor',
       x = 'Meses')+
  geom_point() +
  stat_smooth()

#Visualizar dados com a Regress??o Polinomial
ggplot(data_ano2, aes(mes, valor)) +
  labs(title = 'Exemplo de Visualiza????o dos Dados com a RP',
       y = 'Valor',
       x = 'Meses')+
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE))

#Visualizar coeficientes
coef(mod)

#Resumo do modelo
summary(mod)