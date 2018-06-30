###Librerias###
library(dplyr)
library(ggplot2)
library(googlesheets)
library(cowplot)

## LEctura de datos
gss <- gs_ls()
fb <- gs_url("https://docs.google.com/spreadsheets/d/1EzOwlQicp8o2yilxAw4_ikOd_TqPPkvQiihE_5FlKJM/edit#gid=1155450828") 
gs_title("Research_INAIGEM")
so <- fb %>%
  gs_read(ws = "Suelos")

###Datos totales#####
summary(so)
is.na(so)

#### ANOVA Rotacion cultivo
papa <- filter(so,SC=="Papa")

formu <- colnames(papa[,7:25]) 
formu <- paste(formu,"~","NM",sep = "")
formu <- formu[-c(3,10,17,18)]
for(i in 1:15){
  print(anova(lm(as.formula(formu[i]),data=papa)))
}



##Resumen por cultivos - año
summary(filter(so,SC=="Papa",Year==2016))

#Prueba t
formu <- colnames(papa[,7:25]) 
formu <- paste(formu,"~","NM",sep = "")
formu <- formu[-c(3,10,17,18)]

for(i in 1:15){
  print(t.test(as.formula(formu[i]),data=papa,conf.level=0.95,alternative="two.sided"))
}

#### Graficos comparativos
papa <-filter(so,SC=="Papa")
haba <-filter(so,SC=="Haba")
pasto <-filter(so,SC=="Pasto")
quinua <-filter(so,SC=="Quinua")

bar <- ggplot(data= papa, aes(x=as.factor(Year),y=pH)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  labs( x = "Año", y = "pH")



bar1 <- ggplot(data= so, aes(x=as.factor(Year),y=pH,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  labs(x = "Año", y = "pH") 
bar2 <- ggplot(data= so, aes(x=as.factor(Year),y=Al_H,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  xlab("Año") + ylab(expression(paste('Acidéz Cambiable (cmolc.kg'^"-1"*')')))

plot_grid(bar1, bar2, labels = "AUTO")

#
bar3 <- ggplot(data= so, aes(x=as.factor(Year),y=MO,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  labs(x = "Año", y = "Materia orgánica (%)") 

bar4 <- ggplot(data= so, aes(x=as.factor(Year),y=Cic,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  xlab("Año") + ylab(expression(paste('CIC (cmolc.kg'^"-1"*')')))
plot_grid(bar3, bar4, labels = "AUTO")

#
bar5 <- ggplot(data= so, aes(x=as.factor(Year),y=P,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  xlab("Año") + ylab(expression(paste('Fósforo disponible (mg.kg'^"-1"*')')))

bar6 <- ggplot(data= so, aes(x=as.factor(Year),y=K,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  xlab("Año") + ylab(expression(paste('Potasio disponible (mg.kg'^"-1"*')')))
plot_grid(bar5, bar6, labels = "AUTO")

bar7 <- ggplot(data= so, aes(x=as.factor(Year),y=CE,fill=SC)) +
  geom_bar(stat = "summary", fun.y = "mean",position=position_dodge()) +
  xlab("Año") + ylab(expression(paste('CE (mg.kg'^"-1"*')')))
bar7
