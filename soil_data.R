###Librerias###
library(dplyr)
library(ggplot2)
library(googlesheets)


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
