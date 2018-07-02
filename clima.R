library(agricolae)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(cowplot)

## LEctura de datos
gss <- gs_ls()
fb <- gs_url("https://docs.google.com/spreadsheets/d/1EzOwlQicp8o2yilxAw4_ikOd_TqPPkvQiihE_5FlKJM/edit#gid=1155450828") 
gs_title("Research_INAIGEM")
ws <- fb %>%
  gs_read(ws = "Clim_auto")


ws$Date <- as.Date(ws$Date, format="%d/%m/%Y %H:%M")

ggplot(ws, aes(Date,T_mean)) +
  xlab("Fecha") + ylab("Temperatura (°C)") +
  geom_line(linetype="longdash",col="blue") +scale_x_date(date_labels = "%d-%b",date_breaks = "30 days")


ggplot(ws, aes(Date,HR)) +
  xlab("Fecha") + ylab("Humedad Relativa (%") +
  geom_line(linetype="longdash",col="green") +scale_x_date(date_labels = "%d-%b",date_breaks = "30 days")


ws2 <- fb %>%
  gs_read(ws = "Cima_2017")
ws2$Date <- paste(ws2$Date,ws2$Time)
ws2 <- ws2[,-2]
ws2$Date <- as.Date(ws2$Date, format="%d/%m/%Y %H:%M:%S")

ggplot(ws2, aes(Date,ppt)) +
  xlab("Fecha") + ylab("Precipitación (mm)") +
  geom_line(linetype="longdash",col="blue") +scale_x_date(date_labels = "%d-%b",date_breaks = "15 days")

ggplot(ws2, aes(Date,T_mean)) +
  xlab("Fecha") + ylab("Temperatura (°C)") +
  geom_line(linetype="longdash",col="red") +scale_x_date(date_labels = "%d-%b",date_breaks = "15 days")
