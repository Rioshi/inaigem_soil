library(aqp)
library(Hmisc)
library(lattice)
library(MASS)
library(plyr)

prof <- read.csv(file.choose(),header = TRUE)
names(prof)
depths(prof) <- ID ~ Top + Bottom
print(prof)

#agg <- slab(prof, fm= ~CC + PM, slab.structure=c(seq(from = 0, to = 150, by = 10)), slab.fun=mean, na.rm=TRUE)
agg <- slab(prof, fm= ID~ CC + PM + Li + Ao + Ar, slab.structure = 10)
agg <- slab(prof, fm= ID~ Nmin + P2O5 + K2O, slab.structure = 10)
head(prof)

xyplot(top ~ p.q50 | variable, data=agg, ylab='Profundidad (cm)',
       xlab='',
       lower=agg$p.q25, upper=agg$p.q75, ylim=c(42,-2),
       panel=panel.depth_function,
       alpha=0.25, sync.colors=TRUE,
       par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
       prepanel=prepanel.depth_function,
       cf=agg$contributing_fraction, cf.col='black', cf.interval=5, 
       layout=c(5,1), strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free'))
)


####Agrupar por Parcelas###
library(dplyr)
Parc <-
  prof %>% 
  group_by(NP)

Parc <- filter(prof,NP =='P-01')
depths(Parc) <- ID~ Top + Bottom
agg <- slab(Parc, fm= ~ Comp)
names(Parc)

xyplot(top ~ p.q50 | variable, data=agg, ylab='Profundidad (cm)',
       xlab='Compactación (kPa)',lower=agg$p.q25, upper=agg$p.q75, ylim=c(50,-2),
       panel=panel.depth_function,
       alpha=0.25, sync.colors=TRUE,
       par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
       prepanel=prepanel.depth_function,
       cf.interval=5,
       strip=strip.custom(bg=grey(0.8)),
       scales=list(x=list(tick.number=4, alternating=3, relation='free'))
       
)
Nombre_Parcelas <-levels(prof$NP)
Parc <- filter(prof,NP == Nombre_Parcelas[4])


######################################################
######################################################
#####################################################
library(aqp)
library(Hmisc)
library(lattice)
library(MASS)
library(plyr)
library(dplyr)
compactacion <-read.csv(file.choose(),header = TRUE)
compactacion$NP <-as.factor(compactacion$NP)
Nombre_Parcelas <-levels(compactacion$NP)
Parcelas <- list()
agg<- list()
gf <- list()
for(i in 1:length(Nombre_Parcelas)){
  Parcelas[[i]] <- filter(compactacion,NP ==Nombre_Parcelas[i])
  depths(Parcelas[[i]]) <- ID~ Top + Bottom
  agg[[i]] <- slab(Parcelas[[i]], fm= ~ Comp)
  gf[[i]] <- xyplot(top ~ p.q50 | variable, data=agg[[i]], ylab='Profundidad (cm)',
                    xlab=Nombre_Parcelas[i],lower=agg[[i]]$p.q25, 
                    upper=agg[[i]]$p.q75, ylim=c(50,-2),
                    panel=panel.depth_function,
                    alpha=0.25, sync.colors=TRUE,
                    par.settings=list(superpose.line=list(col='RoyalBlue', lwd=2)),
                    prepanel=prepanel.depth_function,
                    cf.interval=5,
                    strip=strip.custom(bg=grey(0.8)),
                    scales=list(x=list(tick.number=4, alternating=3, relation='free')))
  
}
gf[[3]]




#####################Soil resistance comparission #######
library(agricolae)
library(dplyr)
library(ggplot2)
library(googlesheets)
library(cowplot)

## LEctura de datos
gss <- gs_ls()
fb <- gs_url("https://docs.google.com/spreadsheets/d/1EzOwlQicp8o2yilxAw4_ikOd_TqPPkvQiihE_5FlKJM/edit#gid=1155450828") 
gs_title("Research_INAIGEM")
sc <- fb %>%
  gs_read(ws = "Compactacion")

rpc <- filter(sc,Top==10)
rpc$Comp[which(rpc$Comp == 0)]=NA
model1 <- lm(log(rpc$Comp)~rpc$NP,na.action = na.omit)
summary(model1)
shapiro.test(model1$residuals)
anova(model1)
kruskal.test(Comp~NP,data=rpc) 


HSD_tuk<-HSD.test(aov(Comp~NP,data=rpc), "NP",console=TRUE)
bar.group(HSD_tuk$groups,ylab="Resistencia a la penetración (kPa)",main="Comparación entre parcelas",ylim=c(0,2300))
