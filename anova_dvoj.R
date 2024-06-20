setwd("C:/Users/Asus/Desktop/Vlo�i�")
anov<-read.delim2("rozmery.txt", header=TRUE, sep="\t")
attach(anov)
library(stringr)

pokus<-as.data.frame(str_split_fixed(anov$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(anov, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])
dataA$age.category<-factor(dataA$age.category)
dataA$period<-factor(dataA$period)

str(dataA)
summary(dataA)

library(nortest)
library(car)


## VYSKA SPODINY LEBECNEJ vs. VEK, POHLAVIE, OBDOBIE
# H0: stredna hodnota vysky spodiny lebecnej sa nelisi medzi testovanymi skupinami
# H1: stredna hodnota vysky spodiny lebecnej sa lisi medzi testovanymi skupinami

vbl1<-dataA$vbl1
sex<-dataA$sex
age.category<-dataA$age.category
period<-dataA$period
data<-data.frame(vbl1, sex, age.category, period)
data<-na.omit(data)

sum(data=="M")
sum(data=="F")
sum(data=="I")

# viacfaktorialna ANOVA s interakciami
res<-aov(vbl1~sex*age.category*period, data=data)
summary(res)
# doplnit zavery, normalita overena z inych testov

levels(data$sex)
levels(data$age.category)

# install.packages("ggpubr")
library("ggpubr")
ggplot(data, aes(x=age.category, y=vbl1, fill=sex)) + 
  geom_boxplot()+
  facet_grid(~period)+
  scale_fill_manual(values=c("#e74c3c", "#f1c40f", "#2e86c1"))+
  xlab("Vekov� kateg�ria")+ylab("V��ka spodiny lebe�nej")


## INDEX LEBKY (VSL/VL) vs. VEK, POHLAVIE, OBDOBIE
# H0: stredna hodnota indexu VSL/VL sa nelisi medzi testovanymi skupinami
# H1: stredna hodnota indexu VSL/VL sa lisi medzi testovanymi skupinami

anov<-read.delim2("rozmery.txt", header=TRUE, sep="\t")
attach(anov)
library(stringr)

pokus<-as.data.frame(str_split_fixed(anov$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(anov, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])
dataA$age.category<-factor(dataA$age.category)
dataA$period<-factor(dataA$period)

str(dataA)
summary(dataA)

library(nortest)
library(car)

dataA$index.L<-dataA[,22]/dataA[,28]

index.L<-dataA$index.L
sex<-dataA$sex
age.category<-dataA$age.category
period<-dataA$period

data<-data.frame(index.L, sex, age.category, period)
data<-na.omit(data)

sum(data=="M")
sum(data=="F")
sum(data=="I")

# viacfaktorialna ANOVA s interakciami
res<-aov(index.L~sex*age.category*period, data=data)
summary(res)
# doplnit zavery, normalita overena z inych testov

levels(data$sex)
levels(data$age.category)

# install.packages("ggpubr")
library("ggpubr")
ggplot(data, aes(x=age.category, y=index.L, fill=sex)) + 
  geom_boxplot()+
  facet_grid(~period)+
  scale_fill_manual(values=c("#e74c3c", "#f1c40f", "#2e86c1"))+
  xlab("Vekov� kateg�ria")+ylab("Index VSL / VL")


## VYSKA LEBKY vs. VEK, POHLAVIE, OBDOBIE
# H0: stredna hodnota vysky lebky sa nelisi medzi testovanymi skupinami
# H1: stredna hodnota vysky lebky sa lisi medzi testovanymi skupinami

skull.H<-dataA$skull.H
sex<-dataA$sex
age.category<-dataA$age.category
period<-dataA$period
data<-data.frame(skull.H, sex, age.category, period)
data<-na.omit(data)

sum(data=="M")
sum(data=="F")
sum(data=="I")

# viacfaktorialna ANOVA s interakciami
res<-aov(skull.H~sex*age.category*period, data=data)
summary(res)
# doplnit zavery, normalita overena z inych testov

levels(data$sex)
levels(data$age.category)

# install.packages("ggpubr")
library("ggpubr")
ggplot(data, aes(x=age.category, y=skull.H, fill=sex)) + 
  geom_boxplot()+
  facet_grid(~period)+
  scale_fill_manual(values=c("#e74c3c", "#f1c40f", "#2e86c1"))+
  xlab("Vekov� kateg�ria")+ylab("V��ka lebky")
