setwd("C:/Users/Asus/Desktop/Hotov�")
anov<-read.delim2("zbierky.txt", header=TRUE, sep="\t")
attach(anov)
library(stringr)

pokus<-as.data.frame(str_split_fixed(anov$ident, "_", 3))
names(pokus)<-c("sample", "ident1", "measur")
dataP<-cbind(anov, pokus)
dataA<-data.frame(dataP[dataP$measur!="b",])

str(dataA)
summary(dataA)

library(nortest)
library(car)


## VYSKA SPODINY LEBECNEJ vs. OBDOBIE
# H0: obdobie (faktor) nema vplyv na vysku spodiny lebecnej
# H1: obdobie ma vplyv na vysku spodiny lebecnej
# testovanie strednej hodnoty VSL medzi roznymi obdobiami

dataA$period<-factor(dataA$period)
data<-dataA[dataA$period %in% c("1","2","3","4"), c("period", "vbl1")]
summary(data)

m1<-mean(data[data$period=="1", "vbl1"])  
m1
m2<-mean(data[data$period=="2", "vbl1"])
m2
m3<-mean(data[data$period=="3", "vbl1"])
m3
m4<-mean(data[data$period=="4", "vbl1"])
m4
m<-mean(data[,"vbl1"])
m
sd1<-sd(data[data$period=="1", "vbl1"])  
sd1
sd2<-sd(data[data$period=="2", "vbl1"])
sd2
sd3<-sd(data[data$period=="3", "vbl1"])
sd3
sd4<-sd(data[data$period=="4", "vbl1"])
sd4
sd<-sd(data[,"vbl1"])

boxplot(vbl1~period, data=data, var.width=T, noch=T, xlab="Obdobie",
        ylab="V��ka spodiny lebe�nej")
points(1:4, c(m1,m2,m3,m4), col="red", pch=16)

qqPlot(data[data$period=="1","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "9.-10. storo�ie", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data[data$period=="1","vbl1"])
lillie.test(data[data$period=="1","vbl1"])
# normalita potvrdena

qqPlot(data[data$period=="2","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "11.storo�ie", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data[data$period=="2","vbl1"])
lillie.test(data[data$period=="2","vbl1"])
# normalita potvrdena

qqPlot(data[data$period=="3","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "13.-16.storo�ie", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data[data$period=="3","vbl1"])
# normalita potvrdena
lillie.test(data[data$period=="3","vbl1"])
# toto zamieta, ale berieme shapira

qqPlot(data[data$period=="4","vbl1"], xlab = "teoretick� kvantil", ylab = "v�berov� kvantil",
       main = "18.-19.storo�ie", col = "cadetblue4")
mtext(bquote(paste("V��ka spodiny lebe�nej")), line = 0.3, cex = 0.8)
shapiro.test(data[data$period=="4","vbl1"])
lillie.test(data[data$period=="4","vbl1"])
# normalita potvrdena

# overenie homogenity dat - hladina vyznamnosti 0.05
bartlett.test(vbl1~period, data=data)
leveneTest(vbl1~period, data=data)
# hypotezu o homogenite rozptylov dat nezamietame - poziadavky na anovu splnene

aov<-aov(vbl1~period, data=data)
anova(aov)
# testovacia statistika FA=4.9404, p-hodnota=0.002801
# zamietame hypotezu, ze obdobie nema vplyv na vysku spodiny lebecnej
# to znamena, ze mozny vplyv nutricneho stresu + zivotne podmienky? :)
